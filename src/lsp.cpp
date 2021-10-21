#include "lsp.h"

#include "DataNode.h"
#include "definitions.h"
#include "fmt/core.h"
#include "log.h"
#include "utils.h"

#include <algorithm>
#include <charconv>
#include <iostream>
#include <iterator>
#include <optional>
#include <string>
#include <string_view>

using namespace std::literals;

namespace {

// Parses a workspace json array.
std::list<lsp::Workspace> ParseWorkspace(json::array Value) {
  std::list<lsp::Workspace> workspaces;
  for (auto Element : Value) {
    workspaces.emplace_back();
    workspaces.back().Name = Element["name"].get_string().value();
    workspaces.back().Path = lsp::UriToFsPath(Element["uri"]);
  }
  return workspaces;
}

// Parses a location.
lsp::Location ParseLocation(json::object Value) {
  lsp::Location Location;
  Location.Line = Value["line"].get_uint64();
  Location.Column = Value["character"].get_uint64();
  return Location;
}

std::string RangeToJson(const lsp::Location &Start, const lsp::Location &End) {
  return fmt::format(R"(
{{
    "start": {{ "line": {}, "character": {} }},
    "end": {{ "line": {}, "character": {} }}
}})",
                     Start.Line, Start.Column, End.Line, End.Column);
}

std::string IdToString(json::value Id) {
  if (Id.type() == json::json_type::string)
    return fmt::format("\"{}\"", Id.get_string().value());
  else if (Id.type() == json::json_type::number)
    return fmt::format("{}", Id.get_int64().value());
  return "null";
}

constexpr int ParseError = -32700;
constexpr int InvalidRequest = -32600;
constexpr int MethodNotFound = -32601;
} // namespace

void lsp::Server::HandleNotification(std::string Message) try {
  Log(">> Client sent:\n{}'\n", Message);

  Message.reserve(Message.size() + simdjson::SIMDJSON_PADDING);
  json::parser Parser;
  json::document Doc = Parser.iterate(Message);

  // We expect a valid JSON-RPC object.
  if (Doc["jsonrpc"] != "2.0")
    return SendError(InvalidRequest, "Unknown JSON RPC version", Doc.find_field("id"));

  json::value Id = Doc.find_field("id");
  // Requests without an "id" are in fact a notification.

  std::string_view Method = Doc["method"];
  json::object Params = Doc["params"];
  // "params" is optional.

  Log(">> Received method '{}'.\n", Method);

  // Now that we have parsed the JSON RPC object, handle the given request.
  if (Method == "initialize")
    return Initialize(Id, Params);
  if (Method == "initialized")
    // All good! Nothing to do here.
    return;

  // The "initialize" request needs to be called before all other requests.
  if (!Initialized)
    return SendError(-32002, "Did not call 'initialize'", Id);
  if (Method == "shutdown") {
    Shutdown = true;
    Log(">> 'shutdown' done!\n");
    // Workaround for buggy clients (like the VScode one)
    // that don't send an 'exit' notification sometimes.
    State = ServerState::ExitSuccess;
    return SendResult(Id, "null");
  }
  if (Method == "exit") {
    State = Shutdown ? ServerState::ExitSuccess : ServerState::ExitError;
    Log(">> 'exit' done!\n");
    return;
  }
  if (Shutdown)
    return SendError(InvalidRequest, "Server is shutting down", Id);

  // TODO: What are workspace symbols?
  // TODO: Implement file watch operations.
  if (Method == "workspace/didChangeWorkspaceFolders")
    return DidChangeWorkspaceFolders(Id, Params);
  if (Method == "workspace/didChangeConfiguration")
    // We don't have any configurations yet.
    return;
  if (Method == "textDocument/didOpen")
    return DidOpen(Id, Params);
  if (Method == "textDocument/didChange")
    return DidChange(Id, Params);
  if (Method == "textDocument/didClose")
    return DidClose(Id, Params);
  if (Method == "textDocument/completion")
    return Completion(Id, Params);
  if (Method == "textDocument/hover")
    return Hover(Id, Params);
  // TODO: 'textDocument/signatureHelp'
  if (Method == "textDocument/declaration" ||
      Method == "textDocument/definition" ||
      Method == "textDocument/implementation")
    return Goto(Id, Params);
  // TODO: 'textDocument/references'
  // TODO: 'textDocument/documentHighlight'
  // TODO: 'textDocument/documentSymbol'
  // TODO: 'textDocument/documentColor'
  // TODO: 'textDocument/colorPresentation'
  // TODO: 'textDocument/rename'
  // TODO: 'textDocument/foldingRange'
  if (Method == "textDocument/semanticTokens/full")
    return SemanticTokensFull(Id, Params);
  if (Method == "textDocument/semanticTokens/full/delta")
    return SemanticTokensRange(Id, Params);
  if (Method == "textDocument/semanticTokens/range")
    return SemanticTokensRange(Id, Params);
  // TODO: 'textDocument/linkedEditingRange'

  // Unknown method.
  LogError(">> Unknown method '{}'.\n", Method);
  return SendError(MethodNotFound, "Method not found", Id);
}
catch (const simdjson::simdjson_error &Error)
{
  SendError(ParseError, Error.what(), {});
}

void lsp::Server::LoadFromDirectory(std::string_view Path) {
  auto DataFiles = FindESData(Path);

  for (const auto &Path : DataFiles) {
    if (Files.find(Path) != Files.end())
      // The file already exists. Either the file was opened explicitly by the
      // client, or it was opened as part of a workspace or the resource
      // directory.
      // We don't really need to do anything.
      continue;

    auto &File = Files[Path];
    File.Path = Path;
    File.Content = FileToLines(Path);
    File.CachedNodes = LoadFromLines(Path, File.Content);
  }
}

auto lsp::Server::GetState() const -> ServerState { return State; }

void lsp::Server::SendError(int Error, std::string_view Message,
                            json::value Id) {
  auto Json = fmt::format(R"(
{{
    "jsonrpc": "2.0",
    "id": {},
    "error": {{ "code": {}, "message": "{}" }}
}})",
                          IdToString(Id), Error, Message);
  fmt::print("Content-Length: {}\r\n\r\n{}", Json.size(), Json);
  Log(">> Error: {}\n", Json);
  std::cout.flush();
}

void lsp::Server::SendResult(json::value Id, std::string_view Result) {
  auto Message = fmt::format(R"(
{{
    "jsonrpc": "2.0",
    "id": {},
    "result": {}
}})",
                             IdToString(Id), Result);

  Sanitize(Message);
  fmt::print("Content-Length: {}\r\n\r\n{}", Message.size(), Message);
  Log(">> Sent result: {}\n", Message);
  std::cout.flush();
}

void lsp::Server::SendNotification(std::string_view Method,
                                   std::string_view Params) {
  auto Message = fmt::format(R"(
{{
    "jsonrpc": "2.0",
    "method": "{}",
    "params": {}
}})",
                             Method, Params);

  Sanitize(Message);
  fmt::print("Content-Length: {}\r\n\r\n{}", Message.size(), Message);
  Log(">> Sent notification: {}\n", Message);
  std::cout.flush();
}

void lsp::Server::Initialize(json::value Id, json::object Value) {
  Log(">> Processing 'initialize'.\n");
  if (Initialized)
    return SendError(InvalidRequest, "Server already initialized", Id);

  // Most fields here are not needed.
  json::value It = Value.find_field("workspaceFolders");
  if (It.type() == json::json_type::array) {
    Workspaces = ParseWorkspace(It.get_array());
    for (const auto &Workspace : Workspaces)
      LoadFromWorkspace(Workspace);
  }

  // Tell the client that we are initialized.
  Initialized = true;

  std::string TokenTypes;
  for (const auto &Token : Tokens) {
    TokenTypes += '"';
    TokenTypes += Token;
    TokenTypes += "\",";
  }
  TokenTypes.pop_back();

  std::string TokenModifiers;
  for (const auto &Modifier : Modifiers) {
    TokenModifiers += '"';
    TokenModifiers += Modifier;
    TokenModifiers += "\",";
  }
  TokenModifiers.pop_back();

  SendResult(Id, fmt::format(R"(
{{
    "capabilities":
    {{
          "textDocumentSync": {{ "openClose": true, "change": 2 }},
          "workspace": {{ "workspaceFolders": {{ "supported": true, "changeNotifications": true }} }},
          "completionProvider": {{}},
          "hoverProvider": {{}},
          "semanticTokensProvider": {{ "legend": {{ "tokenTypes": [{}], "tokenModifiers": [{}] }},
              "range": true, "full": true }},
          "declarationProvider": {{}},
          "definitionProvider": {{}},
          "implementationProvider": {{}}
    }},
    "serverInfo": {{ "name": "es-lsp", "version": "v1.0" }}
}})",
                             TokenTypes, TokenModifiers));
  Log(">> 'initialize' done!\n");
}

void lsp::Server::DidChangeWorkspaceFolders(json::value Id,
                                            json::object Value) {
  Log(">> Processing 'workspace/didChangeWorkspaceFolders'.\n");

  json::object Event = Value["event"];

  // Now actually change definitions based on the workspace change.
  for (json::object Remove : Event["removed"]) {
    std::string_view Path = Remove["uri"];

    auto WorkspaceIt = std::find_if(
        Workspaces.begin(), Workspaces.end(),
        [&Path](const auto &Workspace) { return Workspace.Path == Path; });
    if (WorkspaceIt == Workspaces.end())
      return SendError(InvalidRequest, "Specified workspace doesn't exist", Id);

    // Remove every file that belongs to the workspace.
    for (auto It = Files.begin(); It != Files.end();)
      if (It->second.Parent == &*WorkspaceIt)
        It = Files.erase(It);
      else
        ++It;
    Workspaces.erase(WorkspaceIt);
  }
  for (json::object Add : Event["added"]) {
    Workspaces.emplace_back();
    Workspaces.back().Name = Add["uri"].get_string().value();
    Workspaces.back().Path = UriToFsPath(Add["path"]);
    LoadFromWorkspace(Workspaces.back());
  }
  Log(">> 'workspace/didChangeWorkspaceFolders' done!\n");
}

void lsp::Server::DidOpen(json::value Id, json::object Value) {
  Log(">> Processing 'textDocument/didOpen'.\n");

  json::object TextDocument = Value["textDocument"];
  std::string_view Path = TextDocument["uri"];
  int Version = TextDocument["version"];
  std::string_view Text = TextDocument["text"];

  auto FsPath = UriToFsPath(Path);
  auto It = Files.find(FsPath);
  if (It != Files.end())
    UpdateDiagnosticsFor(FsPath,
                         It->second); // The files is already loaded, normally
                                      // because it is part of a workspace.
  else {
    // If the file doesn't exist we load it.
    It = Files.emplace(FsPath, File()).first;
    It->second.Path = FsPath;
    It->second.Content = TextToLines(Text);
    It->second.CachedNodes = LoadFromLines(Path, It->second.Content);
    UpdateDiagnosticsFor(Path, It->second);
  }
  It->second.Version = Version;
  It->second.IsOpen = true;

  Log(">> 'textDocument/didOpen' done!\n");
}

void lsp::Server::DidChange(json::value Id, json::object Value) {
  Log(">> Processing 'textDocument/didChange'.\n");

  json::object TextDocument = Value["textDocument"];
  std::string_view Path = TextDocument["uri"];
  int Version = TextDocument["version"];
  json::array Changes = Value["contentChanges"];

  auto FsPath = UriToFsPath(Path);
  auto It = Files.find(FsPath);
  if (It == Files.end())
    return SendError(InvalidRequest, "didChange called on unloaded file", Id);

  auto &File = It->second;
  if (!File.IsOpen)
    return SendError(InvalidRequest, "didChange called on unopened file", Id);

  // If the version is older than what the server has, don't do anything.
  if (File.Version > Version)
    return;

  for (json::object Change : Changes) {
    Location Start;
    Location End;

    json::value Range = Change.find_field("range");
    if (Range.type() == json::json_type::object) {
      Start = ParseLocation(Range["start"]);
      End = ParseLocation(Range["end"]);
    }

    std::string_view Text = Change["text"];

    // Now actually update our copy of the file and recalculate every
    // diagnostic.
    File.Version = Version;
    if (!Start && !End) {
      // The whole file changed.
      File.Content = TextToLines(Text);
      continue;
    }

    // Only a subset changed. Find out where exactly the change occurred.
    // We're manipulation lines, so we need to detect any newlines in the
    // new text and insert the new lines in the content array.
    auto NewLines = TextToLines(Text);
    // If we're modifying more lines than the range specifies, then this means
    // that we have new lines!

    if (Start.Line == End.Line && NewLines.size() == 1) {
      File.Content[Start.Line].erase(Start.Column, End.Column - Start.Column);
      File.Content[Start.Line].insert(Start.Column, NewLines.front());
      continue;
    }

    // The first and last lines are special: They can be modified in place.
    std::string StartSave = File.Content[Start.Line].substr(Start.Column);
    File.Content[Start.Line].erase(Start.Column);
    File.Content[Start.Line].insert(Start.Column, NewLines.front());

    const auto LinesChanged = End.Line - Start.Line;
    const auto EndIndex = std::min(LinesChanged, NewLines.size() - 1);
    std::size_t LastReplaced = 0;
    for (unsigned I = 1; I < EndIndex; ++I) {
      File.Content[Start.Line + I] = std::move(NewLines[I]);
      LastReplaced = I;
    }

    // Now either insert of delete lines. If we're deleting we might need to
    // include the start or end line.
    const auto LinesToInsert = NewLines.size() - 1;
    if (LinesToInsert == LinesChanged) {
      // We simply need to replace the last line.
      File.Content[End.Line].erase(0, End.Column);
      File.Content[End.Line].insert(0, NewLines.back());
    } else if (LinesToInsert < LinesChanged) {
      // Erase the rest of the lines.
      const bool EndRest = End.Column < File.Content[End.Line].size();
      std::string Save;
      if (EndRest)
        Save = File.Content[End.Line].substr(End.Column);
      File.Content.erase(File.Content.begin() + Start.Line + LastReplaced + 1,
                         File.Content.begin() + End.Line + 1);
      File.Content[Start.Line + LastReplaced].append(Save);
    } else {
      // Insert the rest of the lines.
      NewLines.back().append(StartSave);
      File.Content.insert(File.Content.begin() + Start.Line + LastReplaced + 1,
                          NewLines.begin() + LastReplaced + 1, NewLines.end());
    }
  }

  File.CachedNodes = LoadFromLines(Path, File.Content);
  UpdateDiagnosticsFor(FsPath, File);
  Log(">> 'textDocument/didChange' done!\n");
}

void lsp::Server::DidClose(json::value Id, json::object Value) {
  Log(">> Processing 'textDocument/didClose'.\n");

  json::object TextDocument = Value["textDocument"];

  auto It = Files.find(UriToFsPath(TextDocument["uri"]));
  if (It == Files.end())
    return SendError(InvalidRequest, "can't close file that hasn't been loaded",
                     Id);
  if (!It->second.IsOpen)
    return SendError(InvalidRequest, "can't close file that hasn't been opened",
                     Id);
  It->second.IsOpen = false;
  It->second.Content.clear();

  Log(">> 'textDocument/didClose' done!\n");
}

void lsp::Server::Completion(json::value Id, json::object Value) {
  Log(">> Processing 'textDocument/completion'.\n");

  json::object TextDocument = Value["textDocument"];

  auto FsPath = UriToFsPath(TextDocument["uri"]);
  auto Position = ParseLocation(Value["position"]);

  // Now we need to figure out what the autocomplete list is.
  auto FileIt = Files.find(FsPath);
  if (FileIt == Files.end())
    return SendError(InvalidRequest, "completion wanted on unloaded file", Id);

  auto It = FileIt->second.CachedNodes.Nodes.find(Position.Line);
  if (It == FileIt->second.CachedNodes.Nodes.end()) {
    // We are somewhere without a node definition so figure out on what indent
    // level the client is.
    if (Position.Line >= FileIt->second.Content.size())
      return SendError(InvalidRequest, "line too big", Id);

    std::string_view CurrentLine = FileIt->second.Content[Position.Line];
    const auto Indent =
        CountLineIndentation(CurrentLine, /*AllowEmptyLines=*/true);

    // Calculate line of the parent node.
    std::size_t ParentLine = -1;
    for (auto I = Position.Line - 1; I >= 0; --I) {
      std::string_view Line = FileIt->second.Content[I];
      const auto LineIndent =
          CountLineIndentation(Line, /*AllowEmptyLines=*/true);
      if (LineIndent < Indent) {
        // We have the parent, but that's enough.
        ParentLine = I;
        break;
      }
      if (!Indent)
        // Stop.
        break;
    }

    std::vector<std::string_view> Candidates;
    if (!Indent) {
      // No parent means this is a root node to autocomplete.
      Candidates.reserve(Definitions.size());
      for (const auto &Def : Definitions)
        Candidates.emplace_back(Def.first);
    } else if (ParentLine == (std::size_t)-1)
      return SendResult(Id, "[]");
    else {
      const auto &ChildrenIt =
          FileIt->second.CachedNodes.Nodes.find(ParentLine);
      if (ChildrenIt == FileIt->second.CachedNodes.Nodes.end())
        return SendResult(Id, "[]");
      if (!ChildrenIt->second.Definition)
        return SendResult(Id, "[]");

      Candidates.reserve(ChildrenIt->second.Definition->Children.size());
      for (const auto &Child : ChildrenIt->second.Definition->Children)
        Candidates.emplace_back(Child.Name);
    }

    // Now we build the list of candidates.
    // TODO: This code is duplicated.
    std::string Array;
    for (const auto &Candidate : Candidates) {
      std::string Insert(Candidate);

      // Root level nodes are never quoted.
      if (Indent) {
        Insert = "\\\"" + Insert;
        Insert += "\\\"";
      }
      Array += fmt::format(
          R"(
{{
    "label": "{}",
    "kind": 14,
    "detail": "this is detail",
    "documentation": "documentation yay",
    "insertText": "{}"
}})",
          Candidate, Insert);
      Array += ',';
    }
    if (!Candidates.empty())
      Array.pop_back();
    return SendResult(Id, fmt::format("[{}]", Array));
  }

  const auto &Node = It->second;
  // Nice, we found the line. Now check where exactly the client wants to
  // autocomplete.

  // Figure out what parameter we are autocompleting.
  std::size_t Index = -1;
  for (std::size_t I = 0; I < Node.Parameters.size(); ++I) {
    bool IsQuoted = Node.Quoted[I];
    if (Position.Column >= Node.Columns[I] - IsQuoted &&
        Position.Column <=
            Node.Columns[I] + Node.Parameters[I].size() + 2 * IsQuoted) {
      Index = I;
      break;
    }
  }
  if (Index == (std::size_t)-1)
    return SendResult(Id, "[]");

  std::vector<std::string_view> Candidates;
  if (!Index) {
    // If we are autocompleting the first parameter, then we need to inspect the
    // parent to know the possible values.
    if (Node.Parent) {
      Candidates.reserve(Node.Parent->Definition->Children.size());
      for (const auto &Child : Node.Parent->Definition->Children)
        Candidates.emplace_back(Child.Name);
    } else {
      // There is no parent, this means the candidates are the root defs.
      Candidates.reserve(Definitions.size());
      for (const auto &Def : Definitions)
        Candidates.emplace_back(Def.first);
    }
  } else {
    if (!Node.Definition)
      // Not much we can autocomplete if the node is invalid.
      return SendResult(Id, "[]");
    if (Index - 1 >= Node.Definition->ParameterTypes.size())
      // Not much to do if there the argument is out of range.
      return SendResult(Id, "[]");

    // We inspect the type of the parameter to figure out what to autocomplete.
    // This is only relevant for keywords.
    const auto &Type = Node.Definition->ParameterTypes[Index - 1];
    Candidates.reserve(Type.Keywords.size());
    for (const auto &Keyword : Type.Keywords)
      Candidates.emplace_back(Keyword);

    // We might have an annotated string instead.
    if (Type.Keywords.empty() && !Type.Annotation.empty()) {
      for (const auto &Str : GetAllEntitiesNamed(Type.Annotation))
        // FIXME: This is wrong for definitions with name in third place
        // ('ship').
        Candidates.emplace_back(Str.second->Node->Parameters[1]);
    }
  }

  // Always add quotes except for root nodes, if the parameter
  // already has quotes and if the cursor is at the start/end of the
  // parameter.
  bool AddQuotes = false;
  bool AddStartQuote = false;
  bool AddEndQuote = false;
  if (Node.Parent) {
    if (!Node.Quoted[Index])
      AddQuotes = true;
    if (Position.Column == Node.Columns[Index] - Node.Quoted[Index])
      AddStartQuote = true;
    if (Position.Column == Node.Columns[Index] +
                                Node.Parameters[Index].size() +
                                Node.Quoted[Index])
      AddEndQuote = true;
  }

  // Now that we have the candidate list, send it to the client.
  std::string Array;
  for (const auto &Candidate : Candidates) {
    const bool IsCurrent = Candidate == Node.Parameters[Index];
    std::string NewText(Candidate);

    if (AddQuotes || AddStartQuote || AddEndQuote) {
      if (AddQuotes || AddStartQuote)
        NewText = "\\\"" + NewText;
      if (AddQuotes || AddEndQuote)
        NewText += "\\\"";
    }
    Array += fmt::format(
        R"(
{{
    "label": "{}",
    "kind": 14,
    "detail": "this is detail",
    "documentation": "documentation yay",
    "preselect": {},
    "filterText": "{}",
    "textEdit": {{ "newText": "{}", "range": {} }}
}})",
        Candidate, IsCurrent,
        std::string(Node.Parameters[Index]) + std::string(Candidate), NewText,
        RangeToJson(
            {Node.Line, Node.Columns[Index]},
            {Node.Line, Node.Columns[Index] + Node.Parameters[Index].size()}));
    Array += ',';
  }
  if (!Candidates.empty())
    Array.pop_back();

  SendResult(Id, fmt::format(R"([{}])", Array));
  Log(">> 'textDocument/completion' done!\n");
}

void lsp::Server::Hover(json::value Id, json::object Value) {
  Log(">> Processing 'textDocument/hover'.\n");

  json::object TextDocument = Value["textDocument"];

  auto FsPath = UriToFsPath(TextDocument["uri"]);
  auto Position = ParseLocation(Value["position"]);

  auto FileIt = Files.find(FsPath);
  if (FileIt == Files.end())
    return SendError(InvalidRequest, "hover wanted on unloaded file", Id);

  auto It = FileIt->second.CachedNodes.Nodes.find(Position.Line);
  if (It == FileIt->second.CachedNodes.Nodes.end())
    // Nothing to hover.
    return SendResult(Id, "null");

  const auto *Def = It->second.Definition;
  if (!Def)
    return SendResult(Id, "null");

  auto *ParentNode = It->second.Parent ? It->second.Parent : &It->second;
  while (ParentNode->Parent)
    ParentNode = ParentNode->Parent;
  // Now we just need to display the required docs to the client.
  auto TooltipIt = NodeTooltips.find(ParentNode->Parameters.front());
  if (TooltipIt == NodeTooltips.end())
    // Something went wrong, invalid nodes should have been caught above.
    return SendResult(Id, "null");

  auto Tooltip = TooltipIt->second.find(It->second.Parameters.front());
  if (Tooltip == TooltipIt->second.end())
    // Something went wrong, the child of the root should exist at this point.
    return SendResult(Id, "null");

  std::string Message =
      Tooltip->second.second.empty()
          ? fmt::format("{} {}", Tooltip->first, Tooltip->second.first)
          : fmt::format("{} {}\\n___\\n*{}*", Tooltip->first,
                        Tooltip->second.first, Tooltip->second.second);

  SendResult(
      Id, fmt::format(R"(
{{
    "contents": {{ "kind": "markdown", "value": "{}" }},
    "range": {}
}})",
                      Message,
                      RangeToJson({It->second.Line, It->second.Columns.front()},
                                  {It->second.Line,
                                   It->second.Columns.front() +
                                       It->second.Parameters.front().size()})));
  Log(">> 'textDocument/hover' done!\n");
}

void lsp::Server::SemanticTokensFull(json::value Id,
                                     json::object Value) {
  Log(">> Processing 'textDocument/semanticTokens/full'.\n");
  json::object TextDocument = Value["textDocument"];

  auto FileIt = Files.find(UriToFsPath(TextDocument["uri"]));
  if (FileIt == Files.end())
    return SendError(InvalidRequest, "semantic tokens wanted on unloaded file",
                     Id);

  SendResult(
      Id, fmt::format(R"({{ "data": [{}] }})",
                      CalculateAndSendSemanticTokens(
                          FileIt->second, 0, FileIt->second.Content.size())));
  Log(">> 'textDocument/semanticTokens/full' done!\n");
}

void lsp::Server::SemanticTokensDelta(json::value Id,
                                      json::object Value) {}

void lsp::Server::SemanticTokensRange(json::value Id,
                                      json::object Value) {
  Log(">> Processing 'textDocument/semanticTokens/full'.\n");
  json::object TextDocument = Value["textDocument"];

  json::object Range = Value["range"];
  auto Start = ParseLocation(Range["start"]);
  auto End = ParseLocation(Range["end"]);

  auto FileIt = Files.find(UriToFsPath(TextDocument["uri"]));
  if (FileIt == Files.end())
    return SendError(InvalidRequest, "semantic tokens wanted on unloaded file",
                     Id);

  SendResult(Id, fmt::format(R"({{ "data": [{}] }})",
                             CalculateAndSendSemanticTokens(
                                 FileIt->second, Start.Line, End.Line)));
  Log(">> 'textDocument/semanticTokens/full' done!\n");
}

void lsp::Server::Goto(json::value Id, json::object Value) {
  Log(">> Processing 'textDocument/{{declaration,definition}}'.\n");

  json::object TextDocument = Value["textDocument"];

  auto Position = ParseLocation(Value["position"]);

  auto FileIt = Files.find(UriToFsPath(TextDocument["uri"]));
  if (FileIt == Files.end())
    return SendError(InvalidRequest, "goto wanted on unloaded file", Id);

  auto It = FileIt->second.CachedNodes.Nodes.find(Position.Line);
  if (It == FileIt->second.CachedNodes.Nodes.end())
    return SendResult(Id, "null");
  const auto &Node = It->second;

  // Figure out what parameter we are autocompleting.
  // TODO: Tons of code duplication here and elsewhere.
  std::size_t Index = -1;
  for (std::size_t I = 0; I < Node.Parameters.size(); ++I) {
    if (Node.Columns[I] == Position.Column) {
      Index = I;
      break;
    } else if (Position.Column > Node.Columns[I] &&
               Position.Column <=
                   Node.Columns[I] + Node.Parameters[I].size()) {
      Index = I;
      break;
    }
  }
  if (Index == (std::size_t)-1)
    return SendResult(Id, "null");

  if (!Node.Definition || !Index)
    // If the node is invalid or it is the root (roots don't have a type), then
    // there is nothing to do.
    return SendResult(Id, "null");

  auto Annotation = Node.Definition->ParameterTypes[Index - 1].Annotation;
  // Now that we have an index we can inspect it. Goto on anything but entities
  // doesn't make any sense.
  if (Annotation.empty())
    return SendResult(Id, "null");

  // Now that we have an entity with an annotation, we can send all of its
  // definitions to the client.
  auto Entities = GetAllEntitiesNamed(Annotation);

  std::string Array;
  for (const auto &Entity : Entities) {
    // Filter out any entities that don't have the same name.
    if (Entity.second->Node->Parameters[1] != Node.Parameters[Index])
      continue;

    const auto &DestNode = *Entity.second->Node;
    Array += fmt::format(
        R"(
{{
    "originSelectionRange": {},
    "targetUri": "file://{}",
    "targetRange": {},
    "targetSelectionRange": {}
}},)",
        RangeToJson(
            {Node.Line, Node.Columns[Index]},
            {Node.Line, Node.Columns[Index] + Node.Parameters[Index].size()}),
        Entity.first,
        RangeToJson({DestNode.Line, DestNode.Columns.front()},
                    {Entity.second->LastLine,
                     FileIt->second.Content[Entity.second->LastLine].size()}),
        RangeToJson({DestNode.Line, DestNode.Columns[Index]},
                    {DestNode.Line, DestNode.Columns[Index] +
                                        DestNode.Parameters[Index].size()}));
  }
  if (Array.empty())
    return SendResult(Id, "[]");
  Array.pop_back();

  SendResult(Id, fmt::format("[{}]", Array));
  Log(">> 'textDocument/{{declaration,definition}}' done!\n");
}

void lsp::Server::UpdateDiagnosticsFor(std::string_view Uri, const File &File) {
  Log(">> Processing {} diagnostics.\n", File.CachedNodes.Diagnostics.size());

  auto JsonifyDiagnostic = [](const auto &Diagnostic) {
    return fmt::format(R"(
{{
        "range":
        {{
            "start": {{ "line": {}, "character": {} }},
            "end": {{ "line": {}, "character": {} }}
        }},
        "severity": {},
        "message": "{}"

}})",
                       Diagnostic.Line, Diagnostic.Column, Diagnostic.Line,
                       Diagnostic.EndColumn, Diagnostic.Kind,
                       Diagnostic.Message);
  };

  std::string Diagnostics;
  for (const auto &Diagnostic : File.CachedNodes.Diagnostics)
    Diagnostics += JsonifyDiagnostic(Diagnostic) + ',';
  if (!Diagnostics.empty())
    Diagnostics.pop_back();

  SendNotification("textDocument/publishDiagnostics",
                   fmt::format(R"(
{{
    "uri": "{}",
    "version": {},
    "diagnostics": [{}]
}})",
                               Uri, File.Version, Diagnostics));

  Log(">> Diagnostics done!\n");
}

void lsp::Server::LoadFromWorkspace(const Workspace &Workspace) {
  auto DataFiles = FindESData(Workspace.Path);

  for (const auto &Path : DataFiles) {
    auto It = Files.find(Path);
    if (It != Files.end()) {
      // The file is already loaded, but we still need to update its
      // workspace.
      It->second.Parent = &Workspace;
      continue;
    }

    auto &File = Files[Path];
    File.Path = Path;
    File.Parent = &Workspace;
    File.Content = FileToLines(Path);
    File.CachedNodes = LoadFromLines(Path, File.Content);

    // Workspace files are always error checked.
    if (Initialized)
      UpdateDiagnosticsFor(Workspace.Path, File);
  }
}

std::vector<std::pair<std::string_view, const lsp::Entity *>>
lsp::Server::GetAllEntitiesNamed(std::string_view Name) {
  std::vector<std::pair<std::string_view, const Entity *>> Entities;
  for (auto &File : Files) {
    const auto &ToAdd = File.second.CachedNodes.Entities[Name];
    Entities.reserve(Entities.size() + ToAdd.size());
    for (const auto &Element : ToAdd)
      Entities.emplace_back(File.second.Path, &Element);
  }
  return Entities;
}

std::string lsp::Server::CalculateAndSendSemanticTokens(const File &File,
                                                        unsigned StartLine,
                                                        unsigned EndLine) {

  // Stores the list of completion tokens for the client. Every token has 5
  // entries in this array (so for N tokens this array has size 5 * N).
  //   - Line
  //   - Start Column
  //   - Length
  //   - Token Type
  //   - Token Modifier(s)
  std::vector<int> Tokens;

  // Conversative guess: Every line has a single token.
  Tokens.reserve(EndLine - StartLine);

  std::size_t I = StartLine;
  auto MarkToken = [&Tokens, &I, PrevLine = std::size_t(),
                    PrevStart = std::size_t()](
                       std::size_t Start, std::size_t Length, TokenTypes Type,
                       ModifierTypes Modifiers) mutable {
    Tokens.reserve(Tokens.size() + 5);

    // We need to perform a delta to the previous position.
    Tokens.push_back(I > PrevLine ? I - PrevLine : 0);
    Tokens.push_back(I == PrevLine ? Start - PrevStart : Start);

    Tokens.push_back(Length);
    Tokens.push_back((int)Type);
    Tokens.push_back((int)Modifiers);

    PrevStart = Start;
    PrevLine = I;
  };
  const auto MarkAsComment = [&MarkToken](std::size_t Start, std::size_t End) {
    MarkToken(Start, End - Start, TokenTypes::Comment, ModifierTypes::None);
  };

  // We go through every single line of the file and generate the required
  // tokens.
  for (; I < EndLine; ++I) {
    const auto &Line = File.Content[I];

    auto First = Line.find_first_not_of(" \f\t\v\r\n");
    if (First == std::string::npos)
      // This line is completely empty.
      continue;
    if (Line[First] == '#') {
      MarkAsComment(First, Line.size());
      continue;
    }

    auto NodeIt = File.CachedNodes.Nodes.find(I);
    if (NodeIt != File.CachedNodes.Nodes.end()) {
      const auto &Node = NodeIt->second;

      // Since this line has a valid DataNode, we can color the first parameter.
      const auto Indent = Node.Indent;

      // A root node is marked as a 'class', else it's a 'property'.
      const bool IsRoot = !Indent;
      bool IsQuoted = Node.Quoted.front();
      MarkToken(Node.Columns.front() - IsQuoted,
                Node.Parameters.front().size() + 2 * IsQuoted,
                !IsRoot ? TokenTypes::Class : TokenTypes::Keyword,
                ModifierTypes::None);

      // Now we need to mark all of the parameters.
      // TODO: We parse parameters here *again* (the first time they are parsed
      // for type checking). It makes sense to store the parsed type so that we
      // can reuse it here.
      for (std::size_t I = 1; I < Node.Parameters.size(); ++I) {
        auto ParamType = Type::FromString(Node.Parameters[I]);
        auto Token = ParamType.Kind == Type::String ? TokenTypes::String
                                                    : TokenTypes::Number;
        auto Modifier = ModifierTypes::None;

        if (IsRoot && I + 1 == Node.Parameters.size()) {
          Token = TokenTypes::Variable;
          Modifier = ModifierTypes::Definition;
        }

        bool IsQuoted = Node.Quoted[I];
        MarkToken(Node.Columns[I] - IsQuoted,
                  Node.Parameters[I].size() + 2 * IsQuoted, Token, Modifier);
      }

      // After all the parameters there might be a comment.
      auto CommentStart =
          Line.find('#', Node.Columns.back() + Node.Parameters.back().size());
      if (CommentStart != std::string::npos)
        MarkAsComment(CommentStart, Line.size());
    }
  }

  if (Tokens.empty())
    return {};

  std::string Array;
  // To avoid tons of (re)allocations, we guess the size.
  // Some reasonable upper bounds:
  //   - Line: 4 (files with more than 4 digits of lines are rare)
  //   - Column: 2
  //   - Length: 2
  //   - Type + Modifier: 2
  // => 10 per semantic token.
  Array.resize((10 / 5) * Tokens.size() + Tokens.size());
  char *It = Array.data();
  char *End = Array.data() + Array.size();
  for (auto T : Tokens) {
    It = std::to_chars(It, End, T).ptr;
    *It++ = ',';

    // Check if there is enough space left.
    if (End - It < 10) {
      // Unfortunately this now requires an expensive reallocation.
      // We double the size, which should be enough. In most cases, it is too
      // much.
      auto Index = It - Array.data();
      Array.resize(Array.size() * 2);
      It = Array.data() + Index;
      End = Array.data() + Array.size();
    }
  }

  Array.resize(It - Array.data() - 1);
  return Array;
}
