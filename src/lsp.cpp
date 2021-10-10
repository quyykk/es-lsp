#include "lsp.h"

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

namespace impl {

// Parses a value, either a string or an int from JSON. If it exists.
template <typename T>
std::optional<T> GetValue(const json::Value &Value, std::string_view Name) {
  auto It = Value.FindMember(Name.data());
  if (It == Value.MemberEnd())
    return std::nullopt;

  if constexpr (std::is_same_v<T, int>) {
    if (!It->value.IsInt())
      return std::nullopt;
    return It->value.GetInt();
  } else if constexpr (std::is_same_v<T, std::string_view>) {
    if (!It->value.IsString())
      return std::nullopt;
    return It->value.GetString();
  } else if constexpr (std::is_same_v<T, unsigned>) {
    if (!It->value.IsUint())
      return std::nullopt;
    return It->value.GetUint();
  }

  return std::nullopt;
}

} // namespace impl

std::optional<std::string_view> GetString(const json::Value &Value,
                                          std::string_view Name) {
  return impl::GetValue<std::string_view>(Value, Name);
}
std::optional<int> GetInt(const json::Value &Value, std::string_view Name) {
  return impl::GetValue<int>(Value, Name);
}
std::optional<unsigned> GetUint(const json::Value &Value,
                                std::string_view Name) {
  return impl::GetValue<unsigned>(Value, Name);
}

// Parses a workspace json array.
std::list<lsp::Workspace> ParseWorkspace(const json::Value &Value) {
  std::list<lsp::Workspace> workspaces;
  for (json::SizeType I = 0; I < Value.Size(); ++I) {
    auto Name = GetString(Value[I], "name");
    if (!Name)
      continue;
    auto Uri = GetString(Value[I], "uri");
    if (!Uri)
      continue;

    workspaces.emplace_back();
    workspaces.back().Name = *Name;
    workspaces.back().Path = lsp::UriToFsPath(*Uri);
  }
  return workspaces;
}

// Parses a location.
std::optional<lsp::Location> ParseLocation(const json::Value &Value) {
  auto Line = GetUint(Value, "line");
  if (!Line)
    return std::nullopt;
  auto Column = GetUint(Value, "character");
  if (!Column)
    return std::nullopt;
  return lsp::Location{*Line, *Column};
}

std::string RangeToJson(const lsp::Location &Start, const lsp::Location &End) {
  return fmt::format(R"(
{{
    "start": {{ "line": {}, "character": {} }},
    "end": {{" line": {}, "character": {} }}
}})",
                     Start.Line, Start.Column, End.Line, End.Column);
}

constexpr int ParseError = -32700;
constexpr int InvalidRequest = -32600;
constexpr int MethodNotFound = -32601;
} // namespace

void lsp::Server::HandleNotification(std::string Message) {
  Log(">> Client sent:\n{}'\n", Message);

  json::Document Doc;
  if (Doc.Parse(Message.data()).HasParseError())
    return SendError(ParseError, "Invalid JSON");
  if (!Doc.IsObject())
    return SendError(ParseError, "JSON document not an object");

  // We expect a valid JSON-RPC object.
  auto Version = GetString(Doc, "jsonrpc");
  if (Version != "2.0")
    return SendError(InvalidRequest, "Unknown JSON RPC version");

  auto Id = Doc.FindMember("id");
  // Requests without an "id" are in fact a notification.

  auto Method = GetString(Doc, "method");
  if (!Method)
    return SendError(ParseError, "Error parsing 'method' member", Id->value);

  auto ParamsVal = Doc.FindMember("params");
  // "params" is optional.

  Log(">> Received method '{}'.\n", *Method);

  // Now that we have parsed the JSON RPC object, handle the given request.
  if (*Method == "initialize")
    return Initialize(Id->value, ParamsVal->value);
  if (*Method == "initialized")
    // All good! Nothing to do here.
    return;

  // The "initialize" request needs to be called before all other requests.
  if (!Initialized)
    return SendError(-32002, "Did not call 'initialize'", Id->value);
  if (*Method == "shutdown") {
    Shutdown = true;
    Log(">> 'shutdown' done!\n");
    // Workaround for buggy clients (like the VScode one)
    // that don't send an 'exit' notification sometimes.
    State = ServerState::ExitSuccess;
    return SendResult(Id->value, "null");
  }
  if (*Method == "exit") {
    State = Shutdown ? ServerState::ExitSuccess : ServerState::ExitError;
    Log(">> 'exit' done!\n");
    return;
  }
  if (Shutdown)
    return SendError(InvalidRequest, "Server is shutting down", Id->value);

  // TODO: What are workspace symbols?
  // TODO: Implement file watch operations.
  if (*Method == "workspace/didChangeWorkspaceFolders")
    return DidChangeWorkspaceFolders(Id->value, ParamsVal->value);
  if (*Method == "workspace/didChangeConfiguration")
    // We don't have any configurations yet.
    return;
  if (*Method == "textDocument/didOpen")
    return DidOpen(Id->value, ParamsVal->value);
  if (*Method == "textDocument/didChange")
    return DidChange(Id->value, ParamsVal->value);
  if (*Method == "textDocument/didClose")
    return DidClose(Id->value, ParamsVal->value);
  if (*Method == "textDocument/completion")
    return Completion(Id->value, ParamsVal->value);
  // TODO: 'textDocument/hover'
  // TODO: 'textDocument/signatureHelp'
  // TODO: 'textDocument/declaration'
  // TODO: 'textDocument/definition'
  // TODO: 'textDocument/implementation'
  // TODO: 'textDocument/references'
  // TODO: 'textDocument/documentHighlight'
  // TODO: 'textDocument/documentSymbol'
  // TODO: 'textDocument/documentColor'
  // TODO: 'textDocument/colorPresentation'
  // TODO: 'textDocument/rename'
  // TODO: 'textDocument/foldingRange'
  // TODO: 'textDocument/semanticTokens/full'
  // TODO: 'textDocument/semanticTokens/full/delta'
  // TODO: 'textDocument/semanticTokens/range'
  // TODO: 'textDocument/linkedEditingRange'

  // Unknown method.
  LogError(">> Unknown method '{}'.\n", *Method);
  return SendError(MethodNotFound, "Method not found", Id->value);
}

void lsp::Server::LoadFromDirectory(std::string_view Path) {
  if (!fs::exists(Path) || !fs::is_directory(Path))
    return;

  for (const auto &Path : fs::recursive_directory_iterator(Path)) {
    if (Path.is_directory())
      continue;

    const std::string FsPath = Path.path().c_str();
    if (Files.find(FsPath) != Files.end())
      // The file already exists. Either the file was opened explicitly by the
      // client, or it was opened as part of a workspace or the resource
      // directory.
      // We don't really need to do anything.
      continue;

    auto &File = Files[FsPath];
    File.Path = FsPath;
    File.CachedNodes = LoadFromFile(FsPath);
  }
}

auto lsp::Server::GetState() const -> ServerState { return State; }

void lsp::Server::SendError(int Error, std::string_view Message,
                            const json::Value &Id) {
  std::string IdStr;
  if (Id.IsString())
    IdStr = "\""s + Id.GetString() + "\"";
  else if (Id.IsInt())
    IdStr = fmt::format("{}", Id.GetInt());
  else
    IdStr = "null";

  auto Json = fmt::format(R"(
{{
    "jsonrpc": "2.0",
    "id": {},
    "error": {{ "code": {}, "message": "{}" }}
}})",
                          IdStr, Error, Message);
  fmt::print("Content-Length: {}\r\n\r\n{}", Json.size(), Json);
  Log(">> Error: {}\n", Json);
  std::cout.flush();
}

void lsp::Server::SendResult(const json::Value &Id, std::string_view Result) {
  std::string IdStr;
  if (Id.IsString())
    IdStr = "\""s + Id.GetString() + "\"";
  else
    IdStr = fmt::format("{}", Id.GetInt());
  auto Message = fmt::format(R"(
{{
    "jsonrpc": "2.0",
    "id": {},
    "result": {}
}})",
                             IdStr, Result);
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

  fmt::print("Content-Length: {}\r\n\r\n{}", Message.size(), Message);
  Log(">> Sent notification: {}\n", Message);
  std::cout.flush();
}

void lsp::Server::Initialize(const json::Value &Id, const json::Value &Value) {
  Log(">> Processing 'initialize'.\n");
  if (Initialized)
    return SendError(InvalidRequest, "Server already initialized", Id);

  // Most fields here are not needed.
  auto It = Value.FindMember("workspaceFolders");
  if (It != Value.MemberEnd() && It->value.IsArray()) {
    Workspaces = ParseWorkspace(It->value);
    for (const auto &Workspace : Workspaces)
      LoadFromWorkspace(Workspace);
  }

  // Tell the client that we are initialized.
  Initialized = true;
  SendResult(Id, R"(
{
    "capabilities":
    {
          "textDocumentSync": { "openClose": true, "change": 1 },
          "workspace": { "workspaceFolders": { "supported": true, "changeNotifications": true }},
          "completionProvider": {}
    },
    "serverInfo": { "name": "es-lsp", "version": "v1.0" }
})");
  Log(">> 'initialize' done!\n");
}

void lsp::Server::DidChangeWorkspaceFolders(const json::Value &Id,
                                            const json::Value &Value) {
  Log(">> Processing 'workspace/didChangeWorkspaceFolders'.\n");

  auto Event = Value.FindMember("event");
  if (Event == Value.MemberEnd())
    return SendError(ParseError, "Couldn't parse 'event' field", Id);
  auto Added = Event->value.FindMember("added");
  if (Added == Event->value.MemberEnd() || Added->value.IsArray())
    return SendError(ParseError, "Error parsing 'added' field", Id);
  auto Removed = Event->value.FindMember("removed");
  if (Removed == Event->value.MemberEnd() || Removed->value.IsArray())
    return SendError(ParseError, "Error parsing 'removed' field", Id);

  // Now actually change definitions based on the workspace change.
  for (json::SizeType I = 0; I < Removed->value.Size(); ++I) {
    const auto &Remove = Removed[I].value;
    if (!Remove.IsObject())
      return SendError(ParseError, "Array element isn't an object", Id);
    auto Path = GetString(Remove, "uri");
    if (!Path)
      return SendError(ParseError, "Error parsing 'uri' field", Id);

    auto WorkspaceIt = std::find_if(
        Workspaces.begin(), Workspaces.end(),
        [&Path](const auto &Workspace) { return Workspace.Path == *Path; });
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
  for (json::SizeType I = 0; I < Added->value.Size(); ++I) {
    const auto &Add = Added[I].value;
    if (!Add.IsObject())
      return SendError(ParseError, "Array element isn't an object", Id);
    auto Path = GetString(Add, "uri");
    if (!Path)
      return SendError(ParseError, "Error parsing 'uri' field", Id);
    auto Name = GetString(Add, "name");
    if (!Name)
      return SendError(ParseError, "Error parsing 'name' field", Id);

    Workspaces.emplace_back();
    Workspaces.back().Name = *Name;
    Workspaces.back().Path = UriToFsPath(*Path);
    LoadFromWorkspace(Workspaces.back());
  }
  Log(">> 'workspace/didChangeWorkspaceFolders' done!\n");
}

void lsp::Server::DidOpen(const json::Value &Id, const json::Value &Value) {
  Log(">> Processing 'textDocument/didOpen'.\n");

  auto TextDocument = Value.FindMember("textDocument");
  if (TextDocument == Value.MemberEnd() || !TextDocument->value.IsObject())
    return SendError(ParseError, "Error parsing 'textDocument' field", Id);
  auto Path = GetString(TextDocument->value, "uri");
  if (!Path)
    return SendError(ParseError, "Error parsing 'uri' field", Id);
  auto Version = GetInt(TextDocument->value, "version");
  if (!Version)
    return SendError(ParseError, "Error parsing 'version' field", Id);
  auto Text = GetString(TextDocument->value, "text");
  if (!Text)
    return SendError(ParseError, "Error parsing 'text' field", Id);

  auto FsPath = UriToFsPath(*Path);
  auto It = Files.find(FsPath);
  if (It != Files.end())
    ; // The files is already loaded, normally because it is part of a
      // workspace.
  else {
    // If the file doesn't exist we load it.
    It = Files.emplace(FsPath, File()).first;
    It->second.Path = FsPath;
    It->second.CachedNodes = LoadFromText(*Path, *Text);
    UpdateDiagnosticsFor(*Path, It->second);
  }
  It->second.Content = TextToLines(*Text);
  It->second.Version = *Version;
  It->second.IsOpen = true;

  Log(">> 'textDocument/didOpen' done!\n");
}

void lsp::Server::DidChange(const json::Value &Id, const json::Value &Value) {
  Log(">> Processing 'textDocument/didChange'.\n");

  auto TextDocument = Value.FindMember("textDocument");
  if (TextDocument == Value.MemberEnd() || !TextDocument->value.IsObject())
    return SendError(ParseError, "Error parsing 'textDocument' field", Id);
  auto Path = GetString(TextDocument->value, "uri");
  if (!Path)
    return SendError(ParseError, "Error parsing 'uri' field", Id);
  auto Version = GetInt(TextDocument->value, "version");
  if (!Version)
    return SendError(ParseError, "Error parsing 'version' field", Id);
  auto Changes = Value.FindMember("contentChanges");
  if (Changes == Value.MemberEnd() || !Changes->value.IsArray())
    return SendError(ParseError, "Error parsing 'contentChanges' field", Id);

  auto FsPath = UriToFsPath(*Path);
  auto It = Files.find(FsPath);
  if (It == Files.end())
    return SendError(InvalidRequest, "didChange called on unloaded file", Id);

  auto &File = It->second;
  if (!File.IsOpen)
    return SendError(InvalidRequest, "didChange called on unopened file", Id);

  // If the version is older than what the server has, don't do anything.
  if (File.Version > *Version)
    return;

  for (json::SizeType I = 0; I < Changes->value.Size(); ++I) {
    const auto &Change = Changes->value[I];
    Location Start;
    Location End;

    auto Range = Change.FindMember("range");
    if (Range != Change.MemberEnd()) {
      if (!Range->value.IsObject())
        return SendError(ParseError, "Error parsing 'range' field", Id);
      auto StartVal = Range->value.FindMember("start");
      if (StartVal == Range->value.MemberEnd() || !StartVal->value.IsObject())
        return SendError(ParseError, "Error parsing 'start' field", Id);
      auto EndVal = Range->value.FindMember("end");
      if (EndVal == Range->value.MemberEnd() || !EndVal->value.IsObject())
        return SendError(ParseError, "Error parsing 'end' field", Id);

      if (auto StartPos = ParseLocation(StartVal->value))
        Start = *StartPos;
      if (auto EndPos = ParseLocation(EndVal->value))
        End = *EndPos;
    }

    auto Text = GetString(Change, "text");
    if (!Text)
      return SendError(ParseError, "Error parsing 'text' field", Id);

    // Now actually update our copy of the file and recalculate every
    // diagnostic.
    File.Version = *Version;
    if (!Start && !End)
      // The whole file changed.
      File.Content = TextToLines(*Text);
    else {
      // FIXME: Only a subset changed. Find out where exactly the change
      // occurred.
      assert(!"not implemented! why is the client sending this?");
    }
    File.CachedNodes = LoadFromText(*Path, *Text);
    UpdateDiagnosticsFor(FsPath, File);
  }

  Log(">> 'textDocument/didChange' done!\n");
}

void lsp::Server::DidClose(const json::Value &Id, const json::Value &Value) {
  Log(">> Processing 'textDocument/didClose'.\n");

  auto TextDocument = Value.FindMember("textDocument");
  if (TextDocument == Value.MemberEnd() || !TextDocument->value.IsObject())
    return SendError(ParseError, "Error parsing 'textDocument' field", Id);
  auto Path = GetString(TextDocument->value, "uri");
  if (!Path)
    return SendError(ParseError, "Error parsing 'uri' field", Id);

  auto FsPath = UriToFsPath(*Path);
  auto It = Files.find(FsPath);
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

void lsp::Server::Completion(const json::Value &Id, const json::Value &Value) {
  Log(">> Processing 'textDocument/completion'.\n");

  auto TextDocument = Value.FindMember("textDocument");
  if (TextDocument == Value.MemberEnd() || !TextDocument->value.IsObject())
    return SendError(ParseError, "Error parsing 'textDocument' field", Id);
  auto Path = GetString(TextDocument->value, "uri");
  if (!Path)
    return SendError(ParseError, "Error parsing 'uri' field", Id);
  auto PositionField = Value.FindMember("position");
  if (PositionField == Value.MemberEnd() || !PositionField->value.IsObject())
    return SendError(ParseError, "Error parsing 'position' field", Id);

  auto FsPath = UriToFsPath(*Path);
  auto Position = ParseLocation(PositionField->value);
  if (!Position)
    return SendError(ParseError, "Error parsing inner 'position' field", Id);

  // Now we need to figure out what the autocomplete list is.
  auto FileIt = Files.find(FsPath);
  if (FileIt == Files.end())
    return SendError(InvalidRequest, "completion wanted on unloaded file", Id);

  auto It = FileIt->second.CachedNodes.Nodes.find(Position->Line);
  if (It == FileIt->second.CachedNodes.Nodes.end()) {
    // We are somewhere without a node definition so figure out on what indent
    // level the client is.
    if (Position->Line >= FileIt->second.Content.size())
      return SendError(InvalidRequest, "line too big", Id);

    std::string_view CurrentLine = FileIt->second.Content[Position->Line];
    const auto Indent =
        std::count(CurrentLine.begin(), CurrentLine.end(), '\t');

    // Calculate line of node with the same indentation.
    std::size_t ParentLine = -1;
    for (auto I = Position->Line - 1; I >= 0; --I) {
      std::string_view Line = FileIt->second.Content[I];
      const auto LineIndent = std::count(Line.begin(), Line.end(), '\t');
      if (Indent - 1 == LineIndent) {
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
    } else if (ParentLine == -1)
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
      Array += fmt::format(
          R"(
{{
    "label": "{}",
    "kind": 14,
    "detail": "this is detail",
    "documentation": "documentation yay"
}})",
          Candidate);
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
    if (Node.Columns[I] == Position->Column) {
      Index = I;
      break;
    } else if (Position->Column > Node.Columns[I] &&
               Position->Column <=
                   Node.Columns[I] + Node.Parameters[I].size()) {
      Index = I;
      break;
    }
  }
  if (Index == -1)
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
    // We inspect the type of the parameter to figure out what to autocomplete.
    // This is only relevant for keywords.
    const auto &Type = Node.Definition->ParameterTypes[Index - 1];
    Candidates.reserve(Type.Keywords.size());
    for (const auto &Keyword : Type.Keywords)
      Candidates.emplace_back(Keyword);
  }

  // Now that we have the candidate list, send it to the client.
  std::string Array;
  for (const auto &Candidate : Candidates) {
    const bool IsCurrent = Candidate == Node.Parameters[Index];
    Array += fmt::format(
        R"(
{{
    "label": "{}",
    "kind": 14,
    "detail": "this is detail",
    "documentation": "documentation yay",
    "preselect": {},
    "filterText": "{}",
    "textEdit": {{ "newText": "{}", "replace": {} }}
}})",
        Candidate, IsCurrent,
        std::string(Node.Parameters[Index]) + std::string(Candidate),
        "\\\"" + std::string(Candidate) + "\\\"",
        // FIXME: Why doesn't VSCode replace the whole word?
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
  if (!fs::exists(Workspace.Path) || !fs::is_directory(Workspace.Path))
    return;

  for (const auto &Path : fs::recursive_directory_iterator(Workspace.Path)) {
    if (Path.is_directory())
      continue;

    const std::string FsPath = Path.path().c_str();
    auto It = Files.find(FsPath);
    if (It != Files.end()) {
      // The file is already loaded, but we still need to update its
      // workspace.
      It->second.Parent = &Workspace;
      continue;
    }

    auto &File = Files[FsPath];
    File.Path = FsPath;
    File.Parent = &Workspace;
    File.CachedNodes = LoadFromFile(FsPath);

    // Workspace files are always error checked.
    UpdateDiagnosticsFor(Workspace.Path, File);
  }
}
