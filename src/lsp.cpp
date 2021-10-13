#include "lsp.h"

#include "definitions.h"
#include "fmt/core.h"
#include "log.h"
#include "sajson.h"
#include "utils.h"

#include <algorithm>
#include <charconv>
#include <iostream>
#include <iterator>
#include <mutex>
#include <optional>
#include <string>
#include <string_view>

using namespace std::literals;

namespace {

namespace impl {

// Parses a value, either a string or an int from JSON. If it exists.
template <typename T>
std::optional<T> GetValue(const sajson::value &Value, std::string_view Name) {
  auto It = Value.get_value_of_key(Name);

  if constexpr (std::is_same_v<T, int>) {
    if (It.get_type() != sajson::TYPE_INTEGER)
      return std::nullopt;
    return It.get_integer_value();
  } else if constexpr (std::is_same_v<T, std::string_view>) {
    if (It.get_type() != sajson::TYPE_STRING)
      return std::nullopt;
    return It.as_view();
  }

  return std::nullopt;
}

} // namespace impl

std::optional<std::string_view> GetString(const sajson::value &Value,
                                          std::string_view Name) {
  return impl::GetValue<std::string_view>(Value, Name);
}
std::optional<int> GetInt(const sajson::value &Value, std::string_view Name) {
  return impl::GetValue<int>(Value, Name);
}

// Parses a workspace json array.
std::list<lsp::Workspace> ParseWorkspace(const sajson::value &Value) {
  std::list<lsp::Workspace> workspaces;
  for (std::size_t I = 0; I < Value.get_length(); ++I) {
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
std::optional<lsp::Location> ParseLocation(const sajson::value &Value) {
  auto Line = GetInt(Value, "line");
  if (!Line)
    return std::nullopt;
  auto Column = GetInt(Value, "character");
  if (!Column)
    return std::nullopt;
  return lsp::Location{(unsigned)*Line, (unsigned)*Column};
}

std::string RangeToJson(const lsp::Location &Start, const lsp::Location &End) {
  return fmt::format(R"(
{{
    "start": {{ "line": {}, "character": {} }},
    "end": {{ "line": {}, "character": {} }}
}})",
                     Start.Line, Start.Column, End.Line, End.Column);
}

constexpr int ParseError = -32700;
constexpr int InvalidRequest = -32600;
constexpr int MethodNotFound = -32601;
} // namespace

void lsp::Server::HandleNotification(std::string Message) {
  Log(">> Client sent:\n{}'\n", Message);

  auto Doc = sajson::parse(
      sajson::dynamic_allocation(),
      sajson::mutable_string_view(Message.size(), Message.data()));
  if (!Doc.is_valid())
    return SendError(ParseError, "Invalid JSON");
  const auto &Root = Doc.get_root();
  if (Root.get_type() != sajson::TYPE_OBJECT)
    return SendError(ParseError, "JSON document not an object");

  // We expect a valid JSON-RPC object.
  auto Version = GetString(Root, "jsonrpc");
  if (Version != "2.0")
    return SendError(InvalidRequest, "Unknown JSON RPC version");

  auto IdVal = Root.get_value_of_key("id"sv);
  // Requests without an "id" are in fact a notification.

  auto Method = GetString(Root, "method");
  if (!Method)
    return SendError(ParseError, "Error parsing 'method' member", IdVal);

  auto ParamsVal = Root.get_value_of_key("params"sv);
  // "params" is optional.

  Log(">> Received method '{}'.\n", *Method);

  // Now that we have parsed the JSON RPC object, handle the given request.
  if (*Method == "initialize")
    return Initialize(IdVal, ParamsVal);
  if (*Method == "initialized") {
    // All good! If we loaded workspaces, we can now begin to publish any
    // diagnostics.
    if (false && !Workspaces.empty())
      for (const auto &File : Files)
        UpdateDiagnosticsFor(File.first, File.second);
    return;
  }

  // The "initialize" request needs to be called before all other requests.
  if (!Initialized)
    return SendError(-32002, "Did not call 'initialize'", IdVal);
  if (*Method == "shutdown") {
    Shutdown = true;
    Log(">> 'shutdown' done!\n");
    // Workaround for buggy clients (like the VScode one)
    // that don't send an 'exit' notification sometimes.
    State = ServerState::ExitSuccess;
    return SendResult(IdVal, "null");
  }
  if (*Method == "exit") {
    State = Shutdown ? ServerState::ExitSuccess : ServerState::ExitError;
    Log(">> 'exit' done!\n");
    return;
  }
  if (Shutdown)
    return SendError(InvalidRequest, "Server is shutting down", IdVal);

    // Execute the given function call in a separate thread.
#define ASYNC(F)                                                               \
  Pool.Push([this, IdVal, ParamsVal, Str = std::move(Message),                 \
             D = std::move(Doc)] { return F; })

  // TODO: What are workspace symbols?
  // TODO: Implement file watch operations.
  if (*Method == "workspace/didChangeWorkspaceFolders")
    return ASYNC(DidChangeWorkspaceFolders(IdVal, ParamsVal));
  if (*Method == "workspace/didChangeConfiguration")
    // We don't have any configurations yet.
    return;
  if (*Method == "textDocument/didOpen")
    return ASYNC(DidOpen(IdVal, ParamsVal));
  if (*Method == "textDocument/didChange")
    return ASYNC(DidChange(IdVal, ParamsVal));
  if (*Method == "textDocument/didClose")
    return ASYNC(DidClose(IdVal, ParamsVal));
  if (*Method == "textDocument/completion")
    return ASYNC(Completion(IdVal, ParamsVal));
  if (*Method == "textDocument/hover")
    return ASYNC(Hover(IdVal, ParamsVal));
  // TODO: 'textDocument/signatureHelp'
  if (*Method == "textDocument/declaration" ||
      *Method == "textDocument/definition" ||
      *Method == "textDocument/implementation")
    return ASYNC(Goto(IdVal, ParamsVal));
  // TODO: 'textDocument/references'
  // TODO: 'textDocument/documentHighlight'
  // TODO: 'textDocument/documentSymbol'
  // TODO: 'textDocument/documentColor'
  // TODO: 'textDocument/colorPresentation'
  // TODO: 'textDocument/rename'
  // TODO: 'textDocument/foldingRange'
  if (*Method == "textDocument/semanticTokens/full")
    return ASYNC(SemanticTokensFull(IdVal, ParamsVal));
  if (*Method == "textDocument/semanticTokens/full/delta")
    return ASYNC(SemanticTokensRange(IdVal, ParamsVal));
  if (*Method == "textDocument/semanticTokens/range")
    return ASYNC(SemanticTokensRange(IdVal, ParamsVal));
    // TODO: 'textDocument/linkedEditingRange'
#undef ASYNC

  // Unknown method.
  LogError(">> Unknown method '{}'.\n", *Method);
  return SendError(MethodNotFound, "Method not found", IdVal);
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
    File.CachedNodes = LoadFromFile(Path);
  }
}

auto lsp::Server::GetState() const -> ServerState { return State; }

void lsp::Server::SendError(int Error, std::string_view Message,
                            const sajson::value &Id) {
  std::string IdStr;
  if (Id.get_type() == sajson::TYPE_STRING)
    IdStr = "\""s + Id.as_cstring() + "\"";
  else if (Id.get_type() == sajson::TYPE_INTEGER)
    IdStr = fmt::format("{}", Id.get_integer_value());
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

void lsp::Server::SendResult(const sajson::value &Id, std::string_view Result) {
  std::string IdStr;
  if (Id.get_type() == sajson::TYPE_STRING)
    IdStr = "\""s + Id.as_cstring() + "\"";
  else
    IdStr = fmt::format("{}", Id.get_integer_value());

  auto Message = fmt::format(R"(
{{
    "jsonrpc": "2.0",
    "id": {},
    "result": {}
}})",
                             IdStr, Result);

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

void lsp::Server::Initialize(const sajson::value &Id,
                             const sajson::value &Value) {
  Log(">> Processing 'initialize'.\n");
  if (Initialized)
    return SendError(InvalidRequest, "Server already initialized", Id);

  // Most fields here are not needed.
  auto It = Value.get_value_of_key("workspaceFolders"sv);
  if (It.get_type() == sajson::TYPE_ARRAY) {
    Workspaces = ParseWorkspace(It);
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
          "textDocumentSync": {{ "openClose": true, "change": 1 }},
          "workspace": {{ "workspaceFolders": {{ "supported": true, "changeNotifications": true }} }},
          "completionProvider": {{}},
          "hoverProvider": {{}},
          "semanticTokensProvider": {{ "legend": {{ "tokenTypes": [{}], "tokenModifiers": [{}] }},
              "full": true }},
          "declarationProvider": {{}},
          "definitionProvider": {{}},
          "implementationProvider": {{}}
    }},
    "serverInfo": {{ "name": "es-lsp", "version": "v1.0" }}
}})",
                             TokenTypes, TokenModifiers));
  Log(">> 'initialize' done!\n");
}

bool lsp::Server::DidChangeWorkspaceFolders(const sajson::value &Id,
                                            const sajson::value &Value) {
  Log(">> Processing 'workspace/didChangeWorkspaceFolders'.\n");

  auto Event = Value.get_value_of_key("event"sv);
  if (Event.get_type() != sajson::TYPE_OBJECT) {
    SendError(ParseError, "Couldn't parse 'event' field", Id);
    return true;
  }
  auto Added = Event.get_value_of_key("added"sv);
  if (Added.get_type() != sajson::TYPE_ARRAY) {
    SendError(ParseError, "Error parsing 'added' field", Id);
    return true;
  }
  auto Removed = Event.get_value_of_key("removed"sv);
  if (Removed.get_type() != sajson::TYPE_ARRAY) {
    SendError(ParseError, "Error parsing 'removed' field", Id);
    return true;
  }

  // Now actually change definitions based on the workspace change.
  for (std::size_t I = 0; I < Removed.get_length(); ++I) {
    const auto &Remove = Removed[I];
    if (Remove.get_type() != sajson::TYPE_OBJECT) {
      SendError(ParseError, "Array element isn't an object", Id);
      return true;
    }
    auto Path = GetString(Remove, "uri");
    if (!Path) {
      SendError(ParseError, "Error parsing 'uri' field", Id);
      return true;
    }

    auto WorkspaceIt = std::find_if(
        Workspaces.begin(), Workspaces.end(),
        [&Path](const auto &Workspace) { return Workspace.Path == *Path; });
    if (WorkspaceIt == Workspaces.end()) {
      SendError(InvalidRequest, "Specified workspace doesn't exist", Id);
      return true;
    }

    // Remove every file that belongs to the workspace.
    for (auto It = Files.begin(); It != Files.end();)
      if (It->second.Parent == &*WorkspaceIt)
        It = Files.erase(It);
      else
        ++It;
    Workspaces.erase(WorkspaceIt);
  }
  for (std::size_t I = 0; I < Added.get_length(); ++I) {
    const auto &Add = Added[I];
    if (Add.get_type() != sajson::TYPE_OBJECT) {
      SendError(ParseError, "Array element isn't an object", Id);
      return true;
    }
    auto Path = GetString(Add, "uri");
    if (!Path) {
      SendError(ParseError, "Error parsing 'uri' field", Id);
      return true;
    }
    auto Name = GetString(Add, "name");
    if (!Name) {
      SendError(ParseError, "Error parsing 'name' field", Id);
      return true;
    }

    Workspaces.emplace_back();
    Workspaces.back().Name = *Name;
    Workspaces.back().Path = UriToFsPath(*Path);
    LoadFromWorkspace(Workspaces.back());
  }
  Log(">> 'workspace/didChangeWorkspaceFolders' done!\n");
  return true;
}

bool lsp::Server::DidOpen(const sajson::value &Id, const sajson::value &Value) {
  Log(">> Processing 'textDocument/didOpen'.\n");

  auto TextDocument = Value.get_value_of_key("textDocument"sv);
  if (TextDocument.get_type() != sajson::TYPE_OBJECT) {
    SendError(ParseError, "Error parsing 'textDocument' field", Id);
    return true;
  }
  auto Path = GetString(TextDocument, "uri");
  if (!Path) {
    SendError(ParseError, "Error parsing 'uri' field", Id);
    return true;
  }
  auto Version = GetInt(TextDocument, "version");
  if (!Version) {
    SendError(ParseError, "Error parsing 'version' field", Id);
    return true;
  }
  auto Text = GetString(TextDocument, "text");
  if (!Text) {
    SendError(ParseError, "Error parsing 'text' field", Id);
    return true;
  }

  auto FsPath = UriToFsPath(*Path);

  std::shared_lock SharedLock(Mutex);
  std::unique_lock Lock(Mutex, std::defer_lock);
  auto It = Files.find(FsPath);
  if (It != Files.end()) {
    UpdateDiagnosticsFor(FsPath,
                         It->second); // The files is already loaded, normally
                                      // because it is part of a workspace.
    SharedLock.unlock();
    Lock.lock();
  } else {
    SharedLock.unlock();
    auto Nodes = LoadFromText(*Path, *Text);

    Lock.lock();

    // If the file doesn't exist we load it.
    It = Files.emplace(FsPath, File()).first;
    It->second.Path = FsPath;
    It->second.CachedNodes = std::move(Nodes);
    UpdateDiagnosticsFor(*Path, It->second);
  }
  It->second.Content = TextToLines(*Text);
  It->second.Version = *Version;
  It->second.IsOpen = true;

  Log(">> 'textDocument/didOpen' done!\n");
  return true;
}

bool lsp::Server::DidChange(const sajson::value &Id,
                            const sajson::value &Value) {
  Log(">> Processing 'textDocument/didChange'.\n");

  auto TextDocument = Value.get_value_of_key("textDocument"sv);
  if (TextDocument.get_type() != sajson::TYPE_OBJECT) {
    SendError(ParseError, "Error parsing 'textDocument' field", Id);
    return true;
  }
  auto Path = GetString(TextDocument, "uri");
  if (!Path) {
    SendError(ParseError, "Error parsing 'uri' field", Id);
    return true;
  }
  auto Version = GetInt(TextDocument, "version");
  if (!Version) {
    SendError(ParseError, "Error parsing 'version' field", Id);
    return true;
  }
  auto Changes = Value.get_value_of_key("contentChanges"sv);
  if (Changes.get_type() != sajson::TYPE_ARRAY) {
    SendError(ParseError, "Error parsing 'contentChanges' field", Id);
    return true;
  }

  auto FsPath = UriToFsPath(*Path);

  std::shared_lock SharedLock(Mutex);
  auto It = Files.find(FsPath);
  if (It == Files.end())
    return false;

  auto &File = It->second;
  if (!File.IsOpen)
    return false;

  // If the version is older than what the server has, don't do anything.
  if (File.Version > *Version)
    return true;
  SharedLock.unlock();

  for (std::size_t I = 0; I < Changes.get_length(); ++I) {
    const auto &Change = Changes[I];
    Location Start;
    Location End;

    auto Range = Change.get_value_of_key("range"sv);
    if (Range.get_type() != sajson::TYPE_NULL) {
      if (Range.get_type() != sajson::TYPE_OBJECT) {
        SendError(ParseError, "Error parsing 'range' field", Id);
        return true;
      }
      auto StartVal = Range.get_value_of_key("start"sv);
      if (StartVal.get_type() != sajson::TYPE_OBJECT) {
        SendError(ParseError, "Error parsing 'start' field", Id);
        return true;
      }
      auto EndVal = Range.get_value_of_key("end"sv);
      if (EndVal.get_type() != sajson::TYPE_OBJECT) {
        SendError(ParseError, "Error parsing 'end' field", Id);
        return true;
      }

      if (auto StartPos = ParseLocation(StartVal))
        Start = *StartPos;
      if (auto EndPos = ParseLocation(EndVal))
        End = *EndPos;
    }

    auto Text = GetString(Change, "text");
    if (!Text) {
      SendError(ParseError, "Error parsing 'text' field", Id);
      return true;
    }

    // Now actually update our copy of the file and recalculate every
    // diagnostic.
    std::vector<std::string> NewText;
    if (!Start && !End)
      // The whole file changed.
      NewText = TextToLines(*Text);
    else {
      // FIXME: Only a subset changed. Find out where exactly the change
      // occurred.
      assert(!"not implemented! why is the client sending this?");
    }

    auto Nodes = LoadFromText(*Path, *Text);

    {
      std::lock_guard Guard(Mutex);
      File.Version = *Version;
      File.Content = std::move(NewText);
      File.CachedNodes = std::move(Nodes);
    }

    SharedLock.lock();
    UpdateDiagnosticsFor(FsPath, File);
    SharedLock.unlock();
  }

  Log(">> 'textDocument/didChange' done!\n");
  return true;
}

bool lsp::Server::DidClose(const sajson::value &Id,
                           const sajson::value &Value) {
  Log(">> Processing 'textDocument/didClose'.\n");

  auto TextDocument = Value.get_value_of_key("textDocument"sv);
  if (TextDocument.get_type() != sajson::TYPE_OBJECT) {
    SendError(ParseError, "Error parsing 'textDocument' field", Id);
    return true;
  }
  auto Path = GetString(TextDocument, "uri");
  if (!Path) {
    SendError(ParseError, "Error parsing 'uri' field", Id);
    return true;
  }

  auto FsPath = UriToFsPath(*Path);
  std::shared_lock SharedLock(Mutex);
  auto It = Files.find(FsPath);
  if (It == Files.end() || !It->second.IsOpen)
    return false;
  SharedLock.unlock();
  std::lock_guard Guard(Mutex);
  It->second.IsOpen = false;
  It->second.Content.clear();

  Log(">> 'textDocument/didClose' done!\n");
  return true;
}

bool lsp::Server::Completion(const sajson::value &Id,
                             const sajson::value &Value) {
  Log(">> Processing 'textDocument/completion'.\n");

  auto TextDocument = Value.get_value_of_key("textDocument"sv);
  if (TextDocument.get_type() != sajson::TYPE_OBJECT) {
    SendError(ParseError, "Error parsing 'textDocument' field", Id);
    return true;
  }
  auto Path = GetString(TextDocument, "uri");
  if (!Path) {
    SendError(ParseError, "Error parsing 'uri' field", Id);
    return true;
  }
  auto PositionField = Value.get_value_of_key("position"sv);
  if (PositionField.get_type() != sajson::TYPE_OBJECT) {
    SendError(ParseError, "Error parsing 'position' field", Id);
    return true;
  }

  auto FsPath = UriToFsPath(*Path);
  auto Position = ParseLocation(PositionField);
  if (!Position) {
    SendError(ParseError, "Error parsing inner 'position' field", Id);
    return true;
  }

  std::shared_lock SharedLock(Mutex);
  // Now we need to figure out what the autocomplete list is.
  auto FileIt = Files.find(FsPath);
  if (FileIt == Files.end())
    return false;

  auto It = FileIt->second.CachedNodes.Nodes.find(Position->Line);
  if (It == FileIt->second.CachedNodes.Nodes.end()) {
    // We are somewhere without a node definition so figure out on what indent
    // level the client is.
    if (Position->Line >= FileIt->second.Content.size()) {
      SendError(InvalidRequest, "line too big", Id);
      return true;
    }

    std::string_view CurrentLine = FileIt->second.Content[Position->Line];
    const auto Indent =
        CountLineIndentation(CurrentLine, /*AllowEmptyLines=*/true);

    // Calculate line of the parent node.
    std::size_t ParentLine = -1;
    for (auto I = Position->Line - 1; I >= 0; --I) {
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
    } else if (ParentLine == (std::size_t)-1) {
      SendResult(Id, "[]");
      return true;
    } else {
      const auto &ChildrenIt =
          FileIt->second.CachedNodes.Nodes.find(ParentLine);
      if (ChildrenIt == FileIt->second.CachedNodes.Nodes.end()) {
        SendResult(Id, "[]");
        return true;
      }
      if (!ChildrenIt->second.Definition) {
        SendResult(Id, "[]");
        return true;
      }

      Candidates.reserve(ChildrenIt->second.Definition->Children.size());
      for (const auto &Child : ChildrenIt->second.Definition->Children)
        Candidates.emplace_back(Child.Name);
    }

    SharedLock.unlock();

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
    SendResult(Id, fmt::format("[{}]", Array));
    return true;
  }

  const auto &Node = It->second;
  // Nice, we found the line. Now check where exactly the client wants to
  // autocomplete.

  // Figure out what parameter we are autocompleting.
  std::size_t Index = -1;
  for (std::size_t I = 0; I < Node.Parameters.size(); ++I) {
    bool IsQuoted = Node.Quoted[I];
    if (Position->Column >= Node.Columns[I] - IsQuoted &&
        Position->Column <=
            Node.Columns[I] + Node.Parameters[I].size() + 2 * IsQuoted) {
      Index = I;
      break;
    }
  }
  if (Index == (std::size_t)-1) {
    SendResult(Id, "[]");
    return true;
  }

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
    if (!Node.Definition) {
      // Not much we can autocomplete if the node is invalid.
      SendResult(Id, "[]");
      return true;
    }
    if (Index - 1 >= Node.Definition->ParameterTypes.size()) {
      // Not much to do if there the argument is out of range.
      SendResult(Id, "[]");
      return true;
    }

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
    if (Position->Column == Node.Columns[Index] - Node.Quoted[Index])
      AddStartQuote = true;
    if (Position->Column == Node.Columns[Index] +
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
  return true;
}

bool lsp::Server::Hover(const sajson::value &Id, const sajson::value &Value) {
  Log(">> Processing 'textDocument/hover'.\n");

  auto TextDocument = Value.get_value_of_key("textDocument"sv);
  if (TextDocument.get_type() != sajson::TYPE_OBJECT) {
    SendError(ParseError, "Error parsing 'textDocument' field", Id);
    return true;
  }
  auto Path = GetString(TextDocument, "uri");
  if (!Path) {
    SendError(ParseError, "Error parsing 'uri' field", Id);
    return true;
  }
  auto PositionField = Value.get_value_of_key("position"sv);
  if (PositionField.get_type() != sajson::TYPE_OBJECT) {
    SendError(ParseError, "Error parsing 'position' field", Id);
    return true;
  }

  auto FsPath = UriToFsPath(*Path);
  auto Position = ParseLocation(PositionField);
  if (!Position) {
    SendError(ParseError, "Error parsing inner 'position' field", Id);
    return true;
  }

  std::shared_lock SharedLock(Mutex);
  auto FileIt = Files.find(FsPath);
  if (FileIt == Files.end())
    return false;

  auto It = FileIt->second.CachedNodes.Nodes.find(Position->Line);
  if (It == FileIt->second.CachedNodes.Nodes.end()) {
    // Nothing to hover.
    SendResult(Id, "null");
    return true;
  }

  const auto *Def = It->second.Definition;
  if (!Def) {
    SendResult(Id, "null");
    return true;
  }

  auto *ParentNode = It->second.Parent ? It->second.Parent : &It->second;
  while (ParentNode->Parent)
    ParentNode = ParentNode->Parent;
  // Now we just need to display the required docs to the client.
  auto TooltipIt = NodeTooltips.find(ParentNode->Parameters.front());
  if (TooltipIt == NodeTooltips.end()) {
    // Something went wrong, invalid nodes should have been caught above.
    SendResult(Id, "null");
    return true;
  }

  auto Tooltip = TooltipIt->second.find(It->second.Parameters.front());
  if (Tooltip == TooltipIt->second.end()) {
    // Something went wrong, the child of the root should exist at this point.
    SendResult(Id, "null");
    return true;
  }

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
  return true;
}

bool lsp::Server::SemanticTokensFull(const sajson::value &Id,
                                     const sajson::value &Value) {
  Log(">> Processing 'textDocument/semanticTokens/full'.\n");
  auto TextDocument = Value.get_value_of_key("textDocument"sv);
  if (TextDocument.get_type() != sajson::TYPE_OBJECT) {
    SendError(ParseError, "Error parsing 'textDocument' field", Id);
    return true;
  }
  auto Path = GetString(TextDocument, "uri");
  if (!Path) {
    SendError(ParseError, "Error parsing 'uri' field", Id);
    return true;
  }

  const auto FsPath = UriToFsPath(*Path);
  std::shared_lock SharedLock(Mutex);
  auto FileIt = Files.find(FsPath);
  if (FileIt == Files.end())
    return false;

  // Stores the list of completion tokens for the client. Every token has 5
  // entries in this array (so for N tokens this array has size 5 * N).
  //   - Line
  //   - Start Column
  //   - Length
  //   - Token Type
  //   - Token Modifier(s)
  std::vector<int> Tokens;

  // Conversative guess: Every line has a single token.
  Tokens.reserve(FileIt->second.Content.size());

  std::size_t LineIndex = -1;
  auto MarkToken = [&Tokens, &LineIndex, PrevLine = std::size_t(),
                    PrevStart = std::size_t()](
                       std::size_t Start, std::size_t Length, TokenTypes Type,
                       ModifierTypes Modifiers) mutable {
    Tokens.reserve(Tokens.size() + 5);

    // We need to perform a delta to the previous position.
    Tokens.push_back(LineIndex > PrevLine ? LineIndex - PrevLine : 0);
    Tokens.push_back(LineIndex == PrevLine ? Start - PrevStart : Start);

    Tokens.push_back(Length);
    Tokens.push_back((int)Type);
    Tokens.push_back((int)Modifiers);

    PrevStart = Start;
    PrevLine = LineIndex;
  };
  const auto MarkAsComment = [&MarkToken](std::size_t Start, std::size_t End) {
    MarkToken(Start, End - Start, TokenTypes::Comment, ModifierTypes::None);
  };

  // We go through every single line of the file and generate the required
  // tokens.
  for (const auto &Line : FileIt->second.Content) {
    ++LineIndex;

    auto First = Line.find_first_not_of(" \f\t\v\r\n");
    if (First == std::string::npos)
      // This line is completely empty.
      continue;
    if (Line[First] == '#') {
      MarkAsComment(First, Line.size());
      continue;
    }

    auto NodeIt = FileIt->second.CachedNodes.Nodes.find(LineIndex);
    if (NodeIt != FileIt->second.CachedNodes.Nodes.end()) {
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
  SharedLock.unlock();

  if (Tokens.empty()) {
    SendResult(Id, "[]");
    return true;
  }

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

  std::string_view View(Array.data(), It - Array.data() - 1);
  SendResult(Id, fmt::format(R"({{ "data": [{}] }})", View));
  Log(">> 'textDocument/semanticTokens/full' done!\n");
  return true;
}

bool lsp::Server::SemanticTokensDelta(const sajson::value &Id,
                                      const sajson::value &Value) {
  return true;
}

bool lsp::Server::SemanticTokensRange(const sajson::value &Id,
                                      const sajson::value &Value) {
  return true;
}

bool lsp::Server::Goto(const sajson::value &Id, const sajson::value &Value) {
  Log(">> Processing 'textDocument/{{declaration,definition}}'.\n");

  auto TextDocument = Value.get_value_of_key("textDocument"sv);
  if (TextDocument.get_type() != sajson::TYPE_OBJECT) {
    SendError(ParseError, "Error parsing 'textDocument' field", Id);
    return true;
  }
  auto Path = GetString(TextDocument, "uri");
  if (!Path) {
    SendError(ParseError, "Error parsing 'uri' field", Id);
    return true;
  }
  auto PositionField = Value.get_value_of_key("position"sv);
  if (PositionField.get_type() != sajson::TYPE_OBJECT) {
    SendError(ParseError, "Error parsing 'position' field", Id);
    return true;
  }

  auto FsPath = UriToFsPath(*Path);
  auto Position = ParseLocation(PositionField);
  if (!Position) {
    SendError(ParseError, "Error parsing inner 'position' field", Id);
    return true;
  }

  std::shared_lock SharedLock(Mutex);
  auto FileIt = Files.find(FsPath);
  if (FileIt == Files.end())
    return false;

  auto It = FileIt->second.CachedNodes.Nodes.find(Position->Line);
  if (It == FileIt->second.CachedNodes.Nodes.end()) {
    SendResult(Id, "null");
    return true;
  }
  const auto &Node = It->second;

  // Figure out what parameter we are autocompleting.
  // TODO: Tons of code duplication here and elsewhere.
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
  if (Index == (std::size_t)-1) {
    SendResult(Id, "null");
    return true;
  }

  if (!Node.Definition || !Index) {
    // If the node is invalid or it is the root (roots don't have a type), then
    // there is nothing to do.
    SendResult(Id, "null");
    return true;
  }

  auto Annotation = Node.Definition->ParameterTypes[Index - 1].Annotation;
  // Now that we have an index we can inspect it. Goto on anything but entities
  // doesn't make any sense.
  if (Annotation.empty()) {
    SendResult(Id, "null");
    return true;
  }

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
  if (Array.empty()) {
    SendResult(Id, "[]");
    return true;
  }
  Array.pop_back();

  SendResult(Id, fmt::format("[{}]", Array));
  Log(">> 'textDocument/{{declaration,definition}}' done!\n");
  return true;
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
    {
      std::shared_lock SharedLock(Mutex);
      auto It = Files.find(Path);
      if (It != Files.end()) {
        SharedLock.unlock();
        // The file is already loaded, but we still need to update its
        // workspace.
        {
          std::lock_guard Guard(Mutex);
          It->second.Parent = &Workspace;
        }
        continue;
      }
    }

    // We need write access here.
    auto Nodes = LoadFromFile(Path);
    {
      std::lock_guard Guard(Mutex);
      auto &File = Files[Path];
      File.Path = Path;
      File.Parent = &Workspace;
      File.CachedNodes = std::move(Nodes);
    }
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
