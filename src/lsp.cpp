#include "lsp.h"

#include "fmt/core.h"
#include "log.h"

#include <algorithm>
#include <charconv>
#include <iostream>
#include <iterator>
#include <optional>
#include <string>
#include <string_view>

using namespace std::literals;

namespace {
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
  }

  return std::nullopt;
}

std::vector<lsp::Workspace> ParseWorkspace(const json::Value &Value) {
  std::vector<lsp::Workspace> workspaces;
  for (json::SizeType I = 0; I < Value.Size(); ++I) {
    auto Name = Value[I].FindMember("name");
    if (Name == Value.MemberEnd() || !Name->value.IsString())
      continue;
    auto Uri = Value[I].FindMember("uri");
    if (Uri == Value.MemberEnd() || !Uri->value.IsString())
      continue;
    workspaces.emplace_back();
    workspaces.back().Name = Name->value.GetString();
    workspaces.back().Uri = Uri->value.GetString();
  }
  return workspaces;
}

std::optional<lsp::Location> ParseLocation(const json::Value &Value) {
  auto Line = Value.FindMember("line");
  if (Line == Value.MemberEnd() || !Line->value.IsUint())
    return std::nullopt;
  auto Column = Value.FindMember("character");
  if (Column == Value.MemberEnd() || !Column->value.IsUint())
    return std::nullopt;
  return lsp::Location{Line->value.GetUint(), Column->value.GetUint()};
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
  auto Version = GetValue<std::string_view>(Doc, "jsonrpc");
  if (Version != "2.0")
    return SendError(InvalidRequest, "Unknown JSON RPC version");

  auto Id = Doc.FindMember("id");
  // Requests without an "id" are in fact a notification.

  auto Method = GetValue<std::string_view>(Doc, "method");
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
    Exit = ServerState::ExitSuccess;
    return SendResult(Id->value, "null");
  }
  if (*Method == "exit") {
    Exit = Shutdown ? ServerState::ExitSuccess : ServerState::ExitError;
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
  // TODO: 'textDocument/completion'
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

void lsp::Server::SetResourceNodes(std::vector<RootDataNode> Nodes) {
  ResourceNodes = std::move(Nodes);
}

auto lsp::Server::State() const -> ServerState { return Exit; }

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
    Nodes.clear();
    for (const auto &Workspace : Workspaces) {
      // Strip "file://" prefix from the uri.
      auto Uri = Workspace.Uri.substr(7);
      Nodes.emplace_back(LoadFromFile(Uri));
    }
  }

  // Tell the client that we are initialized.
  Initialized = true;
  SendResult(Id, R"(
{
    "capabilities":
    {
          "textDocumentSync": { "openClose": true, "change": 1 },
          "workspace": { "workspaceFolders": { "supported": true, "changeNotifications": true }}
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
    auto Path = Remove.FindMember("uri");
    if (Path == Remove.MemberEnd() || !Path->value.IsString())
      return SendError(ParseError, "Error parsing 'uri' field", Id);

    std::string_view PathStr = Path->value.GetString();
    auto It =
        std::find_if(Nodes.begin(), Nodes.end(), [&PathStr](const auto &Node) {
          return Node.Path == PathStr;
        });
    if (It != Nodes.end())
      Nodes.erase(It);
    auto WorkspaceIt = std::find_if(
        Workspaces.begin(), Workspaces.end(),
        [&PathStr](const auto &Workspace) { return Workspace.Uri == PathStr; });
    if (WorkspaceIt != Workspaces.end())
      Workspaces.erase(WorkspaceIt);
  }
  for (json::SizeType I = 0; I < Added->value.Size(); ++I) {
    const auto &Add = Added[I].value;
    if (!Add.IsObject())
      return SendError(ParseError, "Array element isn't an object", Id);
    auto Path = Add.FindMember("uri");
    if (Path == Add.MemberEnd() || !Path->value.IsString())
      return SendError(ParseError, "Error parsing 'uri' field", Id);
    auto Name = Add.FindMember("name");
    if (Name == Add.MemberEnd() || !Name->value.IsString())
      return SendError(ParseError, "Error parsing 'name' field", Id);

    Workspaces.emplace_back();
    Workspaces.back().Name = Name->value.GetString();
    Workspaces.back().Uri = Path->value.GetString();
    // Strip "file://" prefix from the uri.
    auto Uri = Workspaces.back().Uri.substr(7);
    Nodes.emplace_back(LoadFromFile(Uri));
  }
  Log(">> 'workspace/didChangeWorkspaceFolders' done!\n");
}

void lsp::Server::DidOpen(const json::Value &Id, const json::Value &Value) {
  Log(">> Processing 'textDocument/didOpen'.\n");

  auto TextDocument = Value.FindMember("textDocument");
  if (TextDocument == Value.MemberEnd() || !TextDocument->value.IsObject())
    return SendError(ParseError, "Error parsing 'textDocument' field", Id);
  auto Path = TextDocument->value.FindMember("uri");
  if (Path == TextDocument->value.MemberEnd() || !Path->value.IsString())
    return SendError(ParseError, "Error parsing 'uri' field", Id);
  auto Version = TextDocument->value.FindMember("version");
  if (Version == TextDocument->value.MemberEnd() || !Version->value.IsInt())
    return SendError(ParseError, "Error parsing 'version' field", Id);
  auto Text = TextDocument->value.FindMember("text");
  if (Text == TextDocument->value.MemberEnd() || !Text->value.IsString())
    return SendError(ParseError, "Error parsing 'text' field", Id);

  auto &File = Files[Path->value.GetString()];
  File.Content = Text->value.GetString();
  File.Version = Version->value.GetInt();
  File.CachedNodes = LoadFromText(Path->value.GetString(), File.Content);
  UpdateDiagnosticsFor(Path->value.GetString(), File);

  Log(">> 'textDocument/didOpen' done!\n");
}

void lsp::Server::DidChange(const json::Value &Id, const json::Value &Value) {
  Log(">> Processing 'textDocument/didChange'.\n");

  auto TextDocument = Value.FindMember("textDocument");
  if (TextDocument == Value.MemberEnd() || !TextDocument->value.IsObject())
    return SendError(ParseError, "Error parsing 'textDocument' field", Id);
  auto Path = TextDocument->value.FindMember("uri");
  if (Path == TextDocument->value.MemberEnd() || !Path->value.IsString())
    return SendError(ParseError, "Error parsing 'uri' field", Id);
  auto Version = TextDocument->value.FindMember("version");
  if (Version == TextDocument->value.MemberEnd() || !Version->value.IsInt())
    return SendError(ParseError, "Error parsing 'version' field", Id);
  auto Changes = Value.FindMember("contentChanges");
  if (Changes == Value.MemberEnd() || !Changes->value.IsArray())
    return SendError(ParseError, "Error parsing 'contentChanges' field", Id);

  auto It = Files.find(Path->value.GetString());
  if (It == Files.end())
    return SendError(InvalidRequest, "didChange called on unopened file", Id);
  auto &File = It->second;

  // If the version is old, don't do anything.
  if (File.Version > Version->value.GetInt())
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

    auto Text = Change.FindMember("text");
    if (Text == Change.MemberEnd() || !Text->value.IsString())
      return SendError(ParseError, "Error parsing 'text' field", Id);

    // Now actually update our copy of the file and recalculate every
    // diagnostic.
    File.Version = Version->value.GetInt();
    if (!Start && !End)
      // The whole file changed.
      File.Content = Text->value.GetString();
    else {
      // FIXME: Only a subset changed. Find out where exactly the change
      // occurred.
      assert(!"not implemented! why is the client sending this?");
    }
    File.CachedNodes = LoadFromText(Path->value.GetString(), File.Content);
    UpdateDiagnosticsFor(Path->value.GetString(), File);
  }

  Log(">> 'textDocument/didChange' done!\n");
}

void lsp::Server::DidClose(const json::Value &Id, const json::Value &Value) {
  Log(">> Processing 'textDocument/didClose'.\n");

  auto TextDocument = Value.FindMember("textDocument");
  if (TextDocument == Value.MemberEnd() || !TextDocument->value.IsObject())
    return SendError(ParseError, "Error parsing 'textDocument' field", Id);
  auto Path = TextDocument->value.FindMember("uri");
  if (Path == TextDocument->value.MemberEnd() || !Path->value.IsString())
    return SendError(ParseError, "Error parsing 'uri' field", Id);
  Files.erase(Path->value.GetString());

  Log(">> 'textDocument/didClose' done!\n");
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
