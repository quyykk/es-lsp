#ifndef LSP_H
#define LSP_H

#include "DataNode.h"

#include <list>
#include <string>
#include <unordered_map>
#include <vector>

#include "rapidjson/document.h"

namespace json = rapidjson;

namespace lsp {

// A workspace is a project, typically a directory.
struct Workspace {
  // The name of the workspace (user-defined).
  std::string Name;
  // The path to the workspace.
  std::string Path;
};

// Reprents a data file that is loaded.
struct File {
  // The file parsed into data nodes.
  RootDataNode CachedNodes;

  // The fs path to the file.
  std::string Path;

  // If the file is opened, this contains the contents of the file.
  std::string Content;

  // The version of the file for synchronization purposes with the client.
  int Version = -1;

  // Whether the file is currently opened in the client.
  bool IsOpen = false;

  // If this file belongs to a workspace, this will point to it.
  const Workspace *Parent = nullptr;
};

// The current state of the server.
enum class ServerState {
  Running,
  ExitSuccess,
  ExitError,
};

// A location inside a file.
struct Location {
  std::size_t Line = 0;
  std::size_t Column = 0;

  constexpr operator bool() const noexcept { return Line || Column; }
};

// The actual LSP server. It is reponsible for talking to the client.
class Server final {
public:
  // Parses and executes the given message from the client.
  void HandleNotification(std::string Message);
  // Loads a whole directory of data files from the given path.
  void LoadFromDirectory(std::string_view Path);

  // Returns the current state of the server.
  ServerState GetState() const;

private:
  // Functions used to talk to the client.
  void SendError(int Error, std::string_view Message,
                 const json::Value &Id = json::Value());
  void SendResult(const json::Value &Id, std::string_view Result);
  void SendNotification(std::string_view Method, std::string_view Params);

  // Callbacks called when the client send the appropriate request/notification.
  void Initialize(const json::Value &Id, const json::Value &Value);
  void DidChangeWorkspaceFolders(const json::Value &Id,
                                 const json::Value &Value);
  void DidOpen(const json::Value &Id, const json::Value &Value);
  void DidChange(const json::Value &Id, const json::Value &Value);
  void DidClose(const json::Value &Id, const json::Value &Value);
  void Completion(const json::Value &Id, const json::Value &Value);

  // Sends diagnostics for the given file to the client.
  void UpdateDiagnosticsFor(std::string_view Uri, const File &File);
  // Loads a whole workspace of data files from the given workspace.
  void LoadFromWorkspace(const Workspace &Workspace);

private:
  // Whether the server is initialized (i.e. received the 'initialize' request).
  bool Initialized = false;
  // Whether the server received the 'shutdown' request.
  bool Shutdown = false;
  // The current state of the server.
  ServerState State = ServerState::Running;
  // The list of loaded workspaces. The files inside those workspaces are
  // loaded.
  std::list<Workspace> Workspaces;

  // Every file loaded. This includes files not opened in the client.
  std::unordered_map<std::string, File> Files;
};

} // namespace lsp

#endif
