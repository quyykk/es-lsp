#ifndef LSP_H
#define LSP_H

#include "DataNode.h"

#include <string>
#include <unordered_map>
#include <vector>

#include "rapidjson/document.h"

namespace json = rapidjson;

namespace lsp {

struct Workspace {
  std::string Name;
  std::string Uri;
};

struct File {
  RootDataNode CachedNodes;
  std::string Content;
  int Version;
};

enum class ServerState {
  Running,
  ExitSuccess,
  ExitError,
};

struct Location {
  unsigned Line = 0;
  unsigned Column = 0;

  constexpr operator bool() const noexcept { return Line || Column; }
};

class Server final {
public:
  void HandleNotification(std::string Message);
  void SetResourceNodes(std::vector<RootDataNode> Nodes);

  ServerState State() const;

private:
  void SendError(int Error, std::string_view Message, const json::Value &Id = json::Value());
  void SendResult(const json::Value &Id, std::string_view Result);
  void SendNotification(std::string_view Method, std::string_view Params);

  void Initialize(const json::Value &Id, const json::Value &Value);
  void DidChangeWorkspaceFolders(const json::Value &Id, const json::Value &Value);
  void DidOpen(const json::Value &Id, const json::Value &Value);
  void DidChange(const json::Value &Id, const json::Value &Value);
  void DidClose(const json::Value &Id, const json::Value &Value);

  void UpdateDiagnosticsFor(std::string_view Uri, const File &File);

private:
  bool Initialized = false;
  bool Shutdown = false;
  ServerState Exit = ServerState::Running;
  std::vector<Workspace> Workspaces;

  // The data loaded.
  std::vector<RootDataNode> ResourceNodes;
  std::vector<RootDataNode> Nodes;
  std::unordered_map<std::string, File> Files;
};

} // namespace lsp

#endif
