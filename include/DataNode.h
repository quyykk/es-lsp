#ifndef DATANODE_H
#define DATANODE_H

#include <filesystem>
#include <string>
#include <string_view>
#include <vector>

namespace fs = std::filesystem;

namespace lsp {

// A simple struct representing the syntax for objects:
//
// <name> <parameters>...
//     <children>
//     ...
struct DataNode final {
  std::vector<std::string> Parameters;
  std::vector<DataNode> Children;
  const DataNode *Parent;

  std::size_t Line;
  // The column information about the parameters.
  std::vector<std::size_t> Columns;

  std::string ToString() const;
};

// Represents a diagnostic for a DataNode.
struct Diagnostic {
  Diagnostic(const DataNode &Node, std::size_t ParamIndex);

  std::size_t Line;
  std::size_t Column;
  std::size_t EndColumn;
  // What kind of diagnostic this is.
  enum { Error = 1, Warning, Information, Hint } Kind{};
  // The diagnostic message to show to the user.
  std::string Message;
};

struct RootDataNode final {
  std::vector<DataNode> Nodes;
  std::vector<Diagnostic> Diagnostics;

  // The file path of this node definition.
  std::string Path;

  std::string ToString() const;
};

RootDataNode LoadFromFile(const fs::path &Path);
RootDataNode LoadFromText(std::string_view Path, std::string_view Text);

} // namespace lsp

#endif
