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
  // This includes the <name> and the <parameters>.
  std::vector<std::string> Parameters;
  // Any children of this data node.
  std::vector<DataNode> Children;
  // The parent, if any.
  const DataNode *Parent;

  // The line in the file where this data node is located.
  std::size_t Line;
  // Each column corresponds to the column location of a parameter.
  std::vector<std::size_t> Columns;

  // Converts the data node to string.
  std::string ToString() const;
};

// Represents a diagnostic for a DataNode.
struct Diagnostic {
  Diagnostic(const DataNode &Node, std::size_t ParamIndex);

  // The line the diagnostic occurred. Multiline diagnostics are not a thing for
  // ES.
  std::size_t Line;
  // The starting column. This doesn't include the starting quote.
  std::size_t Column;
  // The end column of the diagnostic. This doesn't include the ending quote.
  std::size_t EndColumn;
  // What kind of diagnostic this is.
  enum { Error = 1, Warning, Information, Hint } Kind{};
  // The diagnostic message to show to the user.
  std::string Message;
};

// Stores all of the DataNodes of a file as well as related data.
struct RootDataNode final {
  // The data inside this file.
  std::vector<DataNode> Nodes;
  // Any diagnostics are stored here.
  std::vector<Diagnostic> Diagnostics;

  // The file path of this node definition.
  std::string Path;

  // Converts the data nodes to string.
  std::string ToString() const;
};

// Loads and parses the file from disk.
RootDataNode LoadFromFile(const fs::path &Path);
// Loads and parses the given file but its contents are given.
RootDataNode LoadFromText(std::string_view Path, std::string_view Text);

} // namespace lsp

#endif
