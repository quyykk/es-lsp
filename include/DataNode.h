#ifndef DATANODE_H
#define DATANODE_H

#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace lsp {

// A simple struct representing the syntax for objects:
//
// <name> <parameters>...
//     <children>
//     ...
// TODO: Add a 'name' field (maybe an index) so that we know which parameter is
// the actual name if this is a definition node.
struct DataNode final {
  // This includes the <name> and the <parameters>.
  std::vector<std::string> Parameters;
  // Whether any parameter is quoted inside the file.
  std::vector<bool> Quoted;
  // Any children of this data node.
  std::vector<DataNode *> Children;
  // The parent, if any.
  const DataNode *Parent;
  // The type definition of this node if it has been type checked.
  const struct NodeDefinition *Definition = nullptr;

  // The line in the file where this data node is located.
  std::size_t Line;
  // Each column corresponds to the column location of a parameter.
  std::vector<std::size_t> Columns;
  // The indent level of this datanode.
  std::size_t Indent;

  // Converts the data node to string.
  std::string ToString() const;
};

// Represents a diagnostic for a DataNode.
struct Diagnostic {
  Diagnostic() = default;
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

// Stores an entity definition, for example a defined 'outfit'.
struct Entity {
  // The DataNode corresponding to the root node for this entity.
  const DataNode *Node;
  // The last line of this entity. This is the last line of the last attribute.
  std::size_t LastLine;
};

// Stores all of the DataNodes of a file as well as related data.
struct RootDataNode final {
  // The data inside this file.
  std::unordered_map<int, DataNode> Nodes;
  // Any diagnostics are stored here.
  std::vector<Diagnostic> Diagnostics;
  // A map of entities of a given type, for examples outfits and systems.
  std::unordered_map<std::string_view, std::vector<Entity>> Entities;

  // The file path of this node definition.
  std::string Path;

  // Converts the data nodes to string.
  std::string ToString() const;
};

// Loads and parses the given file but its contents are given line by line.
RootDataNode LoadFromLines(std::string_view Path, std::span<std::string> Text);

} // namespace lsp

#endif
