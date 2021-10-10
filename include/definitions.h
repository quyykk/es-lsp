#ifndef DEFINITIONS_H
#define DEFINITIONS_H

#include <optional>
#include <set>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace lsp {

// The "type" of a parameter from a data node. This is used for semantic
// analysis.
struct Type {
  // What kind of base type this is.
  enum {
    Double,
    Int,
    String,
  } Kind;

  // Type annotation if this type is a string, for example 'ship' or 'outfit'.
  std::string_view Annotation;
  // If this type is a (string) keyword, the possible keywords are listed here.
  std::set<std::string_view> Keywords;

  // Converts a string to a type.
  static Type FromString(std::string_view String) noexcept;

  // Verifies if the given type can be converted to this type.
  bool VerifyType(const Type &Other, std::string_view Contents) const noexcept;

  operator std::string_view() const noexcept;

  friend constexpr bool operator==(const Type &Lhs, const Type &Rhs) noexcept;
  friend constexpr bool operator!=(const Type &Lhs, const Type &Rhs) noexcept;
};

constexpr bool operator==(const Type &Lhs, const Type &Rhs) noexcept {
  return Lhs.Kind == Rhs.Kind;
}
constexpr bool operator!=(const Type &Lhs, const Type &Rhs) noexcept {
  return !(Lhs == Rhs);
}

// Stores the format of a given node for parser and semantic analysis.
struct NodeDefinition {
  // The root name of this node.
  std::string_view Name;
  // The type of this node. This applies only if there is no name.
  Type BaseType;

  // The types of the parameters of this node.
  std::vector<Type> ParameterTypes;
  // Stores the index where optional parameters begin.
  int OptionalIndex = -1;
  // Stores the index where variable parameters begin, i.e. an unlimited
  // number of parameters.
  int VariableIndex = -1;

  // The children definitions for this node.
  std::vector<NodeDefinition> Children;
  // The amount of children this node at least needs.
  int RequiredChildren = 0;
  // The child this index refers to is allowed to be specified an infinite
  // amount of times.
  int VariableChildIndex = -1;
};

extern const std::unordered_map<std::string_view, lsp::NodeDefinition>
    Definitions;
extern const std::unordered_map<
    std::string_view, std::unordered_map<std::string_view, std::string_view>>
    NodeTooltips;

struct DataNode;
struct Diagnostic;

void CheckLine(std::vector<Diagnostic> &Diagnostics,
               const std::vector<DataNode *> &Nodes);

} // namespace lsp

#endif
