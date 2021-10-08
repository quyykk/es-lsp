#ifndef DEFINITIONS_H
#define DEFINITIONS_H

#include <optional>
#include <set>
#include <string_view>
#include <vector>

namespace lsp {

// The "type" of a parameter from a data node. This is used for semantic
// analysis.
struct Type {
  enum TypeKind {
    Keyword,

    Double,
    String,

    OptionalDouble,
    OptionalString,

    Effect,
    Fleet,
    Hazard,
    Outfit,
    Planet,
    Ship,
    Sprite,
    System,
  } Kind;

  constexpr Type(TypeKind Kind) noexcept : Kind(Kind) {}
  explicit Type(std::string_view String) noexcept;

  constexpr bool IsOptional() const noexcept {
    return Kind == OptionalDouble || Kind == OptionalString;
  }
  operator std::string_view() const noexcept;

  friend constexpr bool operator==(const Type &Lhs, const Type &Rhs) noexcept;
  friend constexpr bool operator!=(const Type &Lhs, const Type &Rhs) noexcept;
};

constexpr bool operator==(const Type &Lhs, const Type &Rhs) noexcept {
  return (Lhs.Kind == Type::String && Rhs.Kind == Type::OptionalString) ||
         (Lhs.Kind == Type::OptionalString && Rhs.Kind == Type::String) ||
         (Lhs.Kind == Type::Double && Rhs.Kind == Type::OptionalDouble) ||
         (Lhs.Kind == Type::OptionalDouble && Rhs.Kind == Type::Double) ||
         // FIXME: The next two should only be "active" when only a single file
         // is loaded. For projects we should diagnose missing definitions.
         (Lhs.Kind >= Type::Effect && Rhs.Kind == Type::String) ||
         (Lhs.Kind == Type::String && Rhs.Kind >= Type::Effect) ||
         Lhs.Kind == Rhs.Kind;
}

constexpr bool operator!=(const Type &Lhs, const Type &Rhs) noexcept {
  return !(Lhs == Rhs);
}

// Checks whether the give name is a valid root node.
bool IsValidRootNode(std::string_view Name);
// Returns all of the possible children of the given root node.
const std::set<std::string_view> &GetChildrenOf(std::string_view Root);
// Returns the type of the parameters of the given node. Doesn't have to be a
// root node.
const std::vector<Type> *GetParameterTypesOf(std::string_view First);

} // namespace lsp

#endif
