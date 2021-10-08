#ifndef DEFINITIONS_H
#define DEFINITIONS_H

#include <optional>
#include <set>
#include <string_view>
#include <vector>

namespace lsp {

struct Type {
  enum TypeKind { Keyword, Double, String, OptionalDouble, OptionalString } Kind;

  Type(TypeKind Kind) noexcept : Kind(Kind) {}
  explicit Type(std::string_view String) noexcept;

  operator std::string_view() const noexcept;

  friend constexpr bool operator==(const Type &Lhs, const Type &Rhs) noexcept;
  friend constexpr bool operator!=(const Type &Lhs, const Type &Rhs) noexcept;
};

constexpr bool operator==(const Type &Lhs, const Type &Rhs) noexcept {
  return (Lhs.Kind == Type::String && Rhs.Kind == Type::OptionalString)
    || (Lhs.Kind == Type::OptionalString && Rhs.Kind == Type::String)
    || (Lhs.Kind == Type::Double && Rhs.Kind == Type::OptionalDouble)
    || (Lhs.Kind == Type::OptionalDouble && Rhs.Kind == Type::Double)
    || Lhs.Kind == Rhs.Kind;
}

constexpr bool operator!=(const Type &Lhs, const Type &Rhs) noexcept {
  return !(Lhs == Rhs);
}

bool IsValidRootNode(std::string_view Name);
const std::set<std::string_view> &GetChildrenOf(std::string_view Root);
const std::vector<Type> &GetParameterTypesOf(std::string_view First);

} // namespace lsp

#endif
