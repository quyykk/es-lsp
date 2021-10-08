#include "DataNode.h"

#include "definitions.h"
#include "fmt/core.h"

#include <cassert>
#include <fstream>

namespace {

std::string ConvertNode(const lsp::DataNode &Node, std::size_t Indent = 0) {
  std::string String(Indent, '\t');
  for (const auto &Param : Node.Parameters) {
    // Correctly quote the parameter if necessary.
    auto Space = Param.find(' ');
    auto Quote = Param.find('"');
    if (Quote != std::string::npos)
      String += '`';
    else if (Space != std::string::npos)
      String += '"';

    String += Param;

    if (Quote != std::string::npos)
      String += '`';
    else if (Space != std::string::npos)
      String += '"';
    String += ' ';
  }
  String.pop_back();
  String += '\n';

  for (const auto &Child : Node.Children)
    String += ConvertNode(Child, Indent + 1);
  return String;
}

} // namespace

lsp::Diagnostic::Diagnostic(const DataNode &Node, std::size_t ParamIndex) {
  Line = Node.Line;
  Column = Node.Columns[ParamIndex];
  EndColumn = Node.Columns[ParamIndex] + Node.Parameters[ParamIndex].size();
}

std::string lsp::DataNode::ToString() const { return ConvertNode(*this); }

std::string lsp::RootDataNode::ToString() const {
  std::string Str;
  for (const auto &Node : Nodes) {
    Str += Node.ToString();
    Str += '\n';
  }
  return Str;
}

auto lsp::LoadFromFile(const fs::path &Path) -> RootDataNode {
  std::ifstream File(Path, std::ios::binary | std::ios::ate);
  std::size_t Size = File.tellg();
  File.seekg(0, std::ios::beg);

  std::string Contents(Size, '\0');
  if (!File.read(Contents.data(), Size))
    return {};
  return LoadFromText(Path.c_str(), Contents);
}

auto lsp::LoadFromText(std::string_view Path, std::string_view Text)
    -> RootDataNode {
  RootDataNode Result;
  Result.Path = Path;

  std::vector<DataNode *> Previous;
  std::vector<std::size_t> Indents;

  std::size_t Line = -1;
  std::size_t Column;
  // Parse file contents into a DataNode.
  for (std::size_t I = 0; I < Text.size(); ++I) {
    ++Line;
    Column = 0;

    // Parse any indentation first.
    std::size_t Indent = 0;
    while (I < Text.size() && Text[I] <= ' ' && Text[I] != '\n') {
      ++Indent;
      ++I;
      ++Column;
    }

    // Check if this is an empty line.
    if (I == Text.size())
      break;
    if (Text[I] == '#')
      while (I < Text.size() && Text[I] != '\n')
        ++I;
    if (I == Text.size())
      break;
    if (Text[I] == '\n')
      continue;

    // If this line has no indentation, then it is a root node.
    // If it does, we need to check which node is its parent.
    DataNode *Node;
    if (!Indent) {
      Node = &Result.Nodes.emplace_back();
      Node->Parent = nullptr;
    } else {
      // Choose the correct parent node.
      while (Indents.back() >= Indent) {
        Indents.pop_back();
        Previous.pop_back();
        assert(!Indents.empty() && "weird indentation?");
      }
      Node = &Previous.back()->Children.emplace_back();
      Node->Parent = Previous.back();
    }
    Node->Line = Line;

    // Now we read any parameters.
    while (Text[I] != '\n' && Text[I] != '#') {
      // Parameters may be be quoted.
      bool Quoted = false;
      char Quote;
      if (Text[I] == '"' || Text[I] == '`') {
        Quoted = true;
        Quote = Text[I];
        ++I;
        ++Column;
      }

      auto &Param = Node->Parameters.emplace_back();
      Node->Columns.emplace_back(Column);
      while (I < Text.size() && Text[I] != '\n' &&
             (Quoted ? Text[I] != Quote : Text[I] > ' ')) {
        Param += Text[I++];
        ++Column;
      }

      if (I == Text.size())
        break;

      // Eat last quote.
      if (Quoted) {
        ++I;
        ++Column;
      }

      // Eat any whitespace.
      while (I < Text.size() && Text[I] != '\n' && Text[I] <= ' ') {
        ++I;
        ++Column;
      }

      if (I == Text.size())
        break;
    }

    assert(!Node->Parameters.empty() &&
           "empty lines should have been skipped before");

    // Now that we have loaded the whole line, we can generate any diagnostic.
    const auto &RootName = Node->Parameters[0];
    const bool IsRootValid = IsValidRootNode(RootName);
    if (!Indent && !IsRootValid) {
      auto &Diag = Result.Diagnostics.emplace_back(*Node, 0);
      Diag.Kind = Diagnostic::Error;
      Diag.Message = fmt::format("Name '{}' not found.", RootName);
    } else if (Indent == 1 && IsValidRootNode(Node->Parent->Parameters[0])) {
      // Check if the children are valid.
      if (!GetChildrenOf(Node->Parent->Parameters[0]).count(RootName)) {
        auto &Diag = Result.Diagnostics.emplace_back(*Node, 0);
        Diag.Kind = Diagnostic::Error;
        Diag.Message = fmt::format("Invalid child '{}' for '{}'.", RootName,
                                   Node->Parent->Parameters[0]);
      }
    }
    // FIXME: This should work for any root name.
    if (!Indent) {
      // Check if the parameters of the node are correct.
      const auto &ExpectedTypes = GetParameterTypesOf(RootName);
      for (std::size_t I = 1; I < Node->Parameters.size(); ++I) {
        // There might be too many parameters.
        if (I > ExpectedTypes.size()) {
          auto &Diag = Result.Diagnostics.emplace_back(*Node, I);
          Diag.Kind = Diagnostic::Warning;
          Diag.Message = fmt::format("Unused parameter.");
          continue;
        }

        auto Type = lsp::Type(Node->Parameters[I]);
        if (Type != ExpectedTypes[I - 1]) {
          auto &Diag = Result.Diagnostics.emplace_back(*Node, I);
          Diag.Kind = Diagnostic::Error;
          Diag.Message = fmt::format("Expected type '{}' got '{}'.",
                                     ExpectedTypes[I - 1], Type);
        }
      }
      // There might be too few parameters.
      if (ExpectedTypes.size() >= Node->Parameters.size()) {
        auto &Diag = Result.Diagnostics.emplace_back(*Node, 0);
        Diag.Kind = Diagnostic::Error;
        Diag.Message =
            fmt::format("Not enough arguments, {} missing.",
                        ExpectedTypes.size() - Node->Parameters.size() + 1);
      }
    }

    // Skip to the end of the line if this is a comment.
    if (Text[I] == '#')
      while (I < Text.size() && Text[I] != '\n')
        ++I;

    Previous.emplace_back(Node);
    Indents.emplace_back(Indent);
    // We finished parsing the line, on to the next one.
  }

  return Result;
}
