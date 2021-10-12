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
    String += ConvertNode(*Child, Indent + 1);
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
    Str += Node.second.ToString();
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
  return LoadFromText(Path.string().c_str(), Contents);
}

auto lsp::LoadFromText(std::string_view Path, std::string_view Text)
    -> RootDataNode {
  RootDataNode Result;
  Result.Path = Path;

  std::vector<DataNode *> Previous;
  std::vector<std::size_t> Indents;

  std::size_t Line = -1;
  std::size_t Column;
  // Used to update the last line of a entity definition.
  std::size_t *LastEntityLine = nullptr;
  std::size_t LastNonEmptyLine = -1;

  // What kind of indentation this file uses.
  // 0 - unknown
  // 1 - space
  // 2 - tabs
  // 3 - other weird characters.
  int FileIndentation = 0;

  // Helper function to warn about mixed whitespace usage.
  // Parse file contents into a DataNode.
  auto CheckIndent = [&FileIndentation, &Result](char Value, char Indent,
                                                 int Type, int Line, int Column,
                                                 int &LineIndentation,
                                                 std::string_view Seen) {
    // Special case for weird spaces that span multiple characters.
    if (Value != Indent || Type == 3)
      return;

    if (!LineIndentation)
      LineIndentation = Type;
    if (!FileIndentation)
      FileIndentation = Type;

    if (Type != LineIndentation) {
      // Mixed whitespace inside the current line.
      auto &Diag = Result.Diagnostics.emplace_back();
      Diag.Line = Line;
      Diag.Column = Column;
      Diag.EndColumn = Column + 1;
      Diag.Kind = Diagnostic::Warning;
      Diag.Message =
          fmt::format("mixed indentation: {} character seen, expected {}", Seen,
                      LineIndentation == 1   ? "space"
                      : LineIndentation == 2 ? "tab"
                                             : "weird space");
    } else if (Type != FileIndentation) {
      // Mixed whitespace inside the current file.
      auto &Diag = Result.Diagnostics.emplace_back();
      Diag.Line = Line;
      Diag.Column = Column;
      Diag.EndColumn = Column + 1;
      Diag.Kind = Diagnostic::Warning;
      Diag.Message =
          fmt::format("mixed indentation: {} character seen, expected {}", Seen,
                      FileIndentation == 1   ? "space"
                      : FileIndentation == 2 ? "tab"
                                             : "weird space");
    }
  };

  for (std::size_t I = 0; I < Text.size(); ++I) {
    ++Line;
    Column = 0;

    // Same values as the file indentation kind.
    int LineIndentation = 0;

    // Parse any indentation first.
    std::size_t Indent = 0;
    while (I < Text.size() && Text[I] <= ' ' && Text[I] != '\n') {
      // Check for indentation inconsistencies.
      CheckIndent(Text[I], ' ', 1, Line, Column, LineIndentation, "space");
      CheckIndent(Text[I], '\t', 2, Line, Column, LineIndentation, "tab");
      CheckIndent(Text[I], '\0', 3, Line, Column, LineIndentation,
                  "weird space");

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

    LastNonEmptyLine = Line;

    // If this line has no indentation, then it is a root node.
    // If it does, we need to check which node is its parent.
    DataNode *Node;
    if (!Indent) {
      Previous.clear();
      Indents.clear();
      Node = &Result.Nodes[Line];
      Node->Parent = nullptr;

      // We have the definition of a new entity, so if there was a last entity
      // definition we can now update its last line.
      if (LastEntityLine) {
        *LastEntityLine = LastNonEmptyLine;
        LastEntityLine = nullptr;
      }
    } else {
      // Choose the correct parent node.
      while (Indents.back() >= Indent) {
        Indents.pop_back();
        Previous.pop_back();
        assert(!Indents.empty() && "weird indentation?");
      }
      Node = &Result.Nodes[Line];
      Previous.back()->Children.emplace_back(Node);
      Node->Parent = Previous.back();
    }
    Node->Line = Line;
    Node->Indent = Indent;

    Previous.emplace_back(Node);
    Indents.emplace_back(Indent);

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
      Node->Quoted.emplace_back(Quoted);
      while (I < Text.size() && Text[I] != '\n' &&
             (Quoted ? Text[I] != Quote : Text[I] > ' ')) {
        Param += Text[I++];
        ++Column;
      }

      if (Text[I] == '\n' || I == Text.size()) {
        if (Quoted) {
          // If this parameter is quoted then there's a missing quotation mark.
          auto &Diag = Result.Diagnostics.emplace_back(
              *Node, Node->Parameters.size() - 1);
          Diag.Kind = Diagnostic::Error;
          Diag.Message = fmt::format("Missing '{}{}' end quote",
                                     Quote == '`' ? "" : "\\", Quote);
        }
        break;
      }

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

    CheckLine(Result.Diagnostics, Previous);
    if (!Indent && Node->Parameters.size() >= 2)
      // The last line of this entity is only known when we parsed the next
      // entity, so we need to update it later.
      LastEntityLine = &Result.Entities[Node->Parameters[0]]
                            .emplace_back(Entity{Node, 0})
                            .LastLine;

    // Skip to the end of the line if this is a comment.
    if (I < Text.size() && Text[I] == '#')
      while (I < Text.size() && Text[I] != '\n')
        ++I;
    // We finished parsing the line, on to the next one.
  }

  // We have reached the end of the file. We now must update the last line of
  // the last entity definition.
  if (LastEntityLine)
    *LastEntityLine = Line;

  return Result;
}
