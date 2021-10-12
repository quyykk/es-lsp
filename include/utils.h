#ifndef UTILS_H
#define UTILS_H

#include <string>
#include <string_view>
#include <vector>

namespace lsp {

// Converts a uri to a path to the filesystem. If this is not a uri that points
// to a fs path, returns an empty string.
std::string UriToFsPath(std::string_view Uri);

// Converts the given text to a vector that contains each line (without any
// newline).
std::vector<std::string> TextToLines(std::string_view Text);

// Counts the indentation of the passed line.
std::size_t CountLineIndentation(std::string_view Line,
                                 bool AllowEmptyLines = false);

} // namespace lsp

#endif
