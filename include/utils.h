#ifndef UTILS_H
#define UTILS_H

#include <string>
#include <string_view>

namespace lsp {

// Converts a uri to a path to the filesystem. If this is not a uri that points
// to a fs path, returns an empty string.
std::string UriToFsPath(std::string_view Uri);

} // namespace lsp

#endif
