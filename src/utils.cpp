#include "utils.h"

std::string lsp::UriToFsPath(std::string_view Uri) {
  // Strip "file://" prefix from the uri.
  // FIXME: This function is broken on Windows.
  return std::string(Uri.substr(7));
}
