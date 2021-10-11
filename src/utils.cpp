#include "utils.h"

std::string lsp::UriToFsPath(std::string_view Uri) {
  // Strip "file://" prefix from the uri.
  return std::string(Uri.substr(7));
}

std::vector<std::string> lsp::TextToLines(std::string_view Text) {
  std::vector<std::string> Result;

  std::size_t OldPos = 0;
  std::size_t NewLine = -1;
  do {
    NewLine = Text.find('\n', NewLine + 1);
    Result.emplace_back(Text.substr(OldPos, NewLine - OldPos));
    OldPos = NewLine + 1;
  } while (NewLine != std::string::npos);

  return Result;
}
