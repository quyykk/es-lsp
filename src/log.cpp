#include "log.h"

#include <filesystem>

namespace fs = std::filesystem;

bool lsp::Enable(std::string_view Path) {
  if (fs::is_directory(Path))
    return false;

  impl::LogFile = Path;
  // Clear existing log file.
  std::fclose(std::fopen(Path.data(), "w"));

  return true;
}
