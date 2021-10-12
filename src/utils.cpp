#include "utils.h"
#include <filesystem>

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

std::size_t lsp::CountLineIndentation(std::string_view Line,
                                      bool AllowEmptyLines) {
  auto End = Line.find_first_not_of(" \f\t\v\r\n");
  if (End == std::string::npos)
    // This line is empty.
    return AllowEmptyLines ? Line.size() : 0;
  return End;
}

std::vector<std::string> lsp::FindESData(const fs::path &Path) {
  if (!fs::exists(Path) || !fs::is_directory(Path))
    return {};
  std::vector<std::string> Files;

  // If the directory has a 'data/' folder, then we only load files inside
  // inside 'data/'. If it doesn't, assume every .txt file is an ES data file.
  auto DataDir = Path / "data";
  const bool HasDataDirectory =
      fs::exists(DataDir) && fs::is_directory(DataDir);

  for (const auto &File :
       fs::recursive_directory_iterator(HasDataDirectory ? DataDir : Path)) {
    // Skip any directory.
    if (File.is_directory())
      continue;

    const auto &FilePath = File.path();
    // If this isn't the 'data/' dir, we must check the extension.
    if (!HasDataDirectory && FilePath.extension() != ".txt")
      continue;
    Files.emplace_back(FilePath.string());
  }

  return Files;
}

void lsp::Sanitize(std::string &Message) {
  // Remove any tab characters from output.
  std::size_t Index = 0;
  while ((Index = Message.find('\t')) != std::string::npos) {
    Message[Index] = 't';
    Message.insert(Index, 1, '\\');
  }
}
