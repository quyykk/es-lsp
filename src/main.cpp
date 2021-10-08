#include <cassert>
#include <charconv>
#include <filesystem>
#include <iostream>
#include <span>
#include <string_view>
#include <vector>

#include "DataNode.h"
#include "fmt/core.h"
#include "log.h"
#include "lsp.h"

namespace fs = std::filesystem;

int main(int Argc, char *Argv[]) {
  std::span Args(Argv + 1, Argc - 1);

  // Handle command line arguments.
  std::string_view ResourcePath;
  for (std::string_view Arg : Args) {
    if (Arg == "-v" || Arg == "--version") {
      lsp::Log(">> es-lsp v1.0\n");
      return 0;
    } else if (Arg == "--log") {
      // Clear existing log file.
      std::fclose(std::fopen("es-lsp.log", "w"));
      lsp::Enable();
    } else if (Arg.starts_with("-"))
      lsp::LogError(">> Unknown argument '{}'.\n", Arg);
    else
      ResourcePath = Arg;
  }

  fs::path Directory = ResourcePath;
  if (!ResourcePath.empty() && !fs::exists(Directory)) {
    lsp::LogError(">> Unknown directory '{}'.\n", ResourcePath);
    return -1;
  }

  lsp::Server Server;

  // Load data files from resource directory, if any.
  if (!ResourcePath.empty()) {
    lsp::Log(">> Loading resource directory '{}'\n", ResourcePath);
    Server.LoadFromDirectory(ResourcePath);
  }

  constexpr std::string_view ContentLength = "Content-Length: ";
  constexpr std::string_view ContentType = "Content-Type: ";

  lsp::Log(">> Ready...\n");

  // Ready to listen to notifications from the LSP client.
  int Size = 0;
  bool IsContent = false;
  std::string Line;
  while (Server.GetState() == lsp::ServerState::Running) {
    if (IsContent) {
      assert(Size && "invalid size of header");
      lsp::Log(">> Received message from client.\n");
      // We have content, we parse it and handle it.
      std::string Contents(Size, '\0');
      std::cin.read(Contents.data(), Size);
      Server.HandleNotification(std::move(Contents));
      IsContent = false;
      Size = 0;
      continue;
    }

    // Check for valid header.
    std::getline(std::cin, Line);
    if (Line.starts_with(ContentLength)) {
      std::from_chars(Line.data() + ContentLength.size(),
                      Line.data() + Line.size(), Size);
      lsp::Log(">> Got length: {}\n", Size);
    } else if (Line.starts_with(ContentType))
      // Just ignore the content type, it doesn't really matter anyway.
      continue;
    else if ((Line.empty() || (Line.size() == 1 && Line[0] == '\r')) && Size)
      // We are at the separator if we have read a header.
      IsContent = true;
  }

  lsp::Log(">> Shutting down...");
  return Server.GetState() == lsp::ServerState::ExitSuccess ? 0 : -1;
}
