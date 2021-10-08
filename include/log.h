#ifndef LOG_H
#define LOG_H

#include "fmt/core.h"

#include <cstdio>

namespace lsp {

namespace impl {
inline bool Enabled;
} // namespace impl

template <typename... Ts> void Log(fmt::format_string<Ts...> Fmt, Ts &&...Args);
template <typename... Ts>
void LogError(fmt::format_string<Ts...> Fmt, Ts &&...Args);

void Enable();

} // namespace lsp

template <typename... Ts>
void lsp::Log(fmt::format_string<Ts...> Fmt, Ts &&...Args) {
  if (impl::Enabled) {
    auto *File = std::fopen("es-lsp.log", "a");
    fmt::print(File, Fmt, std::forward<Ts>(Args)...);
    std::fclose(File);
  } else
    fmt::print(Fmt, std::forward<Ts>(Args)...);
}

template <typename... Ts>
void lsp::LogError(fmt::format_string<Ts...> Fmt, Ts &&...Args) {
  if (impl::Enabled) {
    auto *File = std::fopen("es-lsp.log", "a");
    fmt::print(File, Fmt, std::forward<Ts>(Args)...);
    std::fclose(File);
  } else
    fmt::print(stderr, Fmt, std::forward<Ts>(Args)...);
}
#endif
