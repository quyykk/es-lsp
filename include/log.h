#ifndef LOG_H
#define LOG_H

#include "fmt/core.h"

#include <cstdio>
#include <mutex>

namespace lsp {

namespace impl {
inline std::string_view LogFile;
inline std::mutex Mutex;
} // namespace impl

template <typename... Ts> void Log(fmt::format_string<Ts...> Fmt, Ts &&...Args);
template <typename... Ts>
void LogError(fmt::format_string<Ts...> Fmt, Ts &&...Args);

bool Enable(std::string_view Path);

} // namespace lsp

template <typename... Ts>
void lsp::Log(fmt::format_string<Ts...> Fmt, Ts &&...Args) {
  if (!impl::LogFile.empty()) {
    std::lock_guard Lock(impl::Mutex);
    auto *File = std::fopen(impl::LogFile.data(), "a");
    fmt::print(File, Fmt, std::forward<Ts>(Args)...);
    std::fclose(File);
  }
}

template <typename... Ts>
void lsp::LogError(fmt::format_string<Ts...> Fmt, Ts &&...Args) {
  if (!impl::LogFile.empty()) {
    std::lock_guard Lock(impl::Mutex);
    auto *File = std::fopen(impl::LogFile.data(), "a");
    fmt::print(File, Fmt, std::forward<Ts>(Args)...);
    std::fclose(File);
  }
}
#endif
