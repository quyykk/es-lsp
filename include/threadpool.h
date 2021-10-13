#ifndef THREADPOOL_H
#define THREADPOOL_H

#include "mfunction.h"

#include <condition_variable>
#include <functional>
#include <mutex>
#include <queue>
#include <thread>

namespace lsp {

constexpr unsigned MaxThreads = 4;

// Thread pools for the LSP server. Work can be added to the queue to be picked
// up by any thread. Some work is more important than other (like the client
// sending a didChange request) so work with a higher priority will be executed
// before others. The order is the order that the work was submitted.
class ThreadPool final {
public:
  ThreadPool() noexcept;
  ThreadPool(const ThreadPool &) noexcept = delete;
  ThreadPool &operator=(const ThreadPool &) noexcept = delete;
  ~ThreadPool();

  // Adds a given job to the queue.
  void Push(Mfunction<bool()> Job) noexcept;

private:
  // Each worker thread loops endlessly waiting for work.
  void Loop();

private:
  std::thread Threads[MaxThreads];

  std::queue<Mfunction<bool()>> Queue;
  std::condition_variable Condition;
  std::mutex Mutex;
  bool Exit = false;
};

} // namespace lsp

#endif
