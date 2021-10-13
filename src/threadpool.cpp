#include "threadpool.h"

#include <algorithm>
#include <iostream>

void lsp::ThreadPool::Loop() {
  while (true) {
    std::unique_lock Lock(Mutex);
    Condition.wait(Lock, [this]() { return Exit || !Queue.empty(); });
    if (Exit)
      return;

    // This thread now controls the queue.
    auto Job = std::move(Queue.front());
    Lock.unlock();

    // If the job returns true, then it successfully finished.
    // If not, then we push it back onto the queue.
    bool Result = Job();

    Lock.lock();
    if (Result)
      Queue.pop();
    else
      Queue.push(std::move(Job));
  }
}

lsp::ThreadPool::ThreadPool() noexcept {
  const auto ThreadNum =
      std::min(std::thread::hardware_concurrency(), MaxThreads);
  for (unsigned I = 0; I < ThreadNum; ++I)
    Threads[I] = std::thread(&ThreadPool::Loop, this);
}

lsp::ThreadPool::~ThreadPool() {
  {
    std::lock_guard Lock(Mutex);
    Exit = true;
    Condition.notify_all();
  }

  for (unsigned I = 0; I < MaxThreads; ++I)
    if (Threads[I].joinable())
      Threads[I].join();
}

void lsp::ThreadPool::Push(Mfunction<bool()> Job) noexcept {
  std::lock_guard Lock(Mutex);
  Queue.push(std::move(Job));
  Condition.notify_one();
}
