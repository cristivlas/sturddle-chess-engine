#pragma once
/*
 * Sturddle Chess Engine (C) 2022 Cristian Vlasceanu
 * --------------------------------------------------------------------------
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * --------------------------------------------------------------------------
 * Third-party files included in this project are subject to copyright
 * and licensed as stated in their respective header notes.
 *--------------------------------------------------------------------------
 */
#include <atomic>
#include <condition_variable>
#include <functional>
#include <mutex>
#include <thread>
#include <vector>
#include "common.h"


template<typename I> class thread_pool
{
public:
    using thread_id_type = I;

    explicit thread_pool(size_t thread_count) : _running(true)
    {
        for (size_t i = 0; i != thread_count; ++i)
            _threads.emplace_back(std::thread([this, i] {
                work(i + 1);
            }));
    }

    ~thread_pool()
    {
        _running = false;
        _cv.notify_all();

        for (auto& thread : _threads)
            thread.join();
    }

    thread_pool(const thread_pool&) = delete;
    thread_pool& operator=(const thread_pool&) = delete;

    size_t get_thread_count() const
    {
        return _threads.size();
    }

    template<typename T> void push_task(T&& task)
    {
        {
            std::unique_lock<std::mutex> lock(_mutex);
            _tasks.emplace_back(std::move(task));
        }
        _cv.notify_all();
    }

    static thread_id_type thread_id()
    {
        return _tid;
    }

    void wait_for_tasks()
    {
        std::unique_lock<std::mutex> lock(_mutex);
        while (!_tasks.empty())
        {
            _cv.notify_all();
            _cv.wait(lock);
        }
        while (_tasks_pending)
            std::this_thread::yield();
    }

private:
    void work(size_t index)
    {
        _tid = index;

        while (_running)
        {
            std::function<void()> task;
            {
                std::unique_lock<std::mutex> lock(_mutex);
                while (_tasks.empty())
                {
                    if (!_running)
                        return;
                    _cv.wait(lock);
                }
                task.swap(_tasks.back());

                ++_tasks_pending;
                _tasks.pop_back();
            }

            task();

            /* if task wraps a lambda, ensure that all variables captured
             * by value go out of scope before decrementing _tasks_pending
             */
            task = nullptr;

            --_tasks_pending;
            _cv.notify_all();
        }
    }

    std::atomic_bool _running;
    std::atomic_int _tasks_pending;
    std::condition_variable _cv;
    std::mutex _mutex;
    std::vector<std::function<void()>> _tasks;
    std::vector<std::thread> _threads;

    static THREAD_LOCAL thread_id_type _tid;
};


template<typename T> THREAD_LOCAL T thread_pool<T>::_tid;
