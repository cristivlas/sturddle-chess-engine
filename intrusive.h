/*
 * Sturddle Chess Engine (C) 2022 Cristi Vlasceanu
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
#pragma once

#include <atomic>
#include <algorithm>
#include <type_traits>
#include "common.h"

template<typename T> class intrusive_ptr
{
    template<typename U> friend class intrusive_ptr;

public:
    ~intrusive_ptr()
    {
        if (_ptr)
            _ptr->remove_ref();
    }

    intrusive_ptr(T* ptr = nullptr) : _ptr(ptr)
    {
        if (_ptr)
            _ptr->add_ref();
    }

    template<typename U> intrusive_ptr(const intrusive_ptr<U>& other)
        : _ptr(other._ptr)
    {
        if (_ptr)
            _ptr->add_ref();
    }

    intrusive_ptr(const intrusive_ptr& other)
        : _ptr(other._ptr)
    {
        if (_ptr)
            _ptr->add_ref();
    }

    template<typename U> intrusive_ptr& operator=(const intrusive_ptr<U>& other)
    {
        intrusive_ptr temp(other);
        this->swap(temp);
        return *this;
    }

    intrusive_ptr& operator=(const intrusive_ptr other)
    {
        intrusive_ptr temp(other);
        this->swap(temp);
        return *this;
    }

    explicit operator T*() const
    {
        return _ptr;
    }

    operator bool() const
    {
        return _ptr != nullptr;
    }

    bool operator !() const
    {
        return _ptr == nullptr;
    }

    T* get() const
    {
        return _ptr;
    }

    T* operator->() const
    {
        return get();
    }

    T& operator*() const
    {
        return *get();
    }

    void reset()
    {
        intrusive_ptr<T> temp;
        this->swap(temp);
    }

    template<typename U> void swap(intrusive_ptr<U>& other)
    {
        std::swap(_ptr, other._ptr);
    }

private:
    T* _ptr;
};


template<typename T> class RefCounted
{
    template<typename U> friend class ::intrusive_ptr;

#if SMP
    static_assert(std::atomic<int>::is_always_lock_free);
    std::atomic<int> _refcnt;

    INLINE void increment()
    {
        _refcnt.fetch_add(1, std::memory_order_relaxed);
    }

    INLINE bool decrement()
    {
        return _refcnt.fetch_sub(1, std::memory_order_acq_rel) == 1;
    }
#else
    int _refcnt = 0;

    INLINE void increment()
    {
        ++_refcnt;
    }

    INLINE bool decrement()
    {
        return --_refcnt == 0;
    }
#endif /* !SMP */

public:
    int ref_count() const { return _refcnt; }

protected:
    constexpr RefCounted() : _refcnt(0) {}
    RefCounted(const RefCounted&) : _refcnt(0) {}
    ~RefCounted() = default;

    INLINE void add_ref() { increment(); }

    INLINE void remove_ref()
    {
        ASSERT(_refcnt > 0);
        static_assert(!std::has_virtual_destructor<T>::value); // for perf. reasons

        if (decrement())
            delete static_cast<T*>(this);
    }
};
