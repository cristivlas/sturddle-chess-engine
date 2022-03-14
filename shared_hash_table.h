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

namespace search
{
    class SharedHashTable
    {
        static constexpr int BUCKET_SIZE = 4;

        using entry_t = TT_Entry;
        using data_t = std::vector<entry_t>;

    #if SMP
        using locks_t = std::vector<std::atomic_bool>;
    #else
        struct locks_t /* dummy */
        {
            using value_type = locks_t;
            explicit locks_t(size_t){};
            void swap(locks_t&){};
        };
    #endif /* !SMP */

        template<bool> friend class SpinLock;

        class SpinLock
        {
            static constexpr auto ACQUIRE = std::memory_order_acquire;
            static constexpr auto RELEASE = std::memory_order_release;

            using table_t = SharedHashTable;

            table_t*        _ht = nullptr;
            const size_t    _ix = 0;
            bool            _locked = false;

        private:
    #if SMP
            std::atomic_bool* lock_p() { return &_ht->_locks[_ix]; }

            inline void lock(bool try_lock)
            {
                if (try_lock)
                {
                    if (!std::atomic_exchange_explicit(lock_p(), true, ACQUIRE))
                    {
                        _locked = true;
                        entry()->_lock = this;
                    }
                }
                else
                {
                    while (std::atomic_exchange_explicit(lock_p(), true, ACQUIRE))
                        ;
                    _locked = true;
                    entry()->_lock = this;
                }
            }

            inline void release()
            {
                ASSERT(_locked);
                ASSERT_ALWAYS(entry()->_lock == this);

                std::atomic_store_explicit(lock_p(), false, RELEASE);
                _locked = false;
            }
    #else
            inline void lock(bool) { _locked = true; }
            inline void release() { _locked = false; }
    #endif /* !SMP */

        protected:
            entry_t* entry() { return _locked ? &_ht->_data[_ix] : nullptr; }
            const entry_t* entry() const { return const_cast<SpinLock*>(this)->entry(); }

            SpinLock() = default;

        public:
            SpinLock(SharedHashTable& ht, size_t ix, bool acquire = true, bool try_lock = false)
                : _ht(&ht), _ix(ix)
            {
                if (acquire)
                    lock(try_lock);
            }

            ~SpinLock()
            {
                if (_locked)
                {
                    _ht->_data[_ix]._age = _ht->_clock;
                    release();
                }
            }

            SpinLock(SpinLock&& other)
                : _ht(other._ht)
                , _ix(other._ix)
                , _locked(other._locked)
            {
                other._locked = false;
                if (_locked)
                    entry()->_lock = this;
            }
            SpinLock(const SpinLock&) = delete;
            SpinLock& operator=(SpinLock&&) = delete;
            SpinLock& operator=(const SpinLock&) = delete;

            explicit operator bool() const { return _locked; }
        };

        class Proxy : public SpinLock
        {
            TT_Entry* const _entry = nullptr;

        public:
            Proxy() = default;

            Proxy(SharedHashTable& ht, size_t ix, bool acquire, bool try_lock = false)
                : SpinLock(ht, ix, acquire, try_lock)
                , _entry(this->entry())
            {
            }

            inline const entry_t* operator->() const { return _entry; }
            inline const entry_t& operator *() const { return *_entry; }
            inline entry_t& operator *() { return *_entry; }
        };

    public:
        explicit SharedHashTable(size_t capacity)
            : _data(capacity)
            , _locks(capacity)
        {
        }

        SharedHashTable(const SharedHashTable&) = delete;
        SharedHashTable& operator=(const SharedHashTable&) = delete;

        void clear()
        {
            if (_used > 0)
            {
                const auto size = _data.size();
                _data.clear();
                _data.resize(size);
                _used = 0;
            }
        }

        void resize(size_t capacity)
        {
            locks_t(capacity).swap(_locks);
            data_t(capacity).swap(_data);
        }

        /*
         * https://en.wikipedia.org/wiki/Open_addressing
         */
        Proxy lookup(const State& state, bool acquire_for_writing, int depth = 0)
        {
            const auto h = state.hash();
            size_t index = h % _data.size();

            for (size_t i = index, j = 0; j < BUCKET_SIZE; ++j)
            {
            #if 0
                Proxy p(*this, i, true, acquire_for_writing);
                if (!p)
                    continue;
            #else
                Proxy p(*this, i, true);
            #endif

                if (!p->is_valid())
                {
                    if (acquire_for_writing)
                    {
                        ++_used; /* slot is unoccupied, bump up usage count */
                        return p;
                    }

                    return Proxy(); /* no match found */
                }

                const auto age = p->_age;

                if (p->matches(state) && age <= _clock)
                    return p;

                if (acquire_for_writing)
                {
                    if (age != _clock)
                        return p;

                    if (depth > p->_depth)
                    {
                        index = i;
                        depth = p->_depth;
                    }
                }
                i = (h + (j + 1) * (j + 1)) % _data.size();
            }

            return Proxy(*this, index, acquire_for_writing);
        }

        inline size_t capacity() const { return _data.size(); }
        inline size_t size() const { return _used; }

        static inline size_t size_in_bytes(size_t n)
        {
            return n * (sizeof(data_t::value_type) + sizeof(locks_t::value_type));
        }

        inline size_t clock() const { return _clock; }
        inline void increment_clock() { ++_clock; }

    private:
        uint16_t    _clock = 0;
        count_t     _used = 0;
        data_t      _data;
        locks_t     _locks;
    };
}