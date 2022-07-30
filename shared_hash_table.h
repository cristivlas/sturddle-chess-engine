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

#define QUADRATIC_PROBING   false
#define TRY_LOCK_ON_READ    true


namespace search
{
    enum class Acquire : int8_t
    {
        Read,
        Write,
    };

    template<Acquire> struct LockTraits
    {
        template<class S> static void lock(S& spin) { spin.lock(); }
        template<class S> static bool constexpr is_locked(S& spin) { return true; }
    };

#if TRY_LOCK_ON_READ
    template<> struct LockTraits<Acquire::Read>
    {
        template<class S> static void lock(S& spin) { spin.try_lock(); }
        template<class S> static bool is_locked(S& spin) { return spin.is_locked(); }
    };
#endif /* TRY_LOCK_ON_READ */


    template<typename T, int BUCKET_SIZE=16> class SharedHashTable
    {
        using entry_t = T;
        using data_t = std::vector<entry_t>;

    #if SMP
        using locks_t = std::vector<std::atomic_flag>;
    #else
        struct locks_t /* dummy */
        {
            struct value_type {};
            explicit locks_t(size_t) {};
            void swap(locks_t&) {};
        };
    #endif /* !SMP */

    public:
        class SpinLock
        {
            static constexpr auto ACQUIRE = std::memory_order_acquire;
            static constexpr auto RELEASE = std::memory_order_release;

            using table_t = SharedHashTable;

            template<Acquire> friend struct LockTraits;

            table_t*        _ht = nullptr;
            const size_t    _ix = 0;
            bool            _locked = false;

        private:
    #if SMP
            std::atomic_flag* lock_p() { return &_ht->_locks[_ix]; }

            INLINE void lock()
            {
                while (std::atomic_flag_test_and_set_explicit(lock_p(), ACQUIRE))
                    ;
                _locked = true;
                entry()->_lock = this;
            }

            INLINE void release()
            {
                ASSERT(_locked);
                ASSERT(entry()->_lock == this);

                std::atomic_flag_clear_explicit(lock_p(), RELEASE);
                _locked = false;
            }

            INLINE void try_lock()
            {
                if (!std::atomic_flag_test_and_set_explicit(lock_p(), ACQUIRE))
                {
                    _locked = true;
                    entry()->_lock = this;
                }
            }
    #else
            INLINE void lock() { _locked = true; }
            INLINE void release() { _locked = false; }
            INLINE bool try_lock() { return true; }
    #endif /* !SMP */

        protected:
            entry_t* entry() { return _locked ? &_ht->_data[_ix] : nullptr; }
            const entry_t* entry() const { return const_cast<SpinLock*>(this)->entry(); }

            SpinLock() = default;

        public:
            template<typename LockTraits> SpinLock(SharedHashTable& ht, size_t ix, LockTraits)
                : _ht(&ht), _ix(ix)
            {
                LockTraits::lock(*this);
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

            bool is_locked() const { return _locked; }
            explicit operator bool() const { return is_locked(); }
        };

        class Proxy : public SpinLock
        {
            entry_t* const _entry = nullptr;

        public:
            Proxy() = default;

            template<typename LockTraits>
            Proxy(SharedHashTable& ht, size_t ix, LockTraits lock_traits)
                : SpinLock(ht, ix, lock_traits)
                , _entry(this->entry())
            {
            }

            INLINE const entry_t* operator->() const { return _entry; }
            INLINE const entry_t& operator *() const { return *_entry; }
            INLINE entry_t& operator *() { return *_entry; }
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
                std::fill_n(&_data[0], _data.size(), entry_t());
                _used = 0;
            }
        }

        void resize(size_t capacity)
        {
            locks_t(capacity).swap(_locks);
            data_t(capacity).swap(_data);
        }

        template<Acquire acquire>
        Proxy lookup(const chess::State& state, int depth = 0, int value = 0)
        {
            const auto h = state.hash();
            size_t index = h % _data.size();

            for (size_t i = index, j = 1; j < BUCKET_SIZE; ++j)
            {
                Proxy p(*this, i, LockTraits<acquire>{});

                if (LockTraits<acquire>::is_locked(p))
                {
                    if (!p->is_valid())
                    {
                        if constexpr (acquire == Acquire::Write)
                        {
                            /* slot is unoccupied, bump up usage count */
                        #if SMP
                            _used.fetch_add(1, std::memory_order_relaxed);
                        #else
                            ++_used;
                        #endif /* SMP */

                            return p;
                        }

                        return Proxy(); /* no match found */
                    }

                    const auto age = p->_age;

                    if (p->matches(state) && age <= _clock)
                        return p;

                    if constexpr (acquire == Acquire::Write)
                    {
                        if (age != _clock)
                            return p;

                        if (depth >= p->_depth && p->_value < value)
                        {
                            index = i;
                            depth = p->_depth;
                        }
                    }
                }

                /* https://en.wikipedia.org/wiki/Open_addressing */
                if constexpr(QUADRATIC_PROBING)
                    i = (h + j * j) % _data.size();
                else
                    i = (h + j) % _data.size();
            }

            /*
             * acquire == SpinLock::Acquire::Write: lock and return the entry at index;
             * otherwise: return unlocked Proxy, which means nullptr (no match found).
             */

            if constexpr (acquire == Acquire::Write)
                return Proxy(*this, index, LockTraits<acquire>{});
            else
                return Proxy();
        }

        INLINE size_t capacity() const { return _data.size(); }
        INLINE size_t size() const { return _used; }

        static INLINE size_t size_in_bytes(size_t n)
        {
            return n * (sizeof(typename data_t::value_type) + sizeof(typename locks_t::value_type));
        }

        INLINE size_t clock() const { return _clock; }
        INLINE void increment_clock() { ++_clock; }

    private:
        uint16_t    _clock = 0;
        count_t     _used = 0;
        data_t      _data;
        locks_t     _locks;
    };
}
