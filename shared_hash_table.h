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

static_assert(std::atomic<uint64_t>::is_always_lock_free);

static constexpr uint64_t LOCKED = uint64_t(-1);
static constexpr auto QUADRATIC_PROBING = false;


namespace search
{
    template<typename T, int BUCKET_SIZE = 8> class SharedHashTable
    {
        using entry_t = T;
        using data_t = std::vector<entry_t>;
        using lock_t = std::atomic<uint64_t>;

    public:
        class SpinLock
        {
            using table_t = SharedHashTable;

            table_t*        _ht = nullptr;
            const size_t    _ix = 0;
            bool            _locked = false;

    #if SMP
            INLINE lock_t* lock_p()
            {
                static_assert(sizeof(lock_t) == sizeof(uint64_t));
                return reinterpret_cast<lock_t*>(&entry()->_lock);
            }

            template<bool strong=false>
            static bool try_lock(lock_t* lock, uint64_t key)
            {
                if constexpr(strong)
                    return lock->compare_exchange_strong(
                        key,
                        LOCKED,
                        std::memory_order_release,
                        std::memory_order_relaxed);
                else
                    return lock->compare_exchange_weak(
                        key,
                        LOCKED,
                        std::memory_order_release,
                        std::memory_order_relaxed);
            }

            INLINE void blocking_lock(const entry_t* e)
            {
                auto lock = lock_p();

                for (auto k = e->_hash; !try_lock(lock, k); k = e->_hash)
                    ;
                _locked = true;

            #if !NO_ASSERT
                entry()->_owner = this;
            #endif /* NO_ASSERT */
            }

            INLINE void non_blocking_lock(const entry_t*, uint64_t key)
            {
                if (try_lock<true>(lock_p(), key))
                {
                    _locked = true;
                #if !NO_ASSERT
                    entry()->_owner = this;
                #endif /* NO_ASSERT */
                }
            }

            INLINE void release()
            {
                ASSERT(_locked);
                ASSERT(*lock_p() == LOCKED);
                ASSERT(entry()->_owner == this);
                ASSERT(entry()->_hash);

                lock_p()->store(entry()->_hash, std::memory_order_release);
                _locked = false;
            }
    #else
            INLINE void blocking_lock(const entry_t*) { _locked = true; }
            INLINE bool non_blocking_lock(const entry_t* e, uint64_t key) { return e->_hash == key; }
            INLINE void release() { _locked = false; }
    #endif /* !SMP */

        protected:
            SpinLock() = default;

            SpinLock(SharedHashTable& ht, size_t ix) : _ht(&ht), _ix(ix)
            {
                blocking_lock(this->entry());
                ASSERT(_locked);
            }

            SpinLock(SharedHashTable& ht, size_t ix, uint64_t hash) : _ht(&ht), _ix(ix)
            {
                non_blocking_lock(this->entry(), hash);
            }

            ~SpinLock()
            {
                if (_locked)
                    release();
            }

            SpinLock(SpinLock&& other)
                : _ht(other._ht)
                , _ix(other._ix)
                , _locked(other._locked)
            {
                other._locked = false;
            #if !NO_ASSERT
                if (_locked)
                    entry()->_owner = this;
            #endif /* NO_ASSERT */
            }

            SpinLock(const SpinLock&) = delete;
            SpinLock& operator=(SpinLock&&) = delete;
            SpinLock& operator=(const SpinLock&) = delete;

            uint8_t clock() const { return _ht->_clock; }
            entry_t* entry() { return &_ht->_data[_ix]; }
            bool is_locked() const { return _locked; }

        public:
            explicit operator bool() const { return is_locked(); }
        };


        class Proxy : public SpinLock
        {
            entry_t* const _entry = nullptr;

        public:
            Proxy() = default;

            Proxy(SharedHashTable& ht, size_t ix) /* write */
                : SpinLock(ht, ix)
                , _entry(this->entry())
            {
                ASSERT(this->is_locked());
            }

            Proxy(SharedHashTable& ht, size_t ix, uint64_t hash) /* read */
                : SpinLock(ht, ix, hash)
                , _entry(this->is_locked() ? this->entry() : nullptr)
            {
            }

            INLINE const entry_t* operator->() const { return _entry; }
            INLINE const entry_t& operator *() const { return *_entry; }

            INLINE entry_t& operator *()
            {
                ASSERT(_entry);

                _entry->_age = this->clock();
                return *_entry;
            }
        };

    public:
        explicit SharedHashTable(size_t capacity)
            : _data(capacity)
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
            data_t(capacity).swap(_data);
        }

        template<bool quadratic> INLINE size_t next(uint64_t hash, size_t j) const
        {
            /*
             * https://en.wikipedia.org/wiki/Open_addressing
             */
            if constexpr(quadratic)
                return (hash + j * j) % _data.size();
            else
                return (hash + j) % _data.size();
        }

        Proxy lookup_read(const chess::State& state)
        {
            const auto h = state.hash();
            const size_t index = h % _data.size();

            for (size_t i = index, j = 1; j < BUCKET_SIZE; ++j)
            {
                Proxy p(*this, i, h);

                if (p)
                {
                    ASSERT(p->matches(state));
                    return p;
                }
                i = next<QUADRATIC_PROBING>(h, j);
            }

            return Proxy();
        }

        Proxy lookup_write(const chess::State& state, int depth)
        {
            const auto h = state.hash();
            size_t index = h % _data.size();

            for (size_t i = index, j = 1; j < BUCKET_SIZE; ++j)
            {
                Proxy q(*this, i, h); /* try non-blocking locking 1st */
                if (q)
                    return q;

                Proxy p(*this, i);
                ASSERT(p);

                if (!p->is_valid())
                {
                    ASSERT(p->_version == 0);

                    /* slot is unoccupied, bump up usage count */
                #if SMP
                    _used.fetch_add(1, std::memory_order_relaxed);
                #else
                    ++_used;
                #endif /* SMP */

                    return p;
                }

                if (p->_hash == h)
                    return p;

                if (p->_age != _clock)
                    return p;

                if (depth >= p->_depth)
                {
                #if 1
                    index = i;
                    depth = p->_depth;
                #else
                    return p;
                #endif
                }
                i = next<QUADRATIC_PROBING>(h, j);
            }
            return Proxy(*this, index);
        }

        INLINE size_t capacity() const { return _data.size(); }
        INLINE size_t size() const { return _used; }

        static INLINE size_t size_in_bytes(size_t n)
        {
            return n * sizeof(typename data_t::value_type);
        }

        INLINE uint8_t clock() const { return _clock; }
        INLINE void increment_clock() { ++_clock; }

    private:
        uint8_t     _clock = 0;
        count_t     _used = 0;
        data_t      _data;
    };
}
