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
/* Part of the code generator for attacks.h */
#include <chrono>   // seed the random engine
#include <cmath>
#include <cstdint>
#include <functional>
#include <limits>
#include <random>
#include <set>


namespace perfect_hash
{
    using namespace std;

    template<typename T> T random()
    {
        static auto gen = bind(
            uniform_int_distribution<uint64_t>(0, numeric_limits<T>::max()),
            default_random_engine(unsigned(chrono::system_clock::now().time_since_epoch().count()))
            );

        return gen();
    }


    static inline uint64_t bitparity(uint64_t u)
    {
    #if _MSC_VER
        return __popcnt64(u) % 2;
    #else
        static_assert(__GNUC__, "unsupported compiler");

        return __builtin_parityll(u);
    #endif
    }

    /* Recursive template for loop unrolling. */
    template<int B> struct Hash
    {
        static constexpr inline uint64_t hash(uint64_t n, uint64_t r)
        {
            return Hash<B-1>::_hash(n, r);
        }

        static inline uint64_t _hash(uint64_t n, uint64_t r)
        {
            return bitparity(r & n) | (Hash<B-1>::_hash(n, r >> 1) << 1);
        }
    };

    template<> struct Hash<0>
    {
        static inline uint64_t _hash(uint64_t n, uint64_t r)
        {
            return bitparity(r & n);
        }
    };


    class HashMatrix
    {
        uint64_t _r = 0;
        uint64_t _h = 0;
        uint64_t (*_hash)(uint64_t, uint64_t) = nullptr;

    public:
        explicit HashMatrix(uint64_t r = 0) : _r(r)
        {
        }

        void init(size_t h)
        {
            _r = random<uint64_t>();
            _h = h;

            switch (_h)
            {
                case 9: _hash = &Hash<9>::hash; break;
                case 8: _hash = &Hash<8>::hash; break;
                case 7: _hash = &Hash<7>::hash; break;
                case 6: _hash = &Hash<6>::hash; break;
                case 5: _hash = &Hash<5>::hash; break;
                case 4: _hash = &Hash<4>::hash; break;
                case 3: _hash = &Hash<3>::hash; break;
                case 2: _hash = &Hash<2>::hash; break;
                case 1: _hash = &Hash<1>::hash; break;
                default: break;
            }
        }

        inline size_t hash(uint64_t n) const
        {
            if (_hash)
            {
                return _hash(n, _r);
            }

            ASSERT(false);
            return 0;
        }

        explicit operator bool() const
        {
            return _r != 0;
        }

        size_t size() const
        {
            return _h;
        }

        uint64_t r() const
        {
            return _r;
        }

        void swap(HashMatrix& other)
        {
            std::swap(_r, other._r);
            std::swap(_h, other._h);
            std::swap(_hash, other._hash);
        }
    };


    template<typename T, int MAX_ITERATIONS = 1000>
    bool generate(const T& container, HashMatrix& hash_matrix)
    {
        set<uint64_t> value_range;
        for (const auto& elem : container)
            value_range.insert(elem.second);

        HashMatrix m;

        const size_t domain_size = container.size();

        // Theoretically proven to have solutions
        // const size_t h_max = 2 * log2(domain_size);
        const size_t h_max = 1 + log2(domain_size);

        for (size_t h = log2(domain_size); h < h_max; ++h)
        {
            for (int i = 0; i < MAX_ITERATIONS; ++i)
            {
                m.init(h);

                set<size_t> range;

                for (const auto& elem : container)
                {
                    const auto hash_val = m.hash(elem.first);
                    if (!range.insert(hash_val).second)
                        break;
                }

                if (range.size() == domain_size)
                {
                    hash_matrix.swap(m);
                    /* clog << "Ok (" << i << "): h=" << h << ", d=" << domain_size << "\n"; */
                    return true;
                }
            }
        }

        return false;
    }
} /* namespace perfect_hash */
