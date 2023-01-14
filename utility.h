/*
 * Sturddle Chess Engine (C) 2022, 2023 Cristian Vlasceanu
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
 * --------------------------------------------------------------------------
 */
#pragma once

#include <random>
#include <stdexcept>
#include "Python.h"


enum class CancelReason
{
    PY_ERROR =  1,
    PY_SIGNAL = 2,
};

extern void cancel_search(CancelReason);

namespace
{
    /*
     * Utility for calling into Cython.
     * Not strictly needed if the cython functions are marked 'with gil'.
     */
    struct cython_wrapper
    {
        class GIL_State
        {
            PyGILState_STATE state;

        public:
            INLINE GIL_State() : state(PyGILState_Ensure())
            {
            }

            INLINE ~GIL_State()
            {
                if (PyErr_CheckSignals() != 0)
                {
                    cancel_search(CancelReason::PY_SIGNAL);
                }
                if (PyErr_Occurred())
                {
                    cancel_search(CancelReason::PY_ERROR);
                }
                PyGILState_Release(state);
            }
        };

        template <typename R, typename... Params, typename... Args>
        static INLINE R call(R (*fn)(Params...), Args&&... args)
        {
            GIL_State gil_state;
            ASSERT(fn);
            return fn(std::forward<Args>(args)...);
        }
    };


    /*
     * For sorting small vectors of small objects.
     * Sorting Move objects takes advantage of the XOR swap hack on 64 bit.
     */
    template<typename Iterator, typename Compare>
    INLINE void insertion_sort(Iterator first, Iterator last, Compare comp)
    {
        using std::swap;

        for (auto i = first; i != last; ++i)
        {
            for (auto j = i; j != first; --j)
            {
                if (comp(*j, *(j-1)))
                    swap(*j, *(j-1));
                else
                    break;
            }
        }
    }

    template<typename  I> INLINE void shift_left_2(I first, I last)
    {
    #if __cplusplus >= 202002L
        /* C++20 */
        std::shift_left(first, last, 2);
    #else
        using V = typename std::iterator_traits<I>::value_type;

        auto i = std::rotate(first, first + 2, last);
        *i++ = V(); *i = V();
    #endif
    }


    static INLINE int random_int(int low, int high)
    {
        static std::random_device rd;  // Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); // Standard mersenne_twister_engine seeded with rd()

        return std::uniform_int_distribution<int>(low, high)(gen);
    }
} /* namespace */
