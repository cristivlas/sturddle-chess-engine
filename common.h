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
/*
 * Constants, compile-time configuration, misc. helpers.
 */
#include <atomic>
#include <iostream>
#include <stdexcept>


#if defined(_MSC_VER) && !defined(__clang__)
  #pragma warning(disable:4244)
  #define _USE_MATH_DEFINES
  #define HAVE_INT128 false
#else
  #define HAVE_INT128 (INTPTR_MAX == INT64_MAX)
#endif /* Microsoft */

#if !defined(_DEBUG)
#if _MSC_VER
  #define INLINE __forceinline
#elif __GNUC__
  #define INLINE __attribute__((always_inline)) inline
#endif
#else
  #define INLINE inline
#endif

#include <cmath>

using score_t = int;


// ---------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------

#define ADAPTIVE_NULL_MOVE                  true

#define CACHE_HEURISTIC_CUTOFFS             false

/*
 * Count valid moves made as nodes if true, otherwise use effectively
 * searched nodes (not including TT pruned, FP and late-move pruned).
 */
#define COUNT_VALID_MOVES_AS_NODES          true

/* NOTE: this setting has no effect when using SEE */
#define EXCHANGES_DETECT_CHECKMATE          false

/* Collect extra stats for troubleshooting */
#define EXTRA_STATS                         false

#if !defined(NATIVE_UCI)
  #define NATIVE_UCI                        false
#endif /* NATIVE_UCI */

#define KILLER_MOVE_HEURISTIC               true

#define MTDF_CSTAR_BISECT                   true

#define MTDF_REORDER_MOVES                  true

#define RAZORING                            true

#define REPORT_CURRENT_MOVE                 false

#define REVERSE_FUTILITY_PRUNING            true

/*
 * https://www.chessprogramming.org/Singular_Extensions
 */
#define SINGULAR_EXTENSION                  true

#define SMP                                 true

/* experiment with different SEE heuristic impls */
#if !defined(USE_SIMPLE_SEE)
  #define USE_SIMPLE_SEE                    false
#endif /* USE_SIMPLE_SEE */

#if !WITH_NNUE && !defined(USE_PIECE_SQUARE_TABLES)
  #define USE_PIECE_SQUARE_TABLES           true
#endif

#define USE_LIBPOPCOUNT                     true

#if SMP
  #if __GNUC__
    /* For visibility("hidden"), see:
     * https://maskray.me/blog/2021-02-14-all-about-thread-local-storage
     */
    #define THREAD_LOCAL __attribute__((visibility("hidden"))) thread_local
  #else
    #define THREAD_LOCAL thread_local
  #endif
  using count_t = std::atomic<size_t>;
#else
  #define THREAD_LOCAL
  using count_t = size_t;
#endif /* !SMP */


/* default hash table size in megabytes */
constexpr size_t DEFAULT_HASH_TABLE_SIZE =  32;


#if !defined(MOBILITY_TUNING_ENABLED)
  #define MOBILITY_TUNING_ENABLED           false
#endif

/*
 * When TUNING_ENABLED is true, values introduced by DECLARE_VALUE in config.h
 * become visible to Python scripts (via set_param, get_params, get_param_info)
 * and can be tweaked at runtime.
 */
#if !defined(TUNING_ENABLED)
  #define TUNING_ENABLED                    false
#endif

/* if false, use piece-type/to-square tables */
#define USE_BUTTERFLY_TABLES                false

/*
 * Use this magic_bits implementation instead of attacks.h:
 * https://github.com/goutham/magic-bits
 */
#define USE_MAGIC_BITS                      true

/*
 * Number of processed nodes after which the search code checks
 * how much time it has left, and calls optional user-defined
 * callback.
 */
#ifndef CALLBACK_PERIOD
  #define CALLBACK_PERIOD                   4096
#endif

#ifndef EVAL_FUZZ_ENABLED
  #define EVAL_FUZZ_ENABLED                 false
#endif

constexpr int ENDGAME_PIECE_COUNT           = 12;

/* https://www.chessprogramming.org/Multi-Cut */
constexpr int MULTICUT_M                    = 6;
constexpr int MULTICUT_C                    = 3;


namespace search
{
    /*
     * https://www.chessprogramming.org/Move_Ordering#Typical_move_ordering
     */
    enum MoveOrder : int8_t
    {
        UNDEFINED = 0,
        PREV_ITER = 1,
        HASH_MOVES = 2,
        PROMOTIONS = 3,
        LAST_MOVED_CAPTURE = 4,
        WINNING_CAPTURES = 5,
        EQUAL_CAPTURES = 6,
        KILLER_MOVES = 7,
        LOSING_CAPTURES = 8,
        HISTORY_COUNTERS = 9,
        TACTICAL_MOVES = 10, /* pushed pawns, checks, etc. */
        LATE_MOVES = 11, /* all other legal moves not covered above */
        UNORDERED_MOVES = 12,
        PRUNED_MOVES = 13,
        QUIET_MOVES = 14,
        ILLEGAL_MOVES = 15,
    };
}


enum class LogLevel : int
{
    DEBUG = 1,
    INFO = 2,
    WARN = 3,
    ERROR = 4
};


template<typename T> struct Hasher
{
    constexpr std::size_t operator()(const T& key) const
    {
        return key.hash();
    }
};


// ---------------------------------------------------------------------
// ASSERT
// ---------------------------------------------------------------------
#define _STR(x) #x
#define _TOSTR(x) _STR(x)

#define ASSERT_ALWAYS(e) assert_expr((e), __FILE__ ":" _TOSTR(__LINE__) " " _STR(e))
#define ASSERT_MESSAGE(e, m) assert_expr((e), __FILE__ ":" _TOSTR(__LINE__) " " + m)

#if NO_ASSERT
 #define ASSERT(e)
#else
 #define ASSERT(e) ASSERT_ALWAYS(e)
#endif

template <typename T, typename S> static inline constexpr void assert_expr(T&& expr, S what)
{
    while (!expr)
    {
    #if !defined(NDEBUG) || defined(_DEBUG)
        abort();
    #else
        throw std::logic_error(what);
    #endif
    }
}

// ---------------------------------------------------------------------
//
// ---------------------------------------------------------------------
static inline std::string timestamp()
{
    return _TOSTR(BUILD_STAMP);
}

