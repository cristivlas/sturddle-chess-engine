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
/*
 * Constants, compile-time configuration, misc. helpers.
 */
#include <atomic>
#include <iostream>
#include <stdexcept>

#if defined(_MSC_VER)
  #pragma warning(disable:4244)
  #define _USE_MATH_DEFINES
  #define HAVE_INT128 false
#else
  #define HAVE_INT128 (INTPTR_MAX == INT64_MAX)
#endif /* Microsoft */

#include <cmath>

using asize_t = std::atomic<size_t>;
using score_t = int32_t;


// ---------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------

#define ADAPTIVE_NULL_MOVE                  true

#if !defined(LOW_MEMORY_PROFILE) && (__arm__ || __aarch64__)
  /* lower memory requirements for mobile */
  #define LOW_MEMORY_PROFILE                true
#endif /* LOW_MEMORY_PROFILE */

/*
 * Count valid moves made as nodes if true, otherwise use effectively
 * searched nodes (not including TT pruned, FP and late-move pruned).
 */
#define COUNT_VALID_MOVES_AS_NODES          true

#define EVAL_MOBILITY                       true

/*
 * Experiment with giving pawns a higher value towards endgame.
 * This is an unfinished idea, needs a smarter approach to incremental
 * updates of material and piece-square evaluations.
 */
/*
#define ENDGAME_INCREASED_PAWN_VALUE        150
*/

#define EXCHANGES_DETECT_CHECKMATE          false

#define KILLER_MOVE_HEURISTIC               true

#define MTDF_CSTAR_BISECT                   true

#define MTDF_REORDER_MOVES                  true

#define RECYCLE_CONTEXTS                    true /* custom operator new */

#define REVERSE_FUTILITY_PRUNING            true

#define SEE_PIN_AWARENESS_DEPTH             0

/*
 * https://www.chessprogramming.org/Singular_Extensions
 */
#define SINGULAR_EXTENSION                  true

#define SMP                                 true

#if SMP
  #if __GNUC__
    /* For visibility("hidden"), see:
     * https://maskray.me/blog/2021-02-14-all-about-thread-local-storage
     */
    #define THREAD_LOCAL __attribute__((visibility("hidden"))) thread_local
  #else
    #define THREAD_LOCAL thread_local
  #endif
  using count_t = asize_t;
#else
  #define THREAD_LOCAL
  using count_t = size_t;
#endif /* !SMP */


/*
 * The hash table size in MB can be changed via set_param()
 */
#if LOW_MEMORY_PROFILE
  /* https://planetmath.org/goodhashtableprimes */
  constexpr size_t TRANSPOSITION_TABLE_SLOTS = 3145739;
#else
  constexpr size_t TRANSPOSITION_TABLE_SLOTS = 16 * 1024 * 1024;
#endif /* LOW_MEMORY_PROFILE */


#define TUNING_ENABLED                      true

/* if false, use piece-type/to-square tables */
#define USE_BUTTERFLY_TABLES                false


/* Check time and call user-defined callback every N nodes */
constexpr int CALLBACK_COUNT                = 8192;

constexpr int ENDGAME_PIECE_COUNT           = 12;

/* https://www.chessprogramming.org/Late_Move_Reductions */
constexpr int LATE_MOVE_REDUCTION           = 1;

/* https://www.chessprogramming.org/Multi-Cut */
constexpr int MULTICUT_M                    = 6;
constexpr int MULTICUT_C                    = 3;
constexpr int MULTICUT_REDUCTION            = 3;

constexpr int KILLER_MOVE_ENTRIES           = 2;

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
        WINNING_CAPTURES = 4,
        EQUAL_CAPTURES = 5,
        KILLER_MOVES = 6,
        LOSING_CAPTURES = 7,
        HISTORY_COUNTERS = 8,
        /* REPEATEAD = 9, */
        TACTICAL_MOVES = 10, /* pushed pawns, checks, etc. */
        LATE_MOVES = 11, /* all other legal moves not covered above */
        UNORDERED_MOVES = 12,
        QUIET_MOVES = 13,
        PRUNED_MOVES = 14, /* experimental: futility-prune at move ordering level */
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
#define ASSERT_MESSAGE(e, m) assert_expr((e), __FILE__ ":" _TOSTR(__LINE__) " " m)

#if NO_ASSERT
 #define ASSERT(e)
#else
 #define ASSERT(e) ASSERT_ALWAYS(e)
#endif

template <typename T> static inline constexpr void assert_expr(T&& expr, const char* what)
{
    while (!expr)
    {
    #if !defined(NDEBUG)
        abort();
    #else
        throw std::logic_error(what);
    #endif
    }
}
