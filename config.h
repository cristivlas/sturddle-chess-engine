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
#if defined(CONFIG_IMPL)
#include <map>
#include <string>
#include <thread>

struct Config
{
    struct Param
    {
        int* const  _val = nullptr;
        const int   _min = 0;
        const int   _max = 0;
        std::string _group;
    };

    using Namespace = std::map<std::string, Param>;

    static Namespace _namespace;
    static std::string _group;

    /* Register parameter names with Config::_namespace */
    Config(const char* n, int* v, int v_min, int v_max)
    {
        _namespace.emplace(n, Config::Param{ v, v_min, v_max, Config::_group });
    }

    struct Group
    {
        Group(const char* group) { _group.assign(group); }
        ~Group() { _group.clear(); }
    };
};

std::string Config::_group;

Config::Namespace Config::_namespace = {
#if MOBILITY_TUNING_ENABLED && EVAL_MOBILITY
    /* Piece mobility coefficients */
    { "MOBILITY_PAWN", Config::Param{ &chess::MOBILITY[chess::PieceType::PAWN], 0, 50 } },
    { "MOBILITY_KNIGHT", Config::Param{ &chess::MOBILITY[chess::PieceType::KNIGHT], 0, 50 } },
    { "MOBILITY_BISHOP", Config::Param{ &chess::MOBILITY[chess::PieceType::BISHOP], 0, 50 } },
    { "MOBILITY_ROOK", Config::Param{ &chess::MOBILITY[chess::PieceType::ROOK], 0, 50 } },
    { "MOBILITY_QUEEN", Config::Param{ &chess::MOBILITY[chess::PieceType::QUEEN], 0, 50 } },
    { "MOBILITY_KING", Config::Param{ &chess::MOBILITY[chess::PieceType::KING], 0, 50 } },
#endif /* MOBILITY_TUNING_ENABLED && EVAL_MOBILITY */
};

  #define GROUP(x) Config::Group __##x(_TOSTR(x));

#else

  #define GROUP(x)

#endif /* !CONFIG_IMPL */

/*
 * Runtime params that are always visible (regardless of TUNING_ENABLED).
 */
#if !defined(CONFIG_IMPL)
  #define DECLARE_ALIAS(n, a, v, v_min, v_max) extern int n;
#else
  #define DECLARE_ALIAS(n, a, v, v_min, v_max) int n(v); Config p_##n(_TOSTR(a), &n, v_min, v_max);
#endif /* CONFIG_IMPL */

#define DECLARE_CONST(n, v, v_min, v_max) static constexpr int n = v;
#define DECLARE_PARAM(n, v, v_min, v_max) DECLARE_ALIAS(n, n, v, v_min, v_max)

#if TUNING_ENABLED
  #define DECLARE_VALUE(n, v, v_min, v_max) DECLARE_PARAM(n, v, v_min, v_max)
#else
 /*
  * tuning disabled: params become compile-time constants
  */
  #define DECLARE_VALUE DECLARE_CONST

#endif /* TUNING_ENABLED */


#if SMP && defined(CONFIG_IMPL)
    static auto THREAD_MAX = std::thread::hardware_concurrency();
#else
    static constexpr int THREAD_MAX = 1;
#endif

static constexpr int HASH_MIN = 16; /* MB */

/* Min-max range is useful when exposing params via UCI (see sturddle.py) */
/****************************************************************************
 *              NAME                                VALUE  MIN      MAX
 ****************************************************************************/

GROUP(Settings)
DECLARE_VALUE(  ASPIRATION_WINDOW,                    1,    0,       1)
DECLARE_CONST(  DEBUG_CAPTURES,                       0,    0,       1)
DECLARE_CONST(  DEBUG_MATERIAL,                       0,    0,       1)
DECLARE_PARAM(  EVAL_FUZZ,                            0,    0,     100)
DECLARE_CONST(  FIFTY_MOVES_RULE,                     1,    0,       1)
DECLARE_VALUE(  FUTILITY_PRUNING,                     1,    0,       1)
DECLARE_VALUE(  MANAGE_TIME,                          1,    0,       1)
DECLARE_VALUE(  MULTICUT,                             1,    0,       1)
DECLARE_ALIAS(  SMP_CORES, Threads,                   1,    1, THREAD_MAX)
DECLARE_CONST(  STATIC_EXCHANGES,                     0,    0,       1)

GROUP(Search)
DECLARE_VALUE(  DOUBLE_EXT_MARGIN,                  635,    0,    2000)
DECLARE_VALUE(  DOUBLE_EXT_MAX,                       6,    0,     100)
DECLARE_VALUE(  LMP_BASE,                             2,    2,     100)
DECLARE_VALUE(  LATE_MOVE_REDUCTION_COUNT,            4,    0,     100)
DECLARE_VALUE(  NULL_MOVE_DEPTH_WEIGHT,               8,    0,     100)
DECLARE_VALUE(  NULL_MOVE_DEPTH_DIV,                  3,    1,     100)
DECLARE_VALUE(  NULL_MOVE_DIV,                      380,    1,    1000)
DECLARE_VALUE(  NULL_MOVE_REDUCTION,                  4,    0,     100)
DECLARE_VALUE(  NULL_MOVE_IMPROVEMENT_DIV,           15,    1,     100)
DECLARE_VALUE(  NULL_MOVE_MARGIN,                   262,    0,    1000)
DECLARE_VALUE(  NULL_MOVE_MIN_VERIFICATION_DEPTH,    14,    0,     100)
DECLARE_VALUE(  SINGULAR_MARGIN,                      6,    0,     100)

GROUP(MoveOrdering)
DECLARE_VALUE(  COUNTER_MOVE_BONUS,                3412,    0,    5000)
DECLARE_VALUE(  COUNTER_MOVE_MIN_DEPTH,               3,    0,      20)
DECLARE_VALUE(  HISTORY_COUNT_HIGH,                8915,    1,   20000)
DECLARE_VALUE(  HISTORY_FAIL_LOW_MARGIN,            918,    0,    2000)
DECLARE_VALUE(  HISTORY_FAIL_LOW_PENALTY,            62,    0,     100)
DECLARE_VALUE(  HISTORY_HIGH,                        87,    0,     100)
DECLARE_VALUE(  HISTORY_LOW,                         61,    0,     100)
DECLARE_VALUE(  HISTORY_MIN_DEPTH,                   11,    0,     100)

GROUP(Eval)
DECLARE_VALUE(  BISHOP_PAIR,                         68,    0,     100)
DECLARE_VALUE(  CASTLING_RIGHTS_BONUS,              108,    0,     300)
DECLARE_VALUE(  CHECK_BONUS,                         50,    0,     100)
DECLARE_VALUE(  CENTER_ATTACKS,                      51,    0,     100)
DECLARE_VALUE(  CENTER_OCCUPANCY,                   108,    0,     300)
DECLARE_VALUE(  KING_ATTACK_DIV,                     28,    1,     100)
DECLARE_VALUE(  KING_OUT_PENALTY,                  -150, -500,       0)
DECLARE_VALUE(  PAWN_SHIELD,                         22,    0,     100)
DECLARE_VALUE(  REDUNDANT_ROOK,                    -119, -500,       0)
DECLARE_VALUE(  TACTICAL_LOW_DEPTH,                  15,    0,     100)

DECLARE_VALUE(  ENDGAME_CONNECTED_ROOKS,             13,    0,     100)
DECLARE_VALUE(  ENDGAME_DEFENDED_PASSED,             15,    0,     100)
DECLARE_VALUE(  ENDGAME_KING_QUADRANT,                0,    0,     100)
DECLARE_VALUE(  ENDGAME_DOUBLED_PAWNS,              -24, -100,       0)
DECLARE_VALUE(  ENDGAME_ISOLATED_PAWNS,             -15, -100,       0)
DECLARE_VALUE(  ENDGAME_PASSED_FORMATION,            32,    0,     250)
DECLARE_VALUE(  ENDGAME_PAWN_MAJORITY,               13,    0,     250)
DECLARE_VALUE(  ENDGAME_THREATS,                     29,    0,     250)
DECLARE_VALUE(  ENDGAME_UNBLOCKED_PASSED_6,         145,    0,     250)
DECLARE_VALUE(  ENDGAME_UNBLOCKED_PASSED_7,         295,    0,     500)

DECLARE_VALUE(  MIDGAME_CONNECTED_ROOKS,             25,    0,     100)
DECLARE_VALUE(  MIDGAME_DEFENDED_PASSED,             63,    0,     100)
DECLARE_VALUE(  MIDGAME_KING_QUADRANT,               14,    0,     100)
DECLARE_VALUE(  MIDGAME_DOUBLED_PAWNS,              -20, -100,       0)
DECLARE_VALUE(  MIDGAME_ISOLATED_PAWNS,             -17, -100,       0)
DECLARE_VALUE(  MIDGAME_HALF_OPEN_FILE,              81,    0,     250)
DECLARE_VALUE(  MIDGAME_OPEN_FILE,                   30,    0,     250)
DECLARE_VALUE(  MIDGAME_PASSED_FORMATION,            76,    0,     250)
DECLARE_VALUE(  MIDGAME_PAWN_MAJORITY,               48,    0,     250)
DECLARE_VALUE(  MIDGAME_THREATS,                     67,    0,     250)
DECLARE_VALUE(  MIDGAME_UNBLOCKED_PASSED_6,         135,    0,     250)
DECLARE_VALUE(  MIDGAME_UNBLOCKED_PASSED_7,         215,    0,     250)

#undef DECLARE_ALIAS
#undef DECLARE_PARAM
#undef DECLARE_VALUE
#undef GROUP
