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
    Config(const char* name, int* val, int min_val, int max_val)
    {
        _namespace.emplace(name, Config::Param{ val, min_val, max_val, Config::_group });
    }

    struct Group
    {
        Group(const char* group) { _group.assign(group); }
        ~Group() { _group.clear(); }
    };
};

std::string Config::_group;

Config::Namespace Config::_namespace = {
#if MOBILITY_TUNING_ENABLED && TUNING_ENABLED && EVAL_MOBILITY
    /* Piece mobility coefficients */
    { "MOBILITY_PAWN", Config::Param{ &chess::MOBILITY[chess::PieceType::PAWN], 0, 50 } },
    { "MOBILITY_KNIGHT", Config::Param{ &chess::MOBILITY[chess::PieceType::KNIGHT], 0, 50 } },
    { "MOBILITY_BISHOP", Config::Param{ &chess::MOBILITY[chess::PieceType::BISHOP], 0, 50 } },
    { "MOBILITY_ROOK", Config::Param{ &chess::MOBILITY[chess::PieceType::ROOK], 0, 50 } },
    { "MOBILITY_QUEEN", Config::Param{ &chess::MOBILITY[chess::PieceType::QUEEN], 0, 50 } },
    { "MOBILITY_KING", Config::Param{ &chess::MOBILITY[chess::PieceType::KING], 0, 50 } },
#endif /* TUNING_ENABLED && EVAL_MOBILITY */
};

  #define GROUP(x) Config::Group __##x(_TOSTR(x));

#else

  #define GROUP(x)

#endif /* !CONFIG_IMPL */

#if TUNING_ENABLED
  #if !defined(CONFIG_IMPL)
    #define DECLARE_VALUE(name, val, min_val, max_val) extern int name;
  #else
    #define DECLARE_VALUE(name, val, min_val, max_val) \
        int name(val); \
        Config param_##name(_TOSTR(name), &name, min_val, max_val);
  #endif /* CONFIG_IMPL */
#else
 /*
  * tuning disabled: params become compile-time constants
  */
  #define DECLARE_VALUE(name, val, min_val, max_val) static constexpr int name = val;
#endif /* TUNING_ENABLED */

/* Runtime params that are always visible (regardless of TUNING_ENABLED) */
#if !defined(CONFIG_IMPL)
    #define DECLARE_PARAM(name, alias, val, min_val, max_val) extern int name;
#else
    #define DECLARE_PARAM(name, alias, val, min_val, max_val) \
        int name(val); \
        Config param_##name(_TOSTR(alias), &name, min_val, max_val);
#endif /* CONFIG_IMPL */

#if SMP && defined(CONFIG_IMPL)
    static auto THREAD_MAX = std::thread::hardware_concurrency();
#else
    static constexpr int THREAD_MAX = 1;
#endif

static constexpr int HASH_MIN = 16; /* MB */

/****************************************************************************
 *              NAME                                VALUE  MIN      MAX
 ****************************************************************************/

GROUP(Settings)
DECLARE_VALUE(  ASPIRATION_WINDOW,                    1,    0,       1)
DECLARE_VALUE(  DEBUG_CAPTURES,                       0,    0,       1)
DECLARE_VALUE(  DEBUG_MATERIAL,                       0,    0,       1)
DECLARE_PARAM(  EVAL_FUZZ, EVAL_FUZZ,                 0,    0,     100)
DECLARE_VALUE(  FIFTY_MOVES_RULE,                     1,    0,       1)
DECLARE_VALUE(  FUTILITY_PRUNING,                     1,    0,       1)
DECLARE_VALUE(  LATE_MOVE_PRUNING,                    1,    0,       1)
DECLARE_VALUE(  MANAGE_TIME,                          1,    0,       1)
DECLARE_VALUE(  MULTICUT,                             1,    0,       1)
DECLARE_PARAM(  SMP_CORES, Threads,                   1,    1, THREAD_MAX)
DECLARE_PARAM(  STATIC_EXCHANGES, SEE,                0,    0,       1)

GROUP(Search)
DECLARE_VALUE(  DOUBLE_EXT_MARGIN,                  142,    0,    2000)
DECLARE_VALUE(  DOUBLE_EXT_MAX,                       6,    0,     100)
DECLARE_VALUE(  LATE_MOVE_REDUCTION_COUNT,            4,    0,     100)
DECLARE_VALUE(  NULL_MOVE_DEPTH_WEIGHT,              20,    0,     100)
DECLARE_VALUE(  NULL_MOVE_DEPTH_FACTOR,               6,    1,     100)

#if ADAPTIVE_NULL_MOVE
DECLARE_VALUE(  NULL_MOVE_FACTOR,                   213,    1,    1000)
#endif /* ADAPTIVE_NULL_MOVE */

DECLARE_VALUE(  NULL_MOVE_REDUCTION,                  4,    0,     100)
DECLARE_VALUE(  NULL_MOVE_MARGIN,                   337,    0,    1000)
DECLARE_VALUE(  NULL_MOVE_MIN_VERIFICATION_DEPTH,    14,    0,     100)
DECLARE_VALUE(  SINGULAR_MARGIN,                     22,    0,     100)

/* Move ordering */
GROUP(MoveOrdering)
DECLARE_VALUE(  COUNTER_MOVE_BONUS,                  69,    0,    2000)
DECLARE_VALUE(  COUNTER_MOVE_MIN_DEPTH,               6,    0,      20)
DECLARE_VALUE(  HISTORY_FAIL_LOW_MARGIN,           1438,    0,    2000)
DECLARE_VALUE(  HISTORY_FAIL_LOW_PENALTY,            55,    0,     100)
DECLARE_VALUE(  HISTORY_HIGH,                        95,    0,     100)
DECLARE_VALUE(  HISTORY_LOW,                          5,    0,     100)
DECLARE_VALUE(  HISTORY_MIN_DEPTH,                    8,    0,     100)

/* Tactical evaluation */
GROUP(Eval)
DECLARE_VALUE(  BISHOP_PAIR,                         68,    0,     100)
DECLARE_VALUE(  CASTLING_RIGHTS_BONUS,              108,    0,     300)
DECLARE_VALUE(  CHECK_BONUS,                         50,    0,     100)
DECLARE_VALUE(  CENTER_ATTACKS,                      32,    0,     100)
DECLARE_VALUE(  CENTER_OCCUPANCY,                   112,    0,     300)
DECLARE_VALUE(  KING_ATTACK_FACTOR,                  46,    1,      50)
DECLARE_VALUE(  KING_OUT_PENALTY,                  -150, -500,       0)
DECLARE_VALUE(  PAWN_SHIELD,                         24,    0,     100)
DECLARE_VALUE(  REDUNDANT_ROOK,                    -290, -500,       0)

DECLARE_VALUE(  ENDGAME_CONNECTED_ROOKS,             13,    0,     100)
DECLARE_VALUE(  ENDGAME_DEFENDED_PASSED,             50,    0,     100)
DECLARE_VALUE(  ENDGAME_KING_QUADRANT,                0,    0,     100)
DECLARE_VALUE(  ENDGAME_DOUBLED_PAWNS,              -24, -100,       0)
DECLARE_VALUE(  ENDGAME_ISOLATED_PAWNS,             -15, -100,       0)
DECLARE_VALUE(  ENDGAME_PASSED_FORMATION,            65,    0,     250)
DECLARE_VALUE(  ENDGAME_PAWN_MAJORITY,               73,    0,     250)
DECLARE_VALUE(  ENDGAME_THREATS,                     45,    0,     250)
DECLARE_VALUE(  ENDGAME_UNBLOCKED_PASSED_6,         110,    0,     250)
DECLARE_VALUE(  ENDGAME_UNBLOCKED_PASSED_7,         210,    0,     250)

DECLARE_VALUE(  MIDGAME_CONNECTED_ROOKS,             25,    0,     100)
DECLARE_VALUE(  MIDGAME_DEFENDED_PASSED,             30,    0,     100)
DECLARE_VALUE(  MIDGAME_KING_QUADRANT,               69,    0,     100)
DECLARE_VALUE(  MIDGAME_KING_OUT_PENALTY,           -65, -500,       0)
DECLARE_VALUE(  MIDGAME_DOUBLED_PAWNS,              -20, -100,       0)
DECLARE_VALUE(  MIDGAME_ISOLATED_PAWNS,             -17, -100,       0)
DECLARE_VALUE(  MIDGAME_HALF_OPEN_FILE,              93,    0,     250)
DECLARE_VALUE(  MIDGAME_OPEN_FILE,                   47,    0,     250)
DECLARE_VALUE(  MIDGAME_PASSED_FORMATION,            35,    0,     250)
DECLARE_VALUE(  MIDGAME_PAWN_MAJORITY,               53,    0,     250)
DECLARE_VALUE(  MIDGAME_THREATS,                     34,    0,     250)
DECLARE_VALUE(  MIDGAME_UNBLOCKED_PASSED_6,          55,    0,     250)
DECLARE_VALUE(  MIDGAME_UNBLOCKED_PASSED_7,         110,    0,     250)

#undef DECLARE_PARAM
#undef DECLARE_VALUE
#undef GROUP
