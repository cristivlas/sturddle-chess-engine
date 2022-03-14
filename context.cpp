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
#include "common.h"
/*
 * Move ordering, board state evaluation, and other stuff
 * pertaining to the Context of the node being searched.
 */
#include <iomanip>
#include <iterator>
#include <map>
#include <random>
#include <sstream>
#include <thread>
#include "context.h"
#include "search.h"
#include "utility.h"

using namespace chess;
using search::TranspositionTable;

/*---------------------------------------------------------------------------
 *
 * Configuration API, for tweaking parameters via Python scripts.
 *
 *--------------------------------------------------------------------------*/
struct Config
{
    struct Param
    {
        int* const _val = nullptr;
        const int  _min = 0;
        const int  _max = 0;
        const int  _idx = 0;
    };

    using Namespace = std::map<std::string, Param>;
    static Namespace _namespace;

    /* Register parameter names with Config::_namespace */
    Config(const char* name, int* val, int min_val, int max_val)
    {
        _namespace.emplace(name, Config::Param{ val, min_val, max_val });
    }
};

#if TUNING_ENABLED
  #define DECLARE_PARAM(name, val, min_val, max_val) \
    static int name(val); \
    Config param_##name(_TOSTR(name), &name, min_val, max_val);
#else
 /*
  * tuning disabled: params become compile-time constants
  */
  #define DECLARE_PARAM(name, val, min_val, max_val) static constexpr int name = val;
#endif /* TUNING_ENABLED */

#define DECLARE_ALIAS(name, alias, val, min_val, max_val) \
    static int name(val); \
    Config param_##name(_TOSTR(alias), &name, min_val, max_val);


/* Late-move pruning counts (idea borrowed from Crafty) */
struct LMP
{
    const size_t _size;
    std::vector<int> _counters;

    LMP(size_t size = 16) : _size(size), _counters(size)
    {
        std::generate_n(_counters.begin(), size, [] {
            static int i = 0;
            return 15 + int(pow(i++ + .5, 1.99));
        });
    }
} LMP;


Config::Namespace Config::_namespace = {
#if TUNING_ENABLED && EVAL_MOBILITY
    /* Piece mobility coefficients */
    { "MOBILITY_PAWN", Config::Param{ &MOBILITY[PieceType::PAWN], 0, 50 } },
    { "MOBILITY_KNIGHT", Config::Param{ &MOBILITY[PieceType::KNIGHT], 0, 50 } },
    { "MOBILITY_BISHOP", Config::Param{ &MOBILITY[PieceType::BISHOP], 0, 50 } },
    { "MOBILITY_ROOK", Config::Param{ &MOBILITY[PieceType::ROOK], 0, 50 } },
    { "MOBILITY_QUEEN", Config::Param{ &MOBILITY[PieceType::QUEEN], 0, 50 } },
    { "MOBILITY_KING", Config::Param{ &MOBILITY[PieceType::KING], 0, 50 } },
#endif /* TUNING_ENABLED && EVAL_MOBILITY */
};

#if SMP
    static auto THREAD_MAX = std::thread::hardware_concurrency();
#else
    static constexpr int THREAD_MAX = 1;
#endif
static constexpr int HASH_MIN = 16; /* MB */

/****************************************************************************
 *              NAME                                VALUE  MIN      MAX
 ****************************************************************************/
/* General */
DECLARE_ALIAS(  STATIC_EXCHANGES, SEE,                0,    0,       1)
DECLARE_PARAM(  ASPIRATION_WINDOW,                    1,    0,       1)
DECLARE_PARAM(  DEBUG_CAPTURES,                       0,    0,       1)
DECLARE_PARAM(  DEBUG_HISTORY,                        0,    0,       1)
DECLARE_PARAM(  DEBUG_MATERIAL,                       0,    0,       1)
DECLARE_PARAM(  DEBUG_PRUNING,                        0,    0,       1)
DECLARE_PARAM(  EVAL_FUZZ,                            0,    0,     100)
DECLARE_PARAM(  FIFTY_MOVES_RULE,                     1,    0,       1)
DECLARE_PARAM(  FUTILITY_PRUNING,                     1,    0,       1)
DECLARE_PARAM(  LATE_MOVE_REDUCTION_COUNT,            4,    0,     100)
DECLARE_PARAM(  MANAGE_TIME,                          1,    0,       1)
DECLARE_PARAM(  MOVE_MAKER_PRUNE_COUNT,               0,    0,     100)
DECLARE_PARAM(  MULTICUT,                             0,    0,       1)

#if ADAPTIVE_NULL_MOVE
DECLARE_PARAM(  NULL_MOVE_FACTOR,                   200,   10,    1000)
#endif /* ADAPTIVE_NULL_MOVE */

DECLARE_PARAM(  NULL_MOVE_REDUCTION,                  4,    0,       4)
DECLARE_PARAM(  NULL_MOVE_MARGIN,                     0,    0,    1000)
DECLARE_PARAM(  NULL_MOVE_MIN_VERIFICATION_DEPTH,    14,    0,     100)

DECLARE_PARAM(  SINGULAR_MARGIN,                      6,    0,     250)
DECLARE_ALIAS(  SMP_CORES, Threads,                   1,    1, THREAD_MAX)

/* Move ordering */
DECLARE_PARAM(  CAPTURE_LAST_MOVED_BONUS,            21,    0,     100)
DECLARE_PARAM(  COUNTER_MOVE_BONUS,                  20,    0,     100)
DECLARE_PARAM(  HISTORY_HIGH,                        95,    0,     100)

/* Tactical evaluation */
DECLARE_PARAM(  BISHOP_PAIR,                         67,    0,     100)
DECLARE_PARAM(  BISHOPS_STRENGTH,                    13,    0,     100)
DECLARE_PARAM(  CHECK_BONUS,                         50,    0,     100)
DECLARE_PARAM(  CENTER_ATTACKS,                      16,    0,     100)
DECLARE_PARAM(  CENTER_OCCUPANCY,                     3,    0,     100)
DECLARE_PARAM(  REDUNDANT_ROOK,                     -33, -500,       0)

DECLARE_PARAM(  ENDGAME_CONNECTED_ROOKS,              6,    0,     100)
DECLARE_PARAM(  ENDGAME_KING_QUADRANT,                4,    0,     100)
DECLARE_PARAM(  ENDGAME_DOUBLED_PAWNS,              -24, -100,       0)
DECLARE_PARAM(  ENDGAME_ISOLATED_PAWNS,             -15, -100,       0)
DECLARE_PARAM(  ENDGAME_PASSED_FORMATION,           100,    0,     300)
DECLARE_PARAM(  ENDGAME_PAWN_MAJORITY,               73,    0,     300)
DECLARE_PARAM(  ENDGAME_UNBLOCKED_PASSED_6,         140,    0,     300)
DECLARE_PARAM(  ENDGAME_UNBLOCKED_PASSED_7,         160,    0,     300)

DECLARE_PARAM(  MIDGAME_CONNECTED_ROOKS,             41,    0,     100)
DECLARE_PARAM(  MIDGAME_KING_QUADRANT,               71,    0,     100)
DECLARE_PARAM(  MIDGAME_KING_OUT_PENALTY,           -62, -500,       0)
DECLARE_PARAM(  MIDGAME_DOUBLED_PAWNS,              -20, -100,       0)
DECLARE_PARAM(  MIDGAME_ISOLATED_PAWNS,             -17, -100,       0)
DECLARE_PARAM(  MIDGAME_HALF_OPEN_FILE,              57,    0,     100)
DECLARE_PARAM(  MIDGAME_OPEN_FILE,                   57,    0,     100)
DECLARE_PARAM(  MIDGAME_PASSED_FORMATION,            75,    0,     300)
DECLARE_PARAM(  MIDGAME_PAWN_MAJORITY,               53,    0,     300)
DECLARE_PARAM(  MIDGAME_UNBLOCKED_PASSED_6,         130,    0,     300)
DECLARE_PARAM(  MIDGAME_UNBLOCKED_PASSED_7,         145,    0,     300)

DECLARE_PARAM(  LAZY_EVAL_MARGIN,                   650,    0, SCORE_MAX)

#if TUNING_ENABLED

extern int ENDGAME_PAWN_SQUARE_TABLE[64];

/* endgame pawns table */
DECLARE_PARAM(  ENDGAME_PAWNS_2, ENDGAME_PAWN_SQUARE_TABLE[48], -100, 100)
DECLARE_PARAM(  ENDGAME_PAWNS_3, ENDGAME_PAWN_SQUARE_TABLE[40], -100, 100)
DECLARE_PARAM(  ENDGAME_PAWNS_4, ENDGAME_PAWN_SQUARE_TABLE[32], -100, 100)
DECLARE_PARAM(  ENDGAME_PAWNS_5, ENDGAME_PAWN_SQUARE_TABLE[24], -100, 200)
DECLARE_PARAM(  ENDGAME_PAWNS_6, ENDGAME_PAWN_SQUARE_TABLE[16], -100, 300)
DECLARE_PARAM(  ENDGAME_PAWNS_7, ENDGAME_PAWN_SQUARE_TABLE[8],  -100, 400)
#endif /* TUNING_ENABLED */

#undef DECLARE_PARAM


std::map<std::string, Param> _get_param_info()
{
    std::map<std::string, Param> info;

    for (const auto& elem : Config::_namespace)
    {
        info.emplace(elem.first, Param{ *elem.second._val, elem.second._min, elem.second._max });
    }

    info.emplace("Hash", Param {
        int(TranspositionTable::get_hash_size()), HASH_MIN, int(TranspositionTable::max_hash_size())
    });

    return info;
}


void _set_param(const std::string& name, int value, bool echo)
{
    if (name == "Hash")
    {
        const int HASH_MAX = int(TranspositionTable::max_hash_size());
        TranspositionTable::set_hash_size(std::max(std::min(value, HASH_MAX), HASH_MIN));

        if (echo)
            std::clog << name << " = " << TranspositionTable::get_hash_size() << std::endl;
        return;
    }

    const auto iter = Config::_namespace.find(name);

    if (iter == Config::_namespace.end())
    {
        std::cerr << "unknown parameter: \"" << name << "\"\n";
    }
    else if (value < iter->second._min || value > iter->second._max)
    {
        std::clog << name << " value " << value << " is out of range [";
        std::clog << iter->second._min << ", " << iter->second._max << "]\n";
    }
    else
    {
        ASSERT_ALWAYS(iter->second._val);
        *iter->second._val = value;

        if (echo) /* debug */
        {
            std::clog << name << " = " << *iter->second._val << "\n";
        }
    }
}


std::map<std::string, int> _get_params()
{
    std::map<std::string, int> cfg;
    for (const auto& elem : Config::_namespace)
    {
        cfg.emplace(elem.first, *elem.second._val);
    }
    cfg.emplace(std::string("Hash"), int(TranspositionTable::get_hash_size()));
    return cfg;
}


#if TUNING_ENABLED

static void set_row(int (&table)[64], int row, int value)
{
    for (int i = 0; i != 8; ++i)
        table[(8 - row) * 8 + i] = value;
}


static void init_endgame_pawns_table()
{
    set_row(ENDGAME_PAWN_SQUARE_TABLE, 2, ENDGAME_PAWNS_2);
    set_row(ENDGAME_PAWN_SQUARE_TABLE, 3, ENDGAME_PAWNS_3);
    set_row(ENDGAME_PAWN_SQUARE_TABLE, 4, ENDGAME_PAWNS_4);
    set_row(ENDGAME_PAWN_SQUARE_TABLE, 5, ENDGAME_PAWNS_5);
    set_row(ENDGAME_PAWN_SQUARE_TABLE, 6, ENDGAME_PAWNS_6);
    set_row(ENDGAME_PAWN_SQUARE_TABLE, 7, ENDGAME_PAWNS_7);

#if !defined(NDEBUG)
    if (search::Context::_log_message)
    {
        std::ostringstream out;
        const auto& table = ENDGAME_PAWN_SQUARE_TABLE;
        for (auto i = 0; i < 64; ++i)
        {
            if (i % 8 == 0)
                out << "\n";
            out << std::setw(3) << table[i] << ", ";
        }
        search::Context::log_message(LogLevel::DEBUG, out.str());
    }
#endif /* !NDEBUG */
}
#else
static void init_endgame_pawns_table()
{
}
#endif /* TUNING_ENABLED */


namespace search
{
    /* Evaluate material plus piece-squares from the point of view
     * of the side that just moved. This is different from the other
     * evaluate methods which apply the perspective of the side-to-move.
     * Side-effect: caches simple score inside the State object.
     */
    static inline score_t eval_material_and_piece_squares(State& state)
    {
        if (state.simple_score == 0)
            state.simple_score = state.eval_simple();

        return state.simple_score * SIGN[!state.turn];
    }


    static inline constexpr int promo_value(const State& state)
    {
        return state.promotion ? state.weight(state.promotion) - state.weight(PAWN) : 0;
    }


    /* No promotions, checks or moving out of check. Captures are OK. */
    static bool is_pseudo_quiet(const State& s, const Context* ctxt = nullptr)
    {
        if (s.promotion)
            return false;

        if (ctxt && ctxt->is_evasion())
            return false;

        if (s.is_check())
            return false;

        return true;
    }


    static bool is_quiet(const State& state, const Context* ctxt = nullptr)
    {
        return is_pseudo_quiet(state, ctxt)
            && state.capture_value == 0
            && state.pushed_pawns_score <= 1;
    }


    /*---------------------------------------------------------------------
     * Context
     *---------------------------------------------------------------------*/
    std::atomic_bool Context::_cancel(false);
    std::mutex Context::_mutex;

    int     Context::_time_limit = 0; /* milliseconds */
    time    Context::_time_start;
    asize_t Context::_callback_count(0);

    /* Cython callbacks */
    PyObject* Context::_engine = nullptr;

    std::string (*Context::_epd)(const State&) = nullptr;
    void (*Context::_log_message)(int, const std::string&, bool) = nullptr;

    void (*Context::_on_iter)(PyObject*, ContextPtr, score_t) = nullptr;
    void (*Context::_on_next)(PyObject*, int64_t) = nullptr;

    std::string(*Context::_pgn)(ContextPtr) = nullptr;
    void (*Context::_print_state)(const State&) = nullptr;
    void (*Context::_report)(PyObject*, std::vector<ContextPtr>&) = nullptr;
    size_t (*Context::_vmem_avail)() = nullptr;


#if RECYCLE_CONTEXTS
    static MemoryPool<Context>& context_pool()
    {
        static THREAD_LOCAL MemoryPool<Context> pool(256);
        return pool;
    }

    void* Context::operator new(size_t size)
    {
        if (auto ptr = context_pool().get(size))
            return ptr;

        return ::operator new(size);
    }
#else
    void* Context::operator new(size_t size)
    {
        return ::operator new(size);
    }
#endif /* RECYCLE_CONTEXTS */


    void* Context::operator new(size_t size, void* ptr)
    {
        return ::operator new(size, ptr);
    }


    void Context::operator delete(void* ptr, size_t size) noexcept
    {
    #if RECYCLE_CONTEXTS
        context_pool().put(ptr, size);
    #else
        ::operator delete(ptr);
    #endif /* RECYCLE_CONTEXTS */
    }


    /* static */ void Context::init()
    {
        _init();
        init_endgame_pawns_table();
    }


    Context::~Context()
    {
    }


    /* static */
    void Context::log_message(LogLevel level, const std::string& message, bool force)
    {
        cython_wrapper::call(_log_message, int(level), message, force);
    }


    /* static */ int Context::late_move_reduction_count()
    {
        return LATE_MOVE_REDUCTION_COUNT;
    }


    /* static */ int Context::cpu_cores()
    {
        return SMP_CORES;
    }


    /*
     * Copy context for two specific use cases:
     * 1) clone root at the beginning of SMP searches, and
     * 2) create a temporary context for singularity search.
     */
    ContextPtr Context::clone(int ply) const
    {
        ContextPtr ctxt(new Context);

        ctxt->_algorithm = _algorithm;
        ctxt->_alpha = _alpha;
        ctxt->_beta = _beta;
        ctxt->_score = _score;
        ctxt->_history = ply ? _history : new History(*_history);
        ctxt->_max_depth = _max_depth;
        ctxt->_parent = _parent;
        ctxt->_ply = ply;
        ctxt->_prev = _prev;
        ctxt->_statebuf = state();
        ctxt->_state = &ctxt->_statebuf;
        ctxt->_move = _move;
        ctxt->_excluded = _excluded;
        ctxt->_tt_entry = _tt_entry;
        ctxt->_tid = _tid;
        ctxt->_move_maker.set_ply(ply);

        return ctxt;
    }


    score_t Context::material_gain(bool same_square) const
    {
        score_t gain = 0;
        int sign = 1;

        auto square = Square::UNDEFINED;

        for (const Context* current = this; current; current = current->_parent)
        {
            if (current->state().capture_value == 0)
            {
                break;
            }

            if (same_square)
            {
                if (square == Square::UNDEFINED)
                {
                    square = current->_move.to_square();
                }
                else if (current->_move.to_square() != square)
                {
                    break;
                }
            }

            gain += sign * current->state().capture_value;
            gain += sign * promo_value(current->state());

            sign = -sign;
        }

        return gain;
    }


    float Context::history_score(const Move& move) const
    {
        ASSERT(_tt);
        ASSERT(move);
        ASSERT(move != _move);

        return _tt->history_score(*this, move) + COUNTER_MOVE_BONUS * (move == _counter_move);
    }


    /*
     * Track the best score and move so far, return true if beta cutoff.
     */
    bool Context::is_beta_cutoff(const ContextPtr& next_ctxt, score_t score)
    {
        ASSERT(next_ctxt->_ply != 0);

        if (!is_cancelled())
        {
            ASSERT(score >= SCORE_MIN && score <= SCORE_MAX);
            ASSERT(_alpha >= _score); /* invariant */

            if (score > _score)
            {
                if (next_ctxt->is_null_move())
                {
                    /* consecutive null moves not allowed by design */
                    ASSERT(!is_null_move());

                    /* ignore if not fail-high */
                    if (score < _beta)
                        return false;
                }

                if (score > _alpha)
                {
                    if (!next_ctxt->is_null_move())
                    {
                        if (next_ctxt->_retry_above_alpha == RETRY::Reduced)
                        {
                            _retry_next = true;
                            ++_tt->_retry_reductions;

                            /* increment, so that late_move_reduce() skips it */
                            _full_depth_count = next_move_index() + 1;
                        }
                        else if (next_ctxt->_retry_above_alpha == RETRY::PVS && score < _beta)
                        {
                            _retry_next = true;
                            _retry_beta = -score;
                        }

                        if (_retry_next)
                        {
                            /* rewind and search again at full depth */
                            rewind(-1);
                            return false;
                        }

                        if (score >= _beta)
                        {
                            _cutoff_move = next_ctxt->_move;

                            if (is_null_move())
                            {
                                ASSERT(_parent);
                                if (next_ctxt->is_capture()) /* null move refuted by capture */
                                    _parent->_capture_square = next_ctxt->_move.to_square();

                                if (score >= MATE_HIGH)
                                    _parent->_mate_detected = CHECKMATE - score + 1;
                            }
                        }
                    }

                    _alpha = score;
                }

                _score = score;

                if (!next_ctxt->is_null_move())
                {
                    ASSERT(next_ctxt->_move._state == next_ctxt->_state);
                #if LAZY_STATE_COPY
                    next_ctxt->copy_move_state();
                #endif
                    _best = next_ctxt;
                }
            }

            ASSERT(_alpha >= _score); /* invariant */
        }

        return _alpha >= _beta;
    }


    score_t Context::evaluate()
    {
        ++_tt->_eval_count;

        auto score = _evaluate();

        ASSERT(score > SCORE_MIN);
        ASSERT(score < SCORE_MAX);

        /* detect draws by repetition and by fifty-moves rule */

        if (is_repeated() > 0)
        {
            score = 0;
        }
        else if (_fifty >= 100)
        {
            score = 0;
        }

        return score;
    }


    /*
     * Called when there are no more moves available (endgame reached or
     * qsearch has examined all non-quiet moves in the current position).
     */
    score_t Context::evaluate_end()
    {
        /* precondition for calling this function */
        ASSERT(!has_moves());

        /* Can't have ALL moves pruned, the search function is expected to
         * futility- and late-move-prune only after one move has been tried.
         */
        ASSERT(!_pruned_count);

        if (_pruned_count || _move_maker.have_skipped_moves())
        {
            ASSERT(_ply > 0);
            ASSERT(!is_check());

            return evaluate();
        }

        return is_check() ? -CHECKMATE + _ply : 0;
    }


    score_t Context::evaluate_material(bool with_piece_squares)
    {
        if (with_piece_squares)
        {
            /*
             * Flip it from the pov of the side that moved
             * to the perspective of the side-to-move.
             */
            return -eval_material_and_piece_squares(*_state);
        }
        else
        {
            return state().eval_material() * SIGN[state().turn];
        }
    }


    /*
     * Make the capturing move, return false if not legal.
     */
    static bool apply_capture(const State& state, State& next_state, const Move& move)
    {
        state.clone_into(next_state);
        next_state.apply_move(move);

        ASSERT(next_state.turn != state.turn);
        ASSERT(next_state.capture_value > 0);

        return !next_state.is_check(state.turn); /* legal move? */
    }


    static constexpr auto FIRST_EXCHANGE_PLY = PLY_MAX;

    /*
     * NOTE: uses top half of MoveMaker's moves buffers to minimize memory allocations.
     */
    static inline score_t do_exchanges(
        const State&    state,
        Bitboard        mask,
        score_t         gain = 0,
        int             ply = FIRST_EXCHANGE_PLY,
        bool            debug = false)
    {
        ASSERT(popcount(mask) == 1);
        ASSERT(gain >= 0);

        if (size_t(ply) >= MoveMaker::MAX_MOVE)
            return 0;

        mask &= ~state.kings;

        MovesList& moves = MoveMaker::_moves[ply];
        state.generate_pseudo_legal_moves(moves, mask);

        /* sort moves by piece type */
        for (auto& move : moves)
        {
            ASSERT(state.piece_type_at(move.from_square()));
            move._score = state.weight(state.piece_type_at(move.from_square()));
        }
        /* sort lower-value attackers first */
        insertion_sort(moves.begin(), moves.end(),
            [](const Move& lhs, const Move& rhs) {
                return lhs._score < rhs._score;
            });

        if (debug)
        {
            std::ostringstream out;
            out << "\tdo_exchanges (" << Context::_epd(state) << ") gain=" << gain << " ";
            for (const auto& move : moves)
                out << move.uci() << "(" << move._score << ") ";
            Context::log_message(LogLevel::DEBUG, out.str());
        }

        score_t score = 0;
        int moves_count = 0;

        State next_state;

        /* iterate over pseudo-legal moves */
        for (const auto& move : moves)
        {
            ASSERT((BB_SQUARES[move.to_square()] & ~mask) == 0);
            ASSERT((state.kings & BB_SQUARES[move.to_square()]) == 0);

            if (!apply_capture(state, next_state, move))
                continue;

            ++moves_count;

            if (debug)
                Context::log_message(LogLevel::DEBUG, "\t>>> " + move.uci());

            const score_t capturer_value = move._score;
            ASSERT(capturer_value == state.weight(state.piece_type_at(move.from_square())));

            /*
             * If the value of the capture exceed the other's side gain plus the value of the
             * capturing piece there is no need to call ourselves recursively, as even in the
             * worst case scenario of the capturer being taken the difference cannot be offset.
             */
            if (next_state.capture_value > gain + capturer_value)
            {
            #if EXCHANGES_DETECT_CHECKMATE
                if (next_state.is_checkmate())
                {
                    score = CHECKMATE - (ply + 1 - FIRST_EXCHANGE_PLY);

                    if (debug)
                    {
                        std::ostringstream out;
                        out << "\t<<< " << move.uci() << ": CHECKMATE " << score;
                        Context::log_message(LogLevel::DEBUG, out.str());
                    }

                    break; /* impractical to keep looping in hope of a faster mate */
                }
            #endif /* EXCHANGES_DETECT_CHECKMATE */

                score = std::max(score, next_state.capture_value - gain - capturer_value);

                if (debug)
                {
                    std::ostringstream out;
                    out << "\t<<< " << move.uci() << ": " << next_state.capture_value
                        << " - " << gain << " - " << capturer_value;
                    Context::log_message(LogLevel::DEBUG, out.str());
                }

                continue;
            }

            /****************************************************************/
            score_t other = 0;

            /* Call recursively if the capture offsets the opponent's gain. */
            if (next_state.capture_value >= gain)
            {
                next_state.castling_rights = 0;  /* castling do not capture */

                other = do_exchanges(
                    next_state,
                    mask,
                    next_state.capture_value - gain,
                    ply + 1,
                    debug);
            }
            /*****************************************************************/
            if (debug)
            {
                std::ostringstream out;
                out << "\t<<< " << move.uci() << ": "
                    << next_state.capture_value << " - " << other;
                Context::log_message(LogLevel::DEBUG, out.str());
            }

            /* could continue and look for a quicker mate, but impractical */
            if (other < MATE_LOW)
                return -other;

            score = std::max(score, next_state.capture_value - other);
        }

    #if EXCHANGES_DETECT_CHECKMATE
        if (moves_count == 0 && state.is_checkmate())
        {
            score = -CHECKMATE + ply - FIRST_EXCHANGE_PLY;
        }
    #endif /* EXCHANGES_DETECT_CHECKMATE */

        if (debug)
        {
            std::ostringstream out;
            out << "\tscore: " << score;
            Context::log_message(LogLevel::DEBUG, out.str());
        }

        return score;
    }


    /*
     * Evaluate same square exchanges
     */
    static score_t eval_exchanges(const Move& move, bool approximate)
    {
        score_t val = 0;

        if (move)
        {
            ASSERT(move._state);
            ASSERT(move._state->piece_type_at(move.to_square()));

            if (approximate)
            {
                /*
                 * Approximate without "playing" the moves.
                 */
                val = estimate_static_exchanges(*move._state, move._state->turn, move.to_square());
            }
            else
            {
                auto mask = BB_SQUARES[move.to_square()];
                val = do_exchanges(*move._state, mask);
            }
        }

        return val;
    }


    /*
     * Look at all captures the side-to-move can make, "play through"
     * same square exchanges and return lower bound of maximum gain.
     *
     * Skip the exchanges when the value of the captured piece exceeds
     * the value of the capturer.
     */
    static score_t do_captures(const State& state, Bitboard from_mask, Bitboard to_mask)
    {
        static constexpr auto ply = FIRST_EXCHANGE_PLY;
        const auto mask = to_mask & state.occupied_co(!state.turn) & ~state.kings;

        /*
         * Generate all pseudo-legal captures. NOTE: (re) use the static (thread local)
         * buffers declared in the MoveMaker class to keep memory allocations down to a
         * minimum. NOTE: generate... function takes masks in reverse: to, from.
         */
        MovesList& moves = MoveMaker::_moves[ply];
        if (state.generate_pseudo_legal_moves(moves, mask, from_mask).empty())
        {
            return 0;
        }
        /*
         * 1) Go over the captures and assign the "victim" value to each move.
         */
        for (auto& move : moves)
        {
            ASSERT(state.piece_type_at(move.to_square()));
            ASSERT(state.piece_type_at(move.from_square()));

            move._score = state.weight(state.piece_type_at(move.to_square()));
        }

        /*
         * 2) Sort most valuable victims first.
         */
        insertion_sort(moves.begin(), moves.end(), [](const Move& lhs, const Move& rhs) {
            return lhs._score > rhs._score;
        });

        if (DEBUG_CAPTURES)
        {
            std::ostringstream out;
            out << "do_captures (" << Context::_epd(state) << ") ";
            for (const auto& move : moves)
                out << move.uci() << "(" << move._score << ") ";

            Context::log_message(LogLevel::DEBUG, out.str());
        }

        score_t score = 0;
        State next_state;

        /*
         * 3) Go through the moves and "play out" the exchanges.
         */
        for (const auto& move : moves)
        {
            /* do not expect to capture the king */
            ASSERT((state.kings & BB_SQUARES[move.to_square()]) == 0);

        #if !EXCHANGES_DETECT_CHECKMATE
            /* victim values less than what we got so far? bail */
            if (move._score <= score)
            {
                if (DEBUG_CAPTURES)
                {
                    std::ostringstream out;
                    out << "\t" << move.uci() << " " << move._score << " <= " << score;
                    Context::log_message(LogLevel::DEBUG, out.str());
                }

                break;
            }
        #endif /* !EXCHANGES_DETECT_CHECKMATE */

            if (DEBUG_CAPTURES)
                Context::log_message(LogLevel::DEBUG, "*** " + move.uci());

            /****************************************************************/
            if (!apply_capture(state, next_state, move))
                continue;

            ASSERT(move._score == next_state.capture_value);
            ASSERT(next_state.capture_value > score || EXCHANGES_DETECT_CHECKMATE);

            auto attacker_value = state.weight(state.piece_type_at(move.from_square()));
            auto gain = next_state.capture_value - attacker_value;

            /*
             * Worst case scenario the attacker gets captured, capturing
             * side still has a nice gain; skip "playing" the exchanges.
             */
            if (gain > 0)
            {
            #if !EXCHANGES_DETECT_CHECKMATE
                return gain;
            #endif /* !EXCHANGES_DETECT_CHECKMATE */

                if (next_state.is_checkmate())
                    return CHECKMATE - (ply + 1 - FIRST_EXCHANGE_PLY);

                if (DEBUG_CAPTURES)
                    Context::log_message(
                        LogLevel::DEBUG,
                        move.uci() + ": skip exchanges: " + std::to_string(gain));

                if (gain > score)
                    score = gain;

                continue;
            }

            /****************************************************************/
            /* "play through" same square exchanges                         */
            next_state.castling_rights = 0; /* castling moves can't capture */

            const auto other = do_exchanges(
                next_state,
                BB_SQUARES[move.to_square()],
                next_state.capture_value,
                ply + 1,
                DEBUG_CAPTURES);
            /****************************************************************/

            if (other < MATE_LOW)
            {
                if (DEBUG_CAPTURES)
                    Context::log_message(LogLevel::DEBUG, move.uci() + ": checkmate");

                return -other;
            }
            const auto value = next_state.capture_value - other;

            if (value > score)
                score = value;

            if (DEBUG_CAPTURES)
            {
                std::ostringstream out;
                out << "\t" << move.uci() << ": " << value << " ("
                    << next_state.capture_value << " - " << other
                    << ") score: " << score;

                Context::log_message(LogLevel::DEBUG, out.str());
            }
        }

        return score;
    }


    /*
     * Evaluate the most valuable hanging piece, then call do_captures.
     */
    static score_t eval_captures(Context& ctxt)
    {
        if (DEBUG_CAPTURES)
            ctxt.log_message(LogLevel::DEBUG, "eval_captures");

        auto state = ctxt._state;

    #if NO_ASSERT
        if (ctxt._tid == 0 && ctxt._tt_entry._capt != SCORE_MIN)
            return ctxt._tt_entry._capt;
    #endif /* NO_ASSERT */

        /* mask of all pieces attacking the hanging piece, if any */
        Bitboard attackers = BB_EMPTY;

        const auto result = STATIC_EXCHANGES
            ? estimate_captures(*state)
            : do_captures(*state, BB_ALL, BB_ALL);

        ASSERT(result >= 0);

        if (DEBUG_CAPTURES)
            ctxt.log_message(LogLevel::DEBUG, "captures: " + std::to_string(result));

        ASSERT(ctxt._tt_entry._capt == SCORE_MIN || ctxt._tt_entry._capt == result);
        ctxt._tt_entry._capt = result;

        return result;
    }


    /*----------------------------------------------------------------------
     * Tactical evaluations.
     * All tactical scores are computed from the white side's perspective.
     *----------------------------------------------------------------------*/
    static inline score_t eval_center(const State& state, int pc)
    {
        score_t score = 0;

        for (auto color: {BLACK, WHITE})
        {
            score += SIGN[color]
                * popcount(state.occupied_co(color) & BB_CENTER)
                * interpolate(pc, CENTER_OCCUPANCY, 0);

            for_each_square(BB_CENTER, [&](Square square) {
                score += SIGN[color]
                    * popcount(state.attackers_mask(color, square))
                    * interpolate(pc, CENTER_ATTACKS, 0);
            });
        }
        return score;
    }

    /*
     * Count friendly pieces minus opponent's pieces in the king
     * quadrant, as an approximate measure of the king's safety.
     */
    static inline score_t eval_king_quadrant(const State& state)
    {
        score_t score[] = {0, 0};

        for (auto color: {BLACK, WHITE})
        {
            const auto ours = state.occupied_co(color);
            const auto theirs = state.occupied_co(!color);
            const auto king_mask = state.kings & ours;

            for (auto q : BB_QUANDRANTS)
            {
                if (king_mask & q)
                {
                    q &= ~king_mask;

                    score[color] = popcount(q & ours) - popcount(q & theirs)
                        /* count queens as 2 pieces */
                        + popcount(q & state.queens & ours)
                        - popcount(q & state.queens & theirs);

                    break;
                }
            }
        }

        return score[WHITE] - score[BLACK];
    }


    /*
     * Add per-rank penalty for "developing" the king to each rank past 2nd.
     */
    static inline score_t eval_midgame_king_out(const State& state, int piece_count)
    {
        ASSERT(!state.is_endgame());

        score_t score[] = { 0, 0 };

        for (auto color : { BLACK, WHITE })
        {
            const auto king = state.king(color);
            const auto kings_rank = chess::square_rank(king);
            ASSERT(kings_rank >= 0 && kings_rank <= 7);

            const auto ranks_out = color ? kings_rank : 7 - kings_rank;

            if (ranks_out > 1)
            {
                score[color] += interpolate(piece_count, MIDGAME_KING_OUT_PENALTY, 0) * ranks_out;
            }
        }

        return score[WHITE] - score[BLACK];
    }


    static inline score_t eval_open_files(const State& state, int piece_count)
    {
        static constexpr Bitboard opposite_backranks[] = {
            BB_RANK_2 | BB_RANK_1,
            BB_RANK_7 | BB_RANK_8,
        };

        score_t score[] = { 0, 0 }; /* black, white */

        for (auto color : { BLACK, WHITE })
        {
            const auto own_color_mask = state.occupied_co(color);
            const auto their_color_mask = state.occupied_co(!color);

            for (const auto file_mask : BB_FILES)
            {
                /* no bonus if own king on file */
                if (file_mask & own_color_mask & state.kings)
                    continue;

                if (auto mask = file_mask & (state.rooks | state.queens) & own_color_mask)
                {
                    if ((file_mask & state.pawns) == BB_EMPTY)
                        score[color] += interpolate(piece_count, MIDGAME_OPEN_FILE, 0);
                    else
                        if ((file_mask & state.pawns & own_color_mask) == BB_EMPTY)
                            score[color] += interpolate(piece_count, MIDGAME_HALF_OPEN_FILE, 0);
                    else
                        if ((file_mask & state.pawns & their_color_mask) == BB_EMPTY)
                        {
                            /*
                             * File has only pawns of own color, and the piece has direct "line of sight"
                             * to opposite ranks (i.e. in front of its own pawns)? treat it as open file.
                             */
                            if (for_each_square_r<bool>(mask, [&](Square square) {
                                return bool(state.attacks_mask(square) & file_mask & opposite_backranks[color]);
                            }))
                                score[color] += interpolate(piece_count, MIDGAME_OPEN_FILE, 0);
                        }
                }
            }
        }

        return score[WHITE] - score[BLACK];
    }


    static score_t eval_material_imbalance(const State& state, score_t mat_eval)
    {
        score_t score = 0;

        if (abs(mat_eval) <= WEIGHT[PAWN])
        {
            for (auto color : { BLACK, WHITE })
            {
                score +=
                    SIGN[color] * REDUNDANT_ROOK
                    * (popcount((state.bishops | state.knights) & state.occupied_co(!color)) >= 2)
                    * (popcount(state.rooks & state.occupied_co(color)) >= 2);
            }

            if (DEBUG_MATERIAL && score)
            {
                std::ostringstream out;
                out << "rook: " << Context::_epd(state) << ": " << score << ", mat: " << mat_eval;
                Context::log_message(LogLevel::DEBUG, out.str());
            }
        }

        return score;
    }


    static score_t eval_pawn_majority(const State& state, int piece_count)
    {
        auto score =
            popcount(state.pawns & state.occupied_co(WHITE))
          - popcount(state.pawns & state.occupied_co(BLACK));

        score = (score != 0) * SIGN[score > 0];
        return score * interpolate(piece_count, MIDGAME_PAWN_MAJORITY, ENDGAME_PAWN_MAJORITY);
    }


    /*
     * Allow apps on top of the engine to implement strength levels by "fuzzing" the evaluation.
     */
    static inline score_t eval_fuzz()
    {
        static std::random_device rd;  // Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); // Standard mersenne_twister_engine seeded with rd()

        return EVAL_FUZZ ? std::uniform_int_distribution<score_t>(-EVAL_FUZZ, EVAL_FUZZ)(gen) : 0;
    }


    inline score_t eval_passed_formation(const State& state, int piece_count)
    {
        const auto diff =
            state.longest_pawn_sequence(state.occupied_co(WHITE) & (BB_RANK_6 | BB_RANK_7)) -
            state.longest_pawn_sequence(state.occupied_co(BLACK) & (BB_RANK_2 | BB_RANK_3));

        return diff * interpolate(piece_count, MIDGAME_PASSED_FORMATION, ENDGAME_PASSED_FORMATION);
    }


    inline score_t eval_passed_pawns(const State& state, int piece_count)
    {
        struct {
            Bitboard mask[2];
            score_t  bonus[2];
        } static const ranks[] = {
            {   /* 6th rank */
                { BB_RANK_3, BB_RANK_6 },
                { MIDGAME_UNBLOCKED_PASSED_6, ENDGAME_UNBLOCKED_PASSED_6 }
            },
            {   /* 7th rank */
                { BB_RANK_2, BB_RANK_7 },
                { MIDGAME_UNBLOCKED_PASSED_7, ENDGAME_UNBLOCKED_PASSED_7 }
            },
        };

        score_t score[] = { 0, 0 }; /* black, white */
        const auto occupied_mask = state.occupied();

        for (auto color : { BLACK, WHITE })
        {
            const auto own_mask = state.occupied_co(color);
            const auto others_mask = state.occupied_co(!color);

            for (const auto& rank : ranks)
            {
                const auto pawns = state.pawns & own_mask & rank.mask[color];

                for_each_square(pawns, [&](Square pawn) {
                    const auto pawn_mask = BB_SQUARES[pawn];
                    const auto advance_mask = color ? shift_up(pawn_mask) : shift_down(pawn_mask);

                    if ((advance_mask & occupied_mask) == 0 || (state.attacks_mask(pawn) & others_mask))
                        score[color] += interpolate(piece_count, rank.bonus[0], rank.bonus[1]);
                });
            }
        }

        return score[WHITE] - score[BLACK] + eval_passed_formation(state, piece_count);
    }


    static score_t eval_tactical(const State& state, score_t mat_eval)
    {
        const auto piece_count = popcount(state.occupied());
        score_t eval = eval_passed_pawns(state, piece_count);

        eval += eval_center(state, piece_count);
        eval += eval_material_imbalance(state, mat_eval);
        eval += eval_pawn_majority(state, piece_count);

        eval += BISHOP_PAIR * state.diff_bishop_pairs();
        eval += BISHOPS_STRENGTH * state.diff_bishops_strength();

        eval += state.diff_connected_rooks() *
            interpolate(piece_count, MIDGAME_CONNECTED_ROOKS, ENDGAME_CONNECTED_ROOKS);

        eval += eval_king_quadrant(state) *
            interpolate(piece_count, MIDGAME_KING_QUADRANT, ENDGAME_KING_QUADRANT);

        eval += state.diff_doubled_pawns() *
            interpolate(piece_count, MIDGAME_DOUBLED_PAWNS, ENDGAME_DOUBLED_PAWNS);

        if (state.is_endgame())
        {
            /*
             * Don't penalize isolated pawns if passed, as they
             * may be advantageous in king and pawns endgames.
             * https://en.wikipedia.org/wiki/Passed_pawn
             */
            eval += ENDGAME_ISOLATED_PAWNS * (
                state.count_isolated_pawns(WHITE, ~BB_PASSED[WHITE]) -
                state.count_isolated_pawns(BLACK, ~BB_PASSED[BLACK]));
        }
        else
        {
            eval += eval_midgame_king_out(state, piece_count);
            eval += eval_open_files(state, piece_count);

            eval += state.diff_isolated_pawns() *
                interpolate(piece_count, MIDGAME_ISOLATED_PAWNS, ENDGAME_ISOLATED_PAWNS);
        }

        return eval;
    }


    static inline score_t eval_tactical(Context& ctxt)
    {
        return eval_tactical(ctxt.state(), ctxt.evaluate_material());
    }


    /*
     * Static evaluation has three components:
     * 1. base = material + pst + mobility
     * 2. tactical
     * 3. capture estimates.
     */
    score_t Context::_evaluate()
    {
        auto eval = _tt_entry._eval;

        if (eval == SCORE_MIN)
        {
            _tt->_eval_depth = _ply;

            /* 1. Material + piece-squares + mobility */
            eval = state().eval();

            ASSERT(eval > SCORE_MIN);
            ASSERT(eval < SCORE_MAX);

            if (abs(eval) < MATE_HIGH)
            {
                eval += eval_fuzz();

                const auto turn = state().turn;

                if (state().is_endgame() && state().has_insufficient_material(turn))
                {
                    if (state().has_insufficient_material(!turn))
                    {
                        eval = 0;
                    }
                    else
                    {
                        /* cannot do better than draw, but can do worse */
                        eval = std::min(eval, 0);
                    }
                    _tt_entry._eval = eval;
                }
                /* 2. Tactical (skip in midgame if above LAZY_EVAL_MARGIN or very low depth) */
                else if (state().is_endgame() || (abs(eval) < LAZY_EVAL_MARGIN && depth() > 3))
	            {
                    eval += SIGN[turn] * eval_tactical(*this);
                    eval -= CHECK_BONUS * is_check();

                    _tt_entry._eval = eval;
                }
            }
        }

        ASSERT(eval > SCORE_MIN);
        ASSERT(eval < SCORE_MAX);

        if (abs(eval) < MATE_HIGH)
        {
            /* 3. Captures */
            const auto capt = eval_captures(*this);

            ASSERT(capt <= CHECKMATE);

            if (capt >= MATE_HIGH)
                eval = capt - 1;
            else
                eval += capt;

            ASSERT(eval > SCORE_MIN);
            ASSERT(eval < SCORE_MAX);
        }

        return eval;
    }


    /*
     * Fractional extensions: https://www.chessprogramming.org/Extensions
     * "[...] extension can be added that does not yet extend the search,
     * but further down the tree may cause an extension when another
     * fractional extension causes the net extension to exceed one ply."
     */
    void Context::extend()
    {
        ASSERT(!is_extended());

        if (_extension || (_ply && depth() > 6))
        {
            /*
             * things that could add up interestingness along the search path
             */
            _extension += state().pushed_pawns_score;
            _extension += _move.from_square() == _parent->_capture_square;
            _extension += _move._group == MoveOrder::LOSING_CAPTURES;

            _extension += is_recapture() * (is_pv_node() * (ONE_PLY - 1) + 1);
            _extension += is_singleton() * (is_pv_node() + 1) * ONE_PLY / 2;

            /*
             * extend if move has high historically high cutoffs percentages and counts
             */
            _extension += ONE_PLY * (
                   _move == _parent->_tt_entry._hash_move
                && abs(_parent->_tt_entry._value) < MATE_HIGH
                && _parent->history_score(_move) > HISTORY_HIGH
                && get_tt()->historical_counters(*_parent, _move).first > 10000);

            const auto extend = std::min(1, _extension / ONE_PLY);

            ASSERT(extend >= 0);

            _max_depth += extend;
            _extension %= ONE_PLY;
        }
    }


    score_t Context::futility_margin()
    {
        ASSERT(can_forward_prune());

        if (_ply == 0 || !_futility_pruning)
            return 0;

        const auto d = depth();

        if (d < 1)
            return 0;

        /*
         * No need to check for futile moves when material is above alpha,
         * since no move can immediately result in decrease of material
         * (margin of one PAWN for piece-square evaluation).
         */
        if (evaluate_material() > std::max(_alpha, _score) + WEIGHT[PAWN])
            return 0;

        // return 50 * d + M_PI_2 * pow(depth, M_E);
        // return 50 * d + pow(depth + M_PI_2, M_E);
        return 50 * d + pow(d + 1.61803, M_E);
    }


    /*
     * Return the first legal move, wrapped in a Context object.
     * Used when the search has fails to find a move before the time runs out.
     */
    ContextPtr Context::first_move()
    {
        rewind(0);
        return next(false, true /* suppress time-checking callback */);
    }


    void Context::cancel()
    {
        _cancel = true;
    }


#if ADAPTIVE_NULL_MOVE
    /*
     * Reduction formula based on ideas from SF and others.
     */
    static inline int null_move_reduction(Context& ctxt)
    {
        return NULL_MOVE_REDUCTION
            + ctxt.depth() / 6
            + std::min(3, (ctxt.evaluate_material() - ctxt._beta) / NULL_MOVE_FACTOR);
    }
#endif /* ADAPTIVE_NULL_MOVE */


    /*
     * Get the next move and wrap it into a Context object.
     */
    ContextPtr Context::next(bool null_move, bool first_move, score_t futility)
    {
        ASSERT(first_move || _alpha < _beta);

        const bool retry = _retry_next;
        _retry_next = false;

        if (!first_move && !_excluded && !on_next())
            return nullptr;

        /* null move must be tried before actual moves */
        ASSERT(!null_move || next_move_index() == 0);

        const auto move = null_move ? nullptr : get_next_move(futility);
        if (!move && !null_move)
            return nullptr;

        ASSERT(null_move || move->_state);
        ASSERT(null_move || move->_group != MoveOrder::UNDEFINED);
        ASSERT(null_move || move->_group < MoveOrder::UNORDERED_MOVES);

        auto ctxt = intrusive_ptr<Context>(new Context);

        if (move)
        {
            ASSERT(move->_state);
            ASSERT(ctxt->_is_null_move == false);

            ctxt->_move = *move;

        #if LAZY_STATE_COPY
            ctxt->_state = move->_state;
        #else
            ctxt->copy_move_state();
        #endif /* LAZY_STATE_COPY */

            ctxt->_leftmost = is_leftmost() && next_move_index() == 1;
        }
        else
        {
            ASSERT(null_move);
            ASSERT(!ctxt->_move);

            state().clone_into(ctxt->_statebuf);
            ctxt->_state = &ctxt->_statebuf;
            flip(ctxt->_state->turn);
            ctxt->_is_null_move = true;
        }

        ctxt->_algorithm = _algorithm;
        ctxt->_tid = _tid;
        ctxt->_parent = this;
        ctxt->_max_depth = _max_depth;
        ctxt->_ply = _ply + 1;
        ctxt->_extension = _extension;
        ctxt->_move_maker.set_ply(ctxt->_ply);
        ctxt->_is_retry = retry;
        ctxt->_is_singleton = !ctxt->is_null_move() && _move_maker.is_singleton(*this);
        ctxt->_history = _history;
        ctxt->_futility_pruning = _futility_pruning && FUTILITY_PRUNING;
        ctxt->_multicut_allowed = _multicut_allowed && MULTICUT;

        for (auto side : { BLACK, WHITE})
            ctxt->_null_move_allowed[side] = _null_move_allowed[side] && (NULL_MOVE_REDUCTION > 0);

        ctxt->_tt = _tt;

        ctxt->_alpha = -_beta;
        ctxt->_beta = -_alpha;

        if (ctxt->is_null_move())
        {
            ASSERT(null_move);
            ASSERT(NULL_MOVE_REDUCTION > 0);

            ctxt->_beta = -_beta + 1;

        #if ADAPTIVE_NULL_MOVE
            ASSERT(depth() >= 0);
            ctxt->_max_depth -= std::min(depth(), null_move_reduction(*this));

            /*
             * Allow the null-move to descend one ply into qsearch, to get the
             * mate detection side-effect; forward pruning is disabled when mate
             * is detected, thus enabling the engine to understand better quiet
             * moves that involve material sacrifices.
             */
            ASSERT(ctxt->depth() >= -1);
        #else
            if (ctxt->depth() > 7)
                --ctxt->_max_depth; /* reduce more */

            ctxt->_max_depth -= NULL_MOVE_REDUCTION;

        #endif /* !ADAPTIVE_NULL_MOVE */
        }
        else
        {
            ASSERT(move);

            if (ctxt->is_pvs_ok())
            {
                ctxt->_alpha = -_alpha - 1;
                ctxt->_retry_above_alpha = RETRY::PVS;
                ASSERT(ctxt->_alpha == ctxt->_beta - 1);
            }
            else if (ctxt->is_retry() && _retry_beta < _beta)
            {
                ctxt->_beta = _retry_beta;
                _retry_beta = SCORE_MAX;
            }

            /*
             * https://en.wikipedia.org/wiki/Fifty-move_rule
             */
            if (FIFTY_MOVES_RULE)
            {
                if (ctxt->is_capture())
                    ctxt->_fifty = 0;
                else if (state().pawns & BB_SQUARES[move->from_square()])
                    ctxt->_fifty = 0;
                else
                    ctxt->_fifty = (_ply == 0) ? _history->_fifty : _fifty + 1;
            }
        }

        if (!first_move)
        {
            ASSERT(ctxt->_alpha < ctxt->_beta);
            ASSERT(ctxt->_alpha >= ctxt->_score);
            ASSERT(ctxt->_score == SCORE_MIN);
        }

        return ctxt;
    }


    /*
     * Reinitialize top context at the start of a new iteration.
     */
    void Context::reinitialize()
    {
        ASSERT(_ply == 0);
        ASSERT(!_is_null_move);
        ASSERT(_tt->_w_alpha <= _alpha);
        ASSERT(iteration());
        ASSERT(_retry_above_alpha == RETRY::None);

        _best.reset();
        _cancel = false;

        _capture_square = Square::UNDEFINED;
        _cutoff_move = Move();

        _extension = 0;
        _full_depth_count = LATE_MOVE_REDUCTION_COUNT;

        _max_depth = iteration();

        _mate_detected = 0;
        _multicut_allowed = true;

        _null_move_allowed[WHITE] = true;
        _null_move_allowed[BLACK] = true;

        _repetitions = -1;

        _retry_next = false;
        _retry_beta = SCORE_MAX;

        rewind(0, true);
    }


    int Context::rewind(int where, bool reorder)
    {
        return _move_maker.rewind(*this, where, reorder);
    }


    void Context::set_search_window(score_t score, bool reset)
    {
        if (!ASPIRATION_WINDOW || reset || iteration() == 1)
        {
            _alpha = SCORE_MIN;
            _beta = SCORE_MAX;
        }
        else
        {
            if (_mate_detected % 2)
            {
                _alpha = _tt->_w_alpha;
                _beta = SCORE_MAX;
            }
            else if (_mate_detected)
            {
                _alpha = SCORE_MIN;
                _beta = _tt->_w_beta;
            }
            else if (score <= _tt->_w_alpha)
            {
                _alpha = std::max(SCORE_MIN, score - HALF_WINDOW * pow2(iteration()));
                _beta = _tt->_w_beta;
            }
            else if (score >= _tt->_w_beta)
            {
                _alpha = _tt->_w_alpha;
                _beta = std::min(SCORE_MAX, score + HALF_WINDOW * pow2(iteration()));
            }
            else
            {
                _alpha = std::max(SCORE_MIN, score - HALF_WINDOW);
                _beta = std::min(SCORE_MAX, score + HALF_WINDOW);
            }
        }

        /* save iteration bounds */
        _tt->_w_alpha = _alpha;
        _tt->_w_beta = _beta;

        _score = SCORE_MIN;
    }


    bool Context::should_verify_null_move() const
    {
        return depth() >= NULL_MOVE_MIN_VERIFICATION_DEPTH;
    }


    score_t Context::singular_margin() const
    {
        return depth() * SINGULAR_MARGIN;
    }


    bool Context::can_forward_prune() const
    {
        return _parent
            && !_parent->_mate_detected
            && !is_pv_node()
            && (_max_depth >= 6 || !is_qsearch())
            && !_excluded
            && state().pushed_pawns_score <= 1
            && !state().just_king_and_pawns()
            && !is_check();
    }


    bool Context::can_prune(int count) const
    {
        ASSERT(_ply > 0);
        ASSERT(!is_null_move());
        ASSERT(_move);

        if (is_singleton() || is_extended() || is_pv_node() || is_repeated())
            return false;

        return _parent->can_prune_move(_move, count);
    }


    bool Context::can_prune_move(const Move& move, int /* count */) const
    {
        ASSERT(move);
        ASSERT(move._state);
        ASSERT(move != _move);

        ASSERT(repeated_count(*move._state) == 0);
        ASSERT(can_forward_prune());

        if (move.promotion()
            || (move._state->capture_value != 0)
            || (move._group <= MoveOrder::HISTORY_COUNTERS)
            || (move._group == MoveOrder::TACTICAL_MOVES)
            || (move.from_square() == _capture_square)
            || (move == _counter_move)
            || (move == _tt_entry._hash_move)
            || (move._state->is_check())
           )
            return false;

        ASSERT(!is_check());
        ASSERT(move.from_square() != _capture_square);
        ASSERT(move != _counter_move);

        return true;
    }


    /*
     * Can the current move be searched with reduced depth?
     */
    bool Context::can_reduce() const
    {
        if (_ply == 0 || is_retry() || is_singleton() || is_null_move())
            return false;

        if (_move._group == LOSING_CAPTURES || _move._group == KILLER_MOVES)
            return false;

        if (state().pushed_pawns_score > 1)
            return false;

        if (is_extended())
            return false;

        /* evades capture by the null-move refutation */
        if (_move.from_square() == _parent->_capture_square)
            return false;

        return is_pseudo_quiet(state(), this);
    }


    void Context::copy_move_state()
    {
        ASSERT(_move);
        ASSERT(_move._state);

        _statebuf = *_move._state;
        _state = &_statebuf;
        _move._state = _state;
    }


    bool Context::is_evasion() const
    {
        return _parent && _parent->is_check();
    }


    bool Context::is_quiet() const
    {
        return ::search::is_quiet(state(), this);
    }


    int Context::repeated_count(const State& state) const
    {
        ASSERT(_history);
        return int(_history->count(state)) + has_cycle(state);
    }


    int Context::is_repeated() const
    {
        if (_repetitions < 0)
        {
            ASSERT(_history);
            _repetitions = repeated_count(state());
        }
        return _repetitions;
    }


    /*
     * Ok to generate a null-move?
     */
    bool Context::is_null_move_ok()
    {
        if (!_null_move_allowed[turn()]
            || _ply == 0
            || _excluded
            || is_null_move() /* consecutive null moves are not allowed */
            || is_singleton()
            || is_qsearch()
            || is_pv_node()
            || is_mate_bound()
            || is_repeated()
            || is_check()
            || state().just_king_and_pawns()
           )
            return false;

        ASSERT(depth() >= 0);
        return evaluate_material() >= _beta - 20 * depth() + NULL_MOVE_MARGIN;
    }


    /*
     * Late move reduction and pruning.
     * https://www.chessprogramming.org/Late_Move_Reductions
     */
    LMR Context::late_move_reduce(bool prune)
    {
        ASSERT(!is_null_move());
        ASSERT(_parent);

        const int to_horizon = depth();

        /* no reduction / pruning in qsearch */
        if (to_horizon < 0)
            return LMR::None;

        ASSERT(depth() >= 0);

        const auto count = _parent->next_move_index();

        prune = prune && count > 1;

        /* counter-based late move pruning */
        if (prune && to_horizon < int(LMP._size) && count >= LMP._counters[to_horizon])
        {
            prune = prune && can_prune(count);
            if (prune)
                return LMR::Prune;
        }

        if (to_horizon < 2)
            return LMR::None;

        if (count < _parent->_full_depth_count || !can_reduce())
            return LMR::None;

        const auto reduction =
            std::max(LATE_MOVE_REDUCTION, int(sqrt(3.0 * count) / to_horizon));

        /* reduction drops to qsearch? prune it. */
        if (prune && to_horizon < reduction)
        {
            prune = prune && can_prune(count);
            if (prune)
                return LMR::Prune;
        }

        _max_depth -= reduction;
        /*
         * https://www.chessprogramming.org/Late_Move_Reductions
         * "Classical implementation assumes a re-search at full depth
         * if the reduced depth search returns a score above alpha."
         */
        _retry_above_alpha = RETRY::Reduced;

        ++_tt->_reductions;

        return LMR::Ok;
    }


    bool Context::is_last_move()
    {
        return _parent && _parent->_move_maker.is_last(*_parent);
    }


    /*
     * Treat this node as a leaf or keep recursing down the graph?
     *
     * Once depth exceeds _max_depth only captures and some tactical
     * moves (such as pushed pawns) will be searched.
     */
    bool Context::is_leaf()
    {
        if (_ply == 0)
            return false;

        if (_ply + 1 >= PLY_MAX)
            return true;

        /* the only available move */
        if (is_singleton() && _ply <= 1)
            return true;

        if (_fifty >= 100 || is_repeated())
            return true;

        if (_ply < _max_depth || is_null_move())
            return false;

        if (is_extended() || is_retry() || is_promotion() || is_check())
            return false;

        /*
         * last move to search from current node, with score close to mate?
         * extend the search as to not miss a possible mate in the next move
         */
        if (_parent->_score < MATE_LOW && _parent->_score > SCORE_MIN && is_last_move())
            return false;

        /* treat as leaf for now but retry and extend if it beats alpha */
        if (is_reduced())
            _retry_above_alpha = RETRY::Reduced;

        return true;
    }


    /*
     * https://en.wikipedia.org/wiki/Extended_Position_Description
     */
    std::string Context::epd() const
    {
        return cython_wrapper::call(_epd, state());
    }


    const Move* Context::get_next_move(score_t futility)
    {
        auto move = _move_maker.get_next_move(*this, futility);

        if (move && *move == _excluded)
        {
            move = _move_maker.get_next_move(*this, futility);
        }
        return move;
    }


    int64_t Context::check_time_and_update_nps()
    {
        std::unique_lock<std::mutex> lock(_mutex);

        auto now = std::chrono::system_clock::now();
        auto millisec = duration_cast<std::chrono::milliseconds>(now - _time_start).count();

        /*
         * Update nodes-per-second for the this thread.
         */
        if (millisec)
            _tt->set_nps((1000 * _tt->nodes()) / millisec);
        else
            _tt->set_nps(_tt->nodes());

        if (_time_limit > 0 && millisec >= _time_limit)
            _cancel = true;

        return millisec;
    }


    /*
     * Check how much time is left, update NPS, call optional Python callback.
     */
    bool Context::on_next()
    {
        if (_tid == 0 && ++_callback_count >= CALLBACK_COUNT)
        {
            _callback_count = 0; /* reset */

            auto millisec = check_time_and_update_nps();

            if (is_cancelled()) /* time is up? */
                return false;

            if (_on_next)
                cython_wrapper::call(_on_next, _engine, millisec);
        }

        return !is_cancelled();
    }


    /* static */ void Context::set_time_limit_ms(int time_limit)
    {
        /*
         * Time limits can be updated from a different
         * thread, to support pondering in the background.
         */
        std::unique_lock<std::mutex> lock(_mutex);

        _cancel = false;
        _time_start = std::chrono::system_clock::now();
        _time_limit = time_limit;
        _callback_count = 0;
    }


    /*
     * How much time and how many moves left till the next time control?
     */
    void Context::set_time_info(int millisec, int moves)
    {
        ASSERT(_ply == 0);

        if (MANAGE_TIME && _time_limit)
        {
            const auto val = evaluate_material();

            if (val < -100)
            {
                _time_limit = millisec / 15;
            }
            else if (val < 0)
            {
                _time_limit = millisec / 25;
            }
            else
            {
                auto estimated_moves_left = (state().is_endgame() || is_check()) ? 25 : 35;

                _time_limit = millisec / std::max(moves, estimated_moves_left);
            }
        }
    }


    bool Context::has_cycle(const State& state) const
    {
        const auto hash = state.hash();

        for (auto ctxt = _parent; ctxt; ctxt = ctxt->_parent)
        {
            if (hash == ctxt->state().hash() && state == ctxt->state())
            {
                return true;
            }
        }
        return false;
    }


    bool Context::has_passed_pawns() const
    {
        return state().passed_pawns(WHITE, ~(BB_RANK_4 | BB_RANK_5)) != 0
            || state().passed_pawns(BLACK, ~(BB_RANK_4 | BB_RANK_5)) != 0;
    }


    bool Context::is_capture() const
    {
        if (!_move)
            return false;

        ASSERT(!is_null_move());
        ASSERT((!_parent && state().capture_value == 0)
            || _parent->state().is_capture(_move) == (state().capture_value != 0));

        return state().capture_value != 0;
    }


    /*
     * Allow for apps to implement strength levels by slowing down the engine.
     */
    int64_t Context::nanosleep(int nanosec)
    {
#if defined(_POSIX_VERSION)
        timespec delay = { 0, nanosec };
        ::nanosleep(&delay, nullptr);
#else
        /* busy wait */
        const auto start = high_resolution_clock::now();

        while (true)
        {
            const auto now = high_resolution_clock::now();
            const auto count = duration_cast<nanoseconds>(now - start).count();
            if (count >= nanosec)
            {
                /* std::clog << count << std::endl; */
                break;
            }
            check_time_and_update_nps();
            if (is_cancelled())
                break;
        }
#endif /* _POSIX_VERSION */

        return check_time_and_update_nps();
    }


    static void incremental_update(Move& move, const Context& ctxt)
    {
    #if !defined(ENDGAME_INCREASED_PAWN_VALUE)

        ASSERT(move._state);

        if (move._state->simple_score == 0)
        {
            /*
             * Skip at the point of transitioning into endgame
             * and expect eval_simple() to be called lazily.
             */
            if (move._state->is_endgame() != ctxt.state().is_endgame())
            {
                return;
            }

            move._state->eval_apply_delta(move, ctxt.state());

            ASSERT(move._state->simple_score == 0
                || move._state->simple_score == move._state->eval_simple());
        }
        else
        {
            ASSERT(move._state->simple_score == move._state->eval_simple());
        }
    #endif /* ENDGAME_INCREASED_PAWN_VALUE */
    }


    /*---------------------------------------------------------------------
     * MoveMaker
     *---------------------------------------------------------------------*/
    THREAD_LOCAL std::vector<State> MoveMaker::_states[MAX_MOVE];
    THREAD_LOCAL MovesList MoveMaker::_moves[MAX_MOVE];


    void MoveMaker::print_moves(std::ostream& out) const
    {
        out << "ply=" << _ply << ", cnt=" << _count << ", cur=" << _current << "\n";

        for (size_t i = 0; i != moves().size(); ++i)
        {
            const auto& move = moves()[i];

            out << "[" << i << "]=" << move.uci()
                << "(" << move._score
                << "/" << int(move._group) << ") ";
        }
    }


    bool MoveMaker::has_moves(Context& ctxt)
    {
        ensure_moves(ctxt);
        ASSERT(_count >= 0);

        return (_count > 0) && get_move_at(ctxt, 0);
    }


    bool MoveMaker::is_last(Context& ctxt)
    {
        ensure_moves(ctxt);
        ASSERT(_count >= 0);

        return _current >= _count || !get_move_at(ctxt, _current);
    }


    int MoveMaker::current(Context& ctxt)
    {
        ensure_moves(ctxt);
        return _current;
    }


    bool is_direct_check(const Move& move)
    {
        ASSERT(move);
        ASSERT(move._state);

        const auto& state = *move._state;
        if (state.attacks_mask(move.to_square()) & state.kings & state.occupied_co(state.turn))
        {
            ASSERT(state.is_check());
            return true;
        }

        return false;
    }


    void MoveMaker::make_capture(Context& ctxt, Move& move, score_t opponent_gain)
    {
        if (make_move(ctxt, move))
        {
            ASSERT(move._state->capture_value);

            /* Use part of the static eval as the sorting score. */
            move._score = eval_material_and_piece_squares(*move._state);

            /*
             * Now determine which capture group it belongs to.
             */
            auto capture_gain = move._state->capture_value;

            /* Subtract gain from captures made by the opponent. */
            capture_gain -= opponent_gain;

            if (capture_gain >= 0)
            {
                /* Subtract re-captures by other side. */
                const auto other = eval_exchanges(move, true /* SEE */);

                if (other < MATE_LOW)
                {
                    move._score = -other;
                    move._group = WINNING_CAPTURES;

                    if (DEBUG_CAPTURES)
                        ctxt.log_message(
                            LogLevel::DEBUG,
                            move.uci() + ": " + std::to_string(move._score) + " " + ctxt.epd());
                    return;
                }
                capture_gain -= other;
            }

            if (capture_gain < 0)
            {
                move._group = LOSING_CAPTURES;
            }
            else
            {
            #if 0
                move._group = capture_gain > 0 ? WINNING_CAPTURES : EQUAL_CAPTURES;
            #else
                if (move._group > 0)
                {
                    move._group = MoveOrder::WINNING_CAPTURES;
                    move._score = capture_gain;
                }
                else
                {
                    move._group = MoveOrder::EQUAL_CAPTURES;
                }
            #endif

                /*
                 * Add sorting score bonus for capturing
                 * the last piece moved by the opponent.
                 */
                if (capture_gain > ctxt.state().weight(PAWN)
                    && move.to_square() == ctxt._move.to_square())
                {
                    move._score += CAPTURE_LAST_MOVED_BONUS;
                }
            }
        }
    }


    void MoveMaker::mark_as_illegal(Move& move)
    {
        move._group = MoveOrder::ILLEGAL_MOVES;

        ASSERT(_count > 0);
        --_count;
    }


    /*
     * Return false if the move is not legal, or pruned.
     */
    bool MoveMaker::make_move(Context& ctxt, Move& move, score_t futility)
    {
        ASSERT(move);
        ASSERT(move._group == MoveOrder::UNORDERED_MOVES);

        _need_sort = true;

        if (move._state == nullptr)
        {
            /* assign state buffer */
            ASSERT(_state_index < states().size());
            move._state = &states()[_state_index++];
        }
        else
            return (_have_move = true);

        ctxt.state().clone_into(*move._state);

        ASSERT(move._state->capture_value == 0);

        /* capturing the king is an illegal move (Louis XV?) */
        if (move._state->kings & BB_SQUARES[move.to_square()])
        {
            mark_as_illegal(move);
            return false;
        }

        move._state->apply_move(move);

        if (move._state->is_check(ctxt.turn()))
        {
            mark_as_illegal(move); /* can't leave the king in check */
            return false;
        }

        if (_group_quiet_moves && is_quiet(*move._state))
        {
            _have_quiet_moves = true;
            move._group = MoveOrder::QUIET_MOVES;
            return false;
        }

        incremental_update(move, ctxt);

        /* idea: combine futility pruning with late-move pruning */
        if (futility
            && move._state->capture_value == 0
            && move._state->promotion == 0
            && ctxt.depth() >= 0
            && ctxt.depth() < int(LMP._size)
            && _current >= LMP._counters[ctxt.depth()])
        {
            auto val = futility + move._state->simple_score * SIGN[!move._state->turn];

            if (val < ctxt._alpha || val < ctxt._score)
            {
                _have_pruned_moves = true;
                move._group = MoveOrder::PRUNED_MOVES;
                ++ctxt.get_tt()->_futility_prune_count;

                return false;
            }
        }

        /* consistency check */
        ASSERT((move._state->capture_value != 0) == ctxt.state().is_capture(move));

        ctxt.get_tt()->_nodes += COUNT_VALID_MOVES_AS_NODES;
        return (_have_move = true);
    }


    bool MoveMaker::make_move(Context& ctxt, Move& move, MoveOrder group, score_t score)
    {
        if (!make_move(ctxt, move))
        {
            ASSERT(move._group == MoveOrder::QUIET_MOVES
                || move._group == MoveOrder::ILLEGAL_MOVES);
            return false;
        }

        ASSERT(move._group == MoveOrder::UNORDERED_MOVES);
        move._group = group;
        move._score = score;

        return true;
    }


    void MoveMaker::sort_moves(Context& /* ctxt */, size_t start_at)
    {
        ASSERT(start_at < moves().size());

        auto& moves_list = moves();

        /*
         * Walk backwards skipping over quiet, pruned, and illegal moves.
         */
        auto n = moves_list.size();
        for (; n > start_at; --n)
        {
            if (moves_list[n-1]._group < MoveOrder::QUIET_MOVES)
                break;
        }
        ASSERT(n == moves_list.size() || moves_list[n]._group >= MoveOrder::QUIET_MOVES);
        _count = n;
        auto last = moves_list.begin() + n;
        auto first = moves_list.begin() + start_at;

        insertion_sort(first, last, [&](const Move& lhs, const Move& rhs)
            {
                return (lhs._group == rhs._group && lhs._score > rhs._score)
                    || (lhs._group < rhs._group);
            });

        _need_sort = false;
    }


    void MoveMaker::ensure_moves(Context& ctxt)
    {
        if (_count < 0)
        {
            ASSERT(_current < 0);
            ASSERT(_phase == 0);

            generate_unordered_moves(ctxt);

            ASSERT(_count >= 0);
            ASSERT(_current >= 0);
        }
    }


    const Move* MoveMaker::get_next_move(Context& ctxt, score_t futility)
    {
        ensure_moves(ctxt);

        /* post-condition */
        ASSERT(_current <= _count);

        auto move = get_move_at(ctxt, _current, futility);

        if (move)
        {
            ++_current;
        }
        else
        {
            ASSERT(_current == _count);
        }
        return move;
    }


    int MoveMaker::rewind(Context& ctxt, int where, bool force_reorder)
    {
        ensure_moves(ctxt);

        ASSERT(_count > 0 || where == 0);
        ASSERT(where == 0 || where == -1); /* other cases not supported */

        if (force_reorder)
        {
            ASSERT(where == 0);

            _phase = 0;

            for (int i = 0; i != _count; ++i)
            {
                auto& move = moves()[i];
                if (move._group >= MoveOrder::QUIET_MOVES)
                {
                    _count = i;
                    break;
                }
                move._group = MoveOrder::UNORDERED_MOVES;
                move._score = 0;
            }
        }

        if (where >= 0)
        {
            _current = std::min(where, _count);
        }
        else
        {
            _current = std::max(0, _current + where);
        }

        ASSERT(_current >= 0);
        return _current;
    }


    /*
     * Is the current move the only available move?
     */
    bool MoveMaker::is_singleton(Context& ctxt)
    {
        if (ctxt._tt_entry._singleton)
            return true;

        ASSERT(_current > 0);

        if (_current > 1 || ctxt._pruned_count || have_skipped_moves())
            return false;

        ASSERT(_current == 1);

        if (_count == 1)
        {
        #if !defined(NO_ASSERT)
            /* checked for skipped moves above, make sure there aren't any */
            const auto& all_moves = moves();
            for (const auto& move : all_moves)
            {
                ASSERT(move._group != MoveOrder::QUIET_MOVES);
                ASSERT(move._group != MoveOrder::PRUNED_MOVES);
            }
        #endif /* !(NO_ASSERT) */
            return (ctxt._tt_entry._singleton = true);
        }
        /*
         * get_move_at() is expensive;
         * use only if moving out of check.
         */
        ctxt._tt_entry._singleton =
            ctxt.is_check() && !get_move_at(ctxt, _current) && !have_skipped_moves();

        return ctxt._tt_entry._singleton;
    }


    /*
     * Lookup move in the principal variation from the previous iteration.
     * https://www.chessprogramming.org/PV-Move
     */
    const BaseMove* lookup_pv(const Context& ctxt)
    {
        if (!ctxt.get_tt() || !ctxt._move)
            return nullptr;

        const auto& pv = ctxt.get_tt()->get_pv();
        const size_t ply = ctxt._ply;

        if (ply + 1 >= pv.size())
            return nullptr;

        return pv[ply] == ctxt._move ? &pv[ply + 1] : nullptr;
    }


    void MoveMaker::generate_unordered_moves(Context& ctxt)
    {
        /* pre-conditions */
        ASSERT(_count < 0);
        ASSERT(_current < 0);
        ASSERT(_phase == 0);
        ASSERT(_state_index == 0);
        ASSERT(_ply == ctxt._ply);

        auto& moves_list = moves();
        moves_list.clear();

        if (_initial_moves.empty())
        {
            ctxt.state().generate_pseudo_legal_moves(moves_list);
        }
        else
        {
            moves_list.swap(_initial_moves);

            for (auto& move : moves_list)
            {
                move._state = nullptr;
                move._score = 0;
                move._group = MoveOrder::UNORDERED_MOVES;
            }
        }

        _count = int(moves_list.size());
        _current = 0;

        /* Initialize "scratch" buffers for making the moves. */
        states().resize(_count);

        /*
         * In quiescent search, only quiet moves are interesting.
         * If in check, no need to determine "quieteness", since
         * all legal moves are about getting out of check.
         */
        _group_quiet_moves = (ctxt.is_qsearch() && !ctxt.is_check());

        if (ctxt._ply && !ctxt.is_null_move() && !ctxt._excluded)
            if (const auto move = lookup_pv(ctxt))
                ctxt._prev = *move;
    }


    const MovesList& MoveMaker::get_moves() const
    {
        ASSERT(_count >= 0);
        return moves();
    }


    const Move* MoveMaker::get_move_at(Context& ctxt, int index, score_t futility)
    {
        ensure_moves(ctxt);

        if (index >= _count)
        {
            return nullptr;
        }

        const auto& moves_list = moves();
        ASSERT(!moves_list.empty());

        const Move* move = &moves_list[index];
        ASSERT(move->_group != MoveOrder::UNDEFINED);

        while (move->_group == MoveOrder::UNORDERED_MOVES)
        {
            order_moves(ctxt, index, futility);
            move = &moves_list[index];
        }

        ASSERT(move->_group != MoveOrder::UNORDERED_MOVES);

        if (move->_group >= MoveOrder::QUIET_MOVES)
        {
            ASSERT(index <= _count);
            _count = index;
            move = nullptr;
        }

        return move;
    }


    static inline const Move* match_killer(const KillerMoves* killers, const Move& move)
    {
        if (killers)
            for (size_t i = 0; i != 2; ++i)
                if ((*killers)[i] == move)
                    return &(*killers)[i];

        return nullptr;
    }


    /*
     * Order moves in multiple phases (passes). The idea is to minimize make_move() calls,
     * which validate the legality of the move. The granularity (number of phases) should
     * not be too high, to keep the overhead cost of sorting the moves low.
     */
    void MoveMaker::order_moves(Context& ctxt, size_t start_at, score_t futility)
    {
        static constexpr int MAX_PHASE = 4;

        ASSERT(_phase <= MAX_PHASE);
        ASSERT(ctxt._tt);
        ASSERT(moves()[start_at]._group == MoveOrder::UNORDERED_MOVES);

        score_t same_square_gain = 0;
        const KillerMoves* killer_moves = nullptr;

        /* "confidence bar" for historical scores */
        const auto hist_threshold = HISTORY_HIGH / (1 + exp(6 - ctxt.iteration()));

        _have_move = false;

        while (!_have_move && start_at < size_t(_count) && _phase < MAX_PHASE)
        {
            if (++_phase == 2)
            {
                same_square_gain = ctxt.material_gain(true);
                killer_moves = ctxt._tt->get_killer_moves(ctxt._ply);
            }

            /********************************************************************/
            /* iterate over pseudo-legal moves                                  */
            /********************************************************************/
            auto& moves_list = moves();
            const auto n = moves_list.size();

            for (size_t i = start_at; i < n; ++i)
            {
                auto& move = moves_list[i];

                if (move._group >= MoveOrder::QUIET_MOVES)
                {
                    if (_need_sort)
                        continue;
                    break;
                }

                ASSERT(move._group == MoveOrder::UNORDERED_MOVES);
                ASSERT(move._score == 0);

                switch (_phase)
                {
                case 1:
                    if (move == ctxt._prev)
                    {
                        make_move(ctxt, move, ctxt._ply ? MoveOrder::HASH_MOVES : MoveOrder::PREV_ITER);
                    }
                    else if (ctxt._tt_entry._hash_move == move)
                    {
                        make_move(ctxt, move, MoveOrder::HASH_MOVES);
                    }
                    else if (move.promotion())
                    {
                        make_move(ctxt, move, MoveOrder::PROMOTIONS, WEIGHT[move.promotion()]);
                    }
                    break;

                case 2: /* Captures and killer moves. */
                    if (move._state ? move._state->capture_value : ctxt.state().is_capture(move))
                    {
                        make_capture(ctxt, move, same_square_gain);
                        break;
                    }

                    if (auto k_move = match_killer(killer_moves, move))
                    {
                        make_move(ctxt, move, MoveOrder::KILLER_MOVES, k_move->_score);
                        break;
                    }
                    break;

                case 3:
                    {   /* top historical scores, including counter-move bonus */
                        const auto score = ctxt.history_score(move);
                        if (score > hist_threshold)
                        {
                            make_move(ctxt, move, MoveOrder::HISTORY_COUNTERS, score);
                            break;
                        }
                    }
                    /* tactical */
                    if ((move == ctxt._counter_move
                        || move.from_square() == ctxt._capture_square
                        || (ctxt.state().pawns & BB_PASSED[ctxt.turn()] & BB_SQUARES[move.from_square()])
                        ) && make_move(ctxt, move, MoveOrder::TACTICAL_MOVES)
                       )
                        move._score = ctxt.history_score(move);
                    break;

                case 4:
                    if (!make_move(ctxt, move, futility))
                        break;

                    move._score = ctxt.history_score(move);

                    if (move._state->has_fork(!move._state->turn) || is_direct_check(move))
                        move._group = MoveOrder::TACTICAL_MOVES;
                    else
                        move._group = MoveOrder::LATE_MOVES;

                    break;

                default:
                    std::cerr << "order_moves(): phase=" << _phase << "\n";
                    ASSERT_MESSAGE(false, "unexpected move ordering phase");
                    break;
                }
            }
        }

        ASSERT(_count >= 0);

        if (size_t(_count) <= start_at)
        {
            ASSERT(moves()[start_at]._group >= QUIET_MOVES);
            _need_sort = false;
        }
        else if (_need_sort)
        {
            sort_moves(ctxt, start_at);
        }

    #if !defined(NO_ASSERT)
        for (size_t i = _count; i < moves().size(); ++i)
        {
            ASSERT(moves()[i]._group >= MoveOrder::QUIET_MOVES);
        }
    #endif /* NO_ASSERT */
    }

    /*
     * SMP: copy the root moves from the main thread to the other
     * workers at the beginning of the iteration; also used in the
     * singular extension heuristic.
     */
    void MoveMaker::set_initial_moves(const MovesList& moves)
    {
        /* Expect initial state */
        ASSERT(_initial_moves.empty());
        ASSERT(_count < 0);
        ASSERT(_current == -1);
        ASSERT(_phase == 0);
        ASSERT(_state_index == 0);

        _initial_moves = moves;
    }
} /* namespace */


void cancel_search(CancelReason reason)
{
    search::Context::cancel();

    switch (reason)
    {
    case CancelReason::PY_SIGNAL:
        std::cout << "\ninterrupted\n";
        _exit(1);

    case CancelReason::PY_ERROR:
        std::cout << "\nPython exception:\n";
        PyErr_Print();
        _exit(2);
    }
}
