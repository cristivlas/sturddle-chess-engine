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
#include <chrono>
#include <iomanip>
#include <iterator>
#include <map>
#include <random>
#include <sstream>
#include "chess.h"
#define CONFIG_IMPL
  #include "context.h"
#undef CONFIG_IMPL

using namespace chess;
using search::TranspositionTable;

using std::chrono::duration_cast;
using std::chrono::high_resolution_clock;
using std::chrono::nanoseconds;



/*
 * Late-move reduction tables (adapted from berserk)
 */
struct LMR
{
    int _table[PLY_MAX][64] = { { 0 }, { 0 } };

    LMR()
    {
        for (int depth = 1; depth < PLY_MAX; ++depth)
            for (int moves = 1; moves < 64; ++moves)
                _table[depth][moves] = 0.1 + log(depth) * log(moves) / 2;
    }
} LMR;

/*---------------------------------------------------------------------------
 *
 * Configuration API, for tweaking parameters via Python scripts
 * (such as https://chess-tuning-tools.readthedocs.io/en/latest/)
 *
 *--------------------------------------------------------------------------*/
std::map<std::string, Param> _get_param_info()
{
    std::map<std::string, Param> info;

    for (const auto& elem : Config::_namespace)
    {
        info.emplace(elem.first,
            Param {
                *elem.second._val,
                elem.second._min,
                elem.second._max,
                elem.second._group
            });
    }

    info.emplace("Hash", Param {
        int(TranspositionTable::get_hash_size()),
        HASH_MIN,
        int(TranspositionTable::max_hash_size()),
        "Settings",
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

        if (*iter->second._val != value)
        {
            *iter->second._val = value;

            if (echo)
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


namespace search
{
    /*---------------------------------------------------------------------
     * Context
     *---------------------------------------------------------------------*/
    atomic_bool Context::_cancel(false);
    atomic_bool Context::_reset_window(false);

    atomic_int  Context::_time_limit = -1; /* milliseconds */
    atomic_time Context::_time_start;
    size_t Context::_callback_count(0);

    HistoryPtr Context::_history; /* moves played so far */

    std::vector<Context::ContextStack> Context::_context_stacks(SMP_CORES);
    std::vector<Context::MoveStack> Context::_move_stacks(SMP_CORES);
    std::vector<Context::StateStack> Context::_state_stacks(SMP_CORES);

    /* Cython callbacks */
    PyObject* Context::_engine = nullptr;

    std::string (*Context::_epd)(const State&) = nullptr;
    void (*Context::_log_message)(int, const std::string&, bool) = nullptr;

    void (*Context::_on_iter)(PyObject*, Context*, score_t) = nullptr;
    void (*Context::_on_next)(PyObject*, int64_t) = nullptr;

    std::string(*Context::_pgn)(Context*) = nullptr;
    void (*Context::_print_state)(const State&) = nullptr;
    void (*Context::_report)(PyObject*, std::vector<Context*>&) = nullptr;

    size_t (*Context::_vmem_avail)() = nullptr;


    /* Init attack masks and other magic bitboards in chess.cpp */
    /* static */ void Context::init()
    {
        _init();
    }


    /* Call into the Python logger */
    /* static */ void Context::log_message(LogLevel level, const std::string& msg, bool force)
    {
        cython_wrapper::call(_log_message, int(level), msg, force);
    }


    /*
     * Copy context for two specific use cases:
     * 1) clone root at the beginning of SMP searches, and
     * 2) create a temporary context for singularity search.
     */
    Context* Context::clone(Context* buffer, int ply) const
    {
        Context* ctxt = new (buffer) Context;

        ctxt->_algorithm = _algorithm;
        ctxt->_alpha = _alpha;
        ctxt->_beta = _beta;
        ctxt->_score = _score;
        ctxt->_max_depth = _max_depth;
        ctxt->_parent = _parent;
        ctxt->_ply = ply;
        ctxt->_prev = _prev;
        ctxt->_statebuf = state();
        ctxt->_state = &ctxt->_statebuf;
        ctxt->_move = _move;
        ctxt->_excluded = _excluded;
        ctxt->_tt_entry = _tt_entry;
        ctxt->_counter_move = _counter_move;

        return ctxt;
    }



    /* static */ void Context::ensure_stacks()
    {
        const size_t n_threads(SMP_CORES);
        if (_context_stacks.size() < n_threads)
        {
            _context_stacks.resize(n_threads);
            _move_stacks.resize(n_threads);
            _state_stacks.resize(n_threads);
        }
    }


    /*
     * Track the best score and move so far, return true if beta cutoff;
     * called from search right after: score = -negamax(*next_ctxt).
     */
    bool Context::is_beta_cutoff(Context* next_ctxt, score_t score)
    {
        ASSERT(next_ctxt->_ply != 0);
        ASSERT(score > SCORE_MIN && score < SCORE_MAX);
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

                    #if EXTRA_STATS
                        ++_tt->_retry_reductions;
                    #endif /* EXTRA_STATS */

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

                            if (score >= CHECKMATE - next_ctxt->depth())
                                _parent->_mate_detected = CHECKMATE - score + 1;
                        }
                        else if (const auto counter_move = next_ctxt->next_ply())
                        {
                            /*
                             * give a bonus to non-capturing moves that
                             * beat beta even when countered by captures
                             */
                            if (_ply < PLY_HISTORY_MAX
                                && score > 0
                                && score < MATE_HIGH
                                && score > _beta + HISTORY_BONUS_MARGIN
                                && depth() >= HISTORY_MIN_DEPTH
                                && !next_ctxt->is_capture()
                                && counter_move->is_capture())
                            {
                                get_tt()->_plyHistory[_ply][turn()][next_ctxt->_move] += double(score) / depth();
                            }
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
                _best_move = next_ctxt->_move;
            }
        }

        ASSERT(_alpha >= _score); /* invariant */

        return _alpha >= _beta;
    }


    /*
     * Called when there are no more moves available (endgame reached or
     * qsearch has examined all non-quiet moves in the current position).
     */
    score_t Context::evaluate_end()
    {
        /* precondition for calling this function */
        ASSERT(!has_moves());

        if (_pruned_count || _move_maker.have_skipped_moves())
        {
            ASSERT(_ply > 0);
            ASSERT(!is_check());

            return evaluate();
        }

        return is_check() ? checkmated(_ply) : 0;
    }


    /*
     * Make the capturing move, return false if not legal.
     */
    static bool INLINE apply_capture(const State& state, State& next_state, const Move& move)
    {
        state.clone_into(next_state);
        next_state.apply_move(move);

        ASSERT(next_state.turn != state.turn);
        ASSERT(next_state.capture_value > 0);

        return !next_state.is_check(state.turn); /* legal move? */
    }


    template<bool Debug>
    int do_exchanges(const State& state, Bitboard mask, score_t gain, int tid, int ply)
    {
        ASSERT(popcount(mask) == 1);
        ASSERT(gain >= 0);

        /* use top half of moves stacks */
        ASSERT(ply >= PLY_MAX);

        if (size_t(ply) >= Context::MAX_MOVE)
            return 0;

        mask &= ~state.kings;

        auto& moves = Context::moves(tid, ply);
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

        if constexpr(Debug)
        {
            std::ostringstream out;
            out << "\tdo_exchanges (" << Context::epd(state) << ") gain=" << gain << " ";
            for (const auto& move : moves)
                out << move.uci() << "(" << move._score << ") ";
            Context::log_message(LogLevel::DEBUG, out.str());
        }

        int score = 0;
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

            if constexpr(Debug)
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

                    if constexpr(Debug)
                    {
                        std::ostringstream out;
                        out << "\t<<< " << move.uci() << ": CHECKMATE " << score;
                        Context::log_message(LogLevel::DEBUG, out.str());
                    }

                    break; /* impractical to keep looping in hope of a faster mate */
                }
            #endif /* EXCHANGES_DETECT_CHECKMATE */

                score = std::max(score, next_state.capture_value - gain - capturer_value);

                if constexpr(Debug)
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

                other = do_exchanges<Debug>(
                    next_state,
                    mask,
                    next_state.capture_value - gain,
                    tid,
                    ply + 1);
            }
            /*****************************************************************/
            if constexpr(Debug)
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

        if constexpr(Debug)
        {
            std::ostringstream out;
            out << "\tscore: " << score;
            Context::log_message(LogLevel::DEBUG, out.str());
        }

        return score;
    }


    /*
     * Look at all captures the side-to-move can make, "play through"
     * same square exchanges and return lower bound of maximum gain.
     *
     * Skip the exchanges when the value of the captured piece exceeds
     * the value of the capturer.
     */
    static score_t do_captures(int tid, const State& state, Bitboard from_mask, Bitboard to_mask)
    {
        static constexpr auto ply = FIRST_EXCHANGE_PLY;
        const auto mask = to_mask & state.occupied_co(!state.turn) & ~state.kings;

        /*
         * Generate all pseudo-legal captures. NOTE: (re) use the static (thread local)
         * buffers declared in the MoveMaker class to keep memory allocations down to a
         * minimum. NOTE: generate... function takes masks in reverse: to, from.
         */
        auto& moves = Context::moves(tid, ply);
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

        if constexpr(DEBUG_CAPTURES)
        {
            std::ostringstream out;
            out << "do_captures (" << Context::epd(state) << ") ";
            for (const auto& move : moves)
                out << move.uci() << "(" << move._score << ") ";

            Context::log_message(LogLevel::DEBUG, out.str());
        }

        int score = 0;
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
                if constexpr(DEBUG_CAPTURES)
                {
                    std::ostringstream out;
                    out << "\t" << move.uci() << " " << move._score << " <= " << score;
                    Context::log_message(LogLevel::DEBUG, out.str());
                }

                break;
            }
        #endif /* !EXCHANGES_DETECT_CHECKMATE */

            if constexpr(DEBUG_CAPTURES)
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
            #else
                if (next_state.is_checkmate())
                    return CHECKMATE - (ply + 1 - FIRST_EXCHANGE_PLY);

                if constexpr(DEBUG_CAPTURES)
                    Context::log_message(
                        LogLevel::DEBUG,
                        move.uci() + ": skip exchanges: " + std::to_string(gain));

                if (gain > score)
                    score = gain;

                continue;
            #endif /* EXCHANGES_DETECT_CHECKMATE */
            }

            /****************************************************************/
            /* "play through" same square exchanges                         */
            next_state.castling_rights = 0; /* castling moves can't capture */

            const auto other = do_exchanges<DEBUG_CAPTURES>(
                next_state,
                BB_SQUARES[move.to_square()],
                next_state.capture_value,
                tid,
                ply + 1);
            /****************************************************************/

            if (other < MATE_LOW)
            {
                if constexpr(DEBUG_CAPTURES)
                    Context::log_message(LogLevel::DEBUG, move.uci() + ": checkmate");

                return -other;
            }
            const auto value = next_state.capture_value - other;

            if (value > score)
                score = value;

            if constexpr(DEBUG_CAPTURES)
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


    static INLINE score_t eval_captures(Context& ctxt)
    {
        if constexpr(DEBUG_CAPTURES)
            ctxt.log_message(LogLevel::DEBUG, "eval_captures");

        auto state = ctxt._state;

    #if NO_ASSERT
        if (ctxt._tt_entry._capt != SCORE_MIN)
            return ctxt._tt_entry._capt;
    #endif /* NO_ASSERT */

        score_t result;

        if constexpr(STATIC_EXCHANGES)
            result = estimate_captures(*state);
        else
            result = do_captures(ctxt.tid(), *state, BB_ALL, BB_ALL);

        ASSERT(result >= 0);

        if constexpr(DEBUG_CAPTURES)
            ctxt.log_message(LogLevel::DEBUG, "captures: " + std::to_string(result));

        ASSERT(ctxt._tt_entry._capt == SCORE_MIN || ctxt._tt_entry._capt == result);
        ctxt._tt_entry._capt = result;

        return result;
    }


    /*----------------------------------------------------------------------
     * Tactical evaluations.
     * All tactical scores are computed from the white side's perspective.
     *----------------------------------------------------------------------*/
    static INLINE int eval_center(const State& state, int pc)
    {
        int attacks = 0;
        int occupancy = 0;

        for (auto color : { BLACK, WHITE })
        {
            const auto s = SIGN[color];
            occupancy += s * popcount(state.pawns & state.occupied_co(color) & BB_CENTER);

            for_each_square(BB_CENTER, [&](Square square) {
                attacks += s * popcount(state.pawns & BB_PAWN_ATTACKS[!color][square]);
            });

        }
        return attacks * interpolate(pc, CENTER_ATTACKS, 0)
            + occupancy * interpolate(pc, CENTER_OCCUPANCY, 0);
    }


    static INLINE Bitboard king_area(const State& state, Square king)
    {
        const auto f = square_file(king);
        const auto r = square_rank(king);

        const auto file = BB_FILES[f + (f == 0) - (f == 7)];
        const auto rank = BB_RANKS[r + (r == 0) - (r == 7)];

        return(file | shift_left(file) | shift_right(file))
            & (rank | shift_up(rank) | shift_down(rank))
            & ~BB_SQUARES[king];
    }


    /*
     * Count friendly pieces minus opponent's pieces in the king
     * quadrant, as an approximate measure of the king's safety.
     */
    static INLINE int eval_king_quadrant(const State& state, int pcs)
    {
        int score = 0;

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

                    score += SIGN[color] * (
                        popcount(q & ours) - popcount(q & theirs)
                        /* count queens as 2 pieces */
                        + popcount(q & state.queens & ours)
                        - popcount(q & state.queens & theirs));

                    break;
                }
            }
        }

        return score * interpolate(pcs, MIDGAME_KING_QUADRANT, ENDGAME_KING_QUADRANT);
    }


    static INLINE int eval_king_safety(const State& state, int pcs)
    {
        int attacks = 0; /* attacks around the king */
        int castle  = 0; /* castling rights bonuses */
        int outside = 0; /* penalty for king out of ranks [0,1] */
        int shield  = 0; /* pawn shield bonus */

        const auto occupied = state.occupied();

        for (auto color : { BLACK, WHITE })
        {
            const auto king = state.king(color);
            const auto ranks = color ? square_rank(king) : 7 - square_rank(king);
            const auto area = king_area(state, king);

            const auto color_mask = state.occupied_co(color);

            if (ranks > 1)
            {
                outside += SIGN[color] * (ranks - 1);
            }
            else
            {
                castle += SIGN[color]
                    * (ranks == 0)
                    * popcount(state.castling_rights & BB_BACKRANKS[color]);

                if (const auto pawns = state.pawns & color_mask)
                {
                    shield += SIGN[color]
                        * bool(BB_FILES[square_file(king)] & pawns)
                        * popcount(area & pawns);
                }
            }

            /*
             * https://www.chessprogramming.org/King_Safety  Attacking King Zone
             */
            static constexpr int ATTACK_WEIGHT[8] = { 0, 0, 50, 75, 88, 94, 97, 99 };

            for_each_square(area & ~color_mask, [&](Square square) {
                double attacks_value = 0;

                const auto attackers_mask =
                    state.attacker_pieces_mask(!color, square, occupied);

                for_each_square(attackers_mask, [&](Square attacking_square) {
                    const auto pt = state.piece_type_at(attacking_square);
                    ASSERT(pt > PieceType::PAWN && pt < PieceType::KING);

                    attacks_value += double(WEIGHT[pt] - 100) / KING_ATTACK_DIV;
                });

                const auto attackers = std::min(popcount(attackers_mask), 7);
                attacks -= SIGN[color] * attacks_value * ATTACK_WEIGHT[attackers] / 100;
            });
        }

        return attacks
            + eval_king_quadrant(state, pcs)
            + castle * CASTLING_RIGHTS_BONUS
            + outside * interpolate(pcs, KING_OUT_PENALTY, 0)
            + shield * interpolate(pcs, PAWN_SHIELD, 0);
    }


    /*
     * If the difference in material is with a pawn or less, favor
     * the side with two minor pieces over the side with extra rook.
     */
    static INLINE int eval_redundant_rook(const State& state, int pcs, score_t mat_eval)
    {
        int score = 0;

        if (abs(mat_eval) <= WEIGHT[PAWN])
        {
            for (auto color : { BLACK, WHITE })
            {
                score += SIGN[color]
                    * (popcount((state.bishops | state.knights) & state.occupied_co(!color)) >= 2)
                    * (popcount(state.rooks & state.occupied_co(color)) >= 2);
            }

            if (DEBUG_MATERIAL && score)
            {
                std::ostringstream out;
                out << "rook: " << Context::epd(state) << ": " << score << ", mat: " << mat_eval;
                Context::log_message(LogLevel::DEBUG, out.str());
            }
        }

        return score * interpolate(pcs, 0, REDUNDANT_ROOK);
    }


    static INLINE int eval_open_files(const State& state, int piece_count)
    {
        static constexpr Bitboard opposite_backranks[] = {
            BB_RANK_2 | BB_RANK_1,
            BB_RANK_7 | BB_RANK_8,
        };

        int open_score = 0;
        int half_open_score = 0;

        const auto occupied = state.occupied();

        for (auto color : { BLACK, WHITE })
        {
            const auto s = SIGN[color];

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
                    {
                        open_score += s;
                    }
                    else
                        if ((file_mask & state.pawns & own_color_mask) == BB_EMPTY)
                        {
                            half_open_score += s;
                        }
                    else
                        if ((file_mask & state.pawns & their_color_mask) == BB_EMPTY)
                        {
                            /*
                             * File has only pawns of own color, and the piece has direct "line of sight"
                             * to opposite ranks (i.e. in front of its own pawns)? treat it as open file.
                             */
                            if (for_each_square_r<bool>(mask, [&](Square square) {
                                return bool(state.attacks_mask(square, occupied) & file_mask & opposite_backranks[color]);
                            }))
                                open_score += s;
                        }
                }
            }
        }

        return half_open_score * interpolate(piece_count, MIDGAME_HALF_OPEN_FILE, 0)
            + open_score * interpolate(piece_count, MIDGAME_OPEN_FILE, 0);
    }


    /*
     * Allow apps on top of the engine to implement strength levels by "fuzzing" the evaluation.
     */
    static INLINE int eval_fuzz()
    {
        static std::random_device rd;  // Will be used to obtain a seed for the random number engine
        static std::mt19937 gen(rd()); // Standard mersenne_twister_engine seeded with rd()

        return EVAL_FUZZ ? std::uniform_int_distribution<score_t>(-EVAL_FUZZ, EVAL_FUZZ)(gen) : 0;
    }


    static INLINE int eval_passed_formation(const State& state, int piece_count)
    {
        const auto diff =
            state.longest_pawn_sequence(state.occupied_co(WHITE) & (BB_RANK_6 | BB_RANK_7)) -
            state.longest_pawn_sequence(state.occupied_co(BLACK) & (BB_RANK_2 | BB_RANK_3));

        return diff * interpolate(piece_count, MIDGAME_PASSED_FORMATION, ENDGAME_PASSED_FORMATION);
    }


    static INLINE Bitboard pawn_defenders(const State& state, Color color, Square square)
    {
        return state.occupied_co(color) & state.pawns & BB_PAWN_ATTACKS[!color][square];
    }


    static Bitboard pawn_chain(
        const State&    state,
        Color           color,
        Square          pawn_square,
        Bitboard        (&cache)[64])
    {
        auto& chain = cache[pawn_square];

        if (chain == BB_ALL)
        {
            const auto defenders = pawn_defenders(state, color, pawn_square);

            chain = defenders;

            for_each_square(defenders, [&](Square square) {
                chain |= pawn_chain(state, color, square, cache);
            });
        }
        return chain;
    }

    static INLINE int eval_pawn_chain(
        const State&    state,
        Color           color,
        Square          pawn_square,
        Bitboard        (&cache)[64])
    {
        const auto chain = pawn_chain(state, color, pawn_square, cache);
        return chain == BB_EMPTY ? 0 : pow(2, 1 + popcount(chain));
    }


    /* Evaluate unblocked, passed pawns */
    template<int i> static INLINE
    int eval_passed_pawns(const State& state, int piece_count, Bitboard (&pawn_chain)[2][64])
    {
        struct PassedPawnRank
        {
            Bitboard mask[2]; /* midgame, endgame */
            int bonus[2]; /* ditto */
        };

        static
    #if TUNING_ENABLED || TUNING_PARTIAL
            const
    #else
            constexpr
    #endif
        PassedPawnRank ranks[2] = {
            {   /* 6th rank */
                { BB_RANK_3, BB_RANK_6 },
                { MIDGAME_UNBLOCKED_PASSED_6, ENDGAME_UNBLOCKED_PASSED_6 }
            },
            {   /* 7th rank */
                { BB_RANK_2, BB_RANK_7 },
                { MIDGAME_UNBLOCKED_PASSED_7, ENDGAME_UNBLOCKED_PASSED_7 }
            },
        };

        int score = 0;
        const auto occupied = state.occupied();

        for (auto color : { BLACK, WHITE })
        {
            const auto sign = SIGN[color];
            const auto own_mask = state.occupied_co(color);
            const auto others_mask = state.occupied_co(!color);

            const auto pawns = state.pawns & own_mask & ranks[i].mask[color];

            for_each_square(pawns, [&](Square square) {
                const auto pawn_mask = BB_SQUARES[square];
                const auto advance_mask = color ? shift_up(pawn_mask) : shift_down(pawn_mask);

                if ((advance_mask & occupied) == 0)
                {
                    score += sign * interpolate(piece_count, ranks[i].bonus[0], ranks[i].bonus[1]);
                }
                else if (BB_PAWN_ATTACKS[color][square] & others_mask)
                {
                    score += sign * interpolate(piece_count, ranks[i].bonus[0], ranks[i].bonus[1]);
                }

                score += sign
                    * eval_pawn_chain(state, color, square, pawn_chain[color])
                    * interpolate(piece_count, MIDGAME_PAWN_CHAIN, ENDGAME_PAWN_CHAIN);
            });
        }

        return score;
    }


    /*
     * https://www.chessprogramming.org/images/7/70/LittleChessEvaluationCompendium.pdf
     * Grading of Pieces, page 4
     */
    static INLINE int eval_piece_grading(const State& state, int pcs)
    {
        static constexpr int percents[4][4] = {
            /*  n,  b,   r,  q */
            {   2,  0, -3,  -2, }, /* closed */
            {   2,  1, -2,  -1, }, /* semi-closed */
            {  -2,  3,  2,   4, }, /* semi-open */
            {  -3,  4,  2,   6, }, /* open */
        };

        const int p = popcount(state.pawns);
        const auto& grading = percents[int(p > 4) + int(p > 8) + int(p > 12)];

        int score = 0;

        for (const auto color : { BLACK, WHITE })
        {
            const auto color_mask = state.occupied_co(color);

            score += SIGN[color] * (
                + popcount(state.knights & color_mask) * WEIGHT[KNIGHT] * grading[0]
                + popcount(state.bishops & color_mask) * WEIGHT[BISHOP] * grading[1]
                + popcount(state.rooks & color_mask) * WEIGHT[ROOK] * grading[2]
                + popcount(state.queens & color_mask) * WEIGHT[QUEEN] * grading[3]
            ) / 100;

            score += SIGN[color] * popcount(state.pawns * color_mask) * interpolate(pcs, 0, 3);
        }

        return score;
    }


    static INLINE int eval_pawn_structure(const State& state, int pc)
    {
        int eval = eval_passed_formation(state, pc);

        Bitboard pawn_chain_cache[2][64];
        std::fill_n(&pawn_chain_cache[0][0], 2 * 64, BB_ALL);

        eval += eval_passed_pawns<0>(state, pc, pawn_chain_cache);
        eval += eval_passed_pawns<1>(state, pc, pawn_chain_cache);

        int doubled_pawn_count = 0;
        int isolated_pawn_count = 0;
        int diff = 0; /* for pawn majority */
        int pawn_chain = 0;

        for (auto color : { BLACK, WHITE })
        {
            const auto sign = SIGN[color];

            if (const auto own_pawns = state.pawns & state.occupied_co(color))
            {
                for (const auto& bb_file : BB_FILES)
                {
                    auto n = popcount(own_pawns & bb_file);
                    doubled_pawn_count += sign * (n > 1) * (n - 1);
                }
                diff += sign * popcount(own_pawns);
                isolated_pawn_count += sign * state.count_isolated_pawns(color);
            }
        }

        return eval
            + doubled_pawn_count * interpolate(pc, MIDGAME_DOUBLED_PAWNS, ENDGAME_DOUBLED_PAWNS)
            + isolated_pawn_count * interpolate(pc, MIDGAME_ISOLATED_PAWNS, ENDGAME_ISOLATED_PAWNS)
            + pawn_chain * interpolate(pc, MIDGAME_PAWN_CHAIN, ENDGAME_PAWN_CHAIN)
            + (diff != 0) * SIGN[diff > 0] * interpolate(pc, MIDGAME_PAWN_MAJORITY, ENDGAME_PAWN_MAJORITY);
    }


    /*
     * Discourage exchanging queens if the "other" side's king is less safe than "ours".
     */
    static INLINE int eval_queen_exchange(const Context& ctxt, int king_safety)
    {
        if (king_safety
            && ctxt._parent
            && ctxt.state().capture_value == WEIGHT[QUEEN]
            && ctxt._parent->state().capture_value == WEIGHT[QUEEN]
            && ctxt.state().queens == BB_EMPTY
            && ctxt.turn() == (king_safety > 0))
        {
            return king_safety + SIGN[ctxt.turn()] * QUEEN_EXCHANGE_PENALTY;
        }

        return king_safety;
    }


    static INLINE int eval_threats(const State& state, int piece_count)
    {
        int diff = 0;

        const auto occupied = state.occupied();

        for (auto color : { BLACK, WHITE })
        {
            const auto sign = SIGN[color];

            for_each_square(state.occupied_co(color) & ~(state.pawns | state.kings), [&](Square square) {
                diff -= sign * popcount(state.attackers_mask(!color, square, occupied));
            });
        }
        return diff * interpolate(piece_count, MIDGAME_THREATS, ENDGAME_THREATS);
    }


    static INLINE int eval_tactical(const State& state, score_t mat_eval, int piece_count)
    {
        score_t eval = 0;

        eval += eval_center(state, piece_count);

        eval += eval_redundant_rook(state, piece_count, mat_eval);

        eval += eval_open_files(state, piece_count);

        eval += eval_pawn_structure(state, piece_count);
        eval += eval_piece_grading(state, piece_count);

        eval += state.diff_connected_rooks()
             * interpolate(piece_count, MIDGAME_CONNECTED_ROOKS, ENDGAME_CONNECTED_ROOKS);

        eval += BISHOP_PAIR * state.diff_bishop_pairs();

        return eval;
    }


    static INLINE int eval_tactical(Context& ctxt)
    {
        const auto piece_count = popcount(ctxt.state().occupied());
        const auto mat_eval = ctxt.evaluate_material();

        return eval_tactical(ctxt.state(), mat_eval, piece_count)
            + eval_queen_exchange(ctxt, ctxt.eval_king_safety(piece_count))
            + ctxt.eval_threats(piece_count);
    }


    /*
     * Static evaluation has three components:
     * 1. base = material + pst + mobility
     * 2. tactical (positional)
     * 3. capture estimates.
     */
    score_t Context::_evaluate()
    {
        auto eval = _tt_entry._eval;

        if (eval == SCORE_MIN)
        {
            _tt->_eval_depth = _ply;

            /* 1. Material + piece-squares + mobility, if PV node */
            if (is_pv_node())
                eval = state().eval();
            else
                eval = state().eval<false>();

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

                /* 2. Tactical (skip in midgame at low depth) */
                else if (is_pv_node() || state().is_endgame() || depth() > TACTICAL_LOW_DEPTH)
                {
                    eval -= CHECK_BONUS * is_check();
                    eval += SIGN[turn] * eval_tactical(*this);

                    ASSERT(eval < SCORE_MAX);

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


    int Context::eval_king_safety(int piece_count)
    {
        if (_tt_entry._king_safety == SCORE_MIN)
        {
            _tt_entry._king_safety = search::eval_king_safety(state(), piece_count);
        }

        return _tt_entry._king_safety;
    }


    int Context::eval_threats(int piece_count)
    {
        if (_tt_entry._threats == SCORE_MIN)
            _tt_entry._threats = search::eval_threats(state(), piece_count);

        return _tt_entry._threats;
    }


    /*
     * Fractional extensions: https://www.chessprogramming.org/Extensions
     * "[...] extension can be added that does not yet extend the search,
     * but further down the tree may cause an extension when another
     * fractional extension causes the net extension to exceed one ply."
     */
    void Context::extend()
    {
        if (_extension || depth() > 6)
        {
            /*
             * things that could add interestingness along the search path
             * "what is ultimately to be reduced must first be expanded" Lao Tzu
             */
            _extension += state().pushed_pawns_score;
            _extension += _move.from_square() == _parent->_capture_square;
            _extension += is_recapture() * (is_pv_node() * (ONE_PLY - 1) + 1);
            _extension += is_singleton() * (is_pv_node() + 1) * ONE_PLY / 2;

            _extension += !state().promotion * !state().capture_value * (improvement() / IMPROVEMENT_EXT_DIV);

            /*
             * extend if move has historically high cutoff percentages and counts
             */
            _extension += ONE_PLY
                * (_move == _parent->_tt_entry._hash_move)
                * (abs(_parent->_tt_entry._value) < MATE_HIGH)
                * (_parent->history_count(_move) > HISTORY_COUNT_HIGH)
                * (_parent->history_score(_move) > HISTORY_HIGH);

            const auto double_extension_ok = (_double_ext <= DOUBLE_EXT_MAX);
            const auto extend = std::min(1 + double_extension_ok, _extension / ONE_PLY);

            ASSERT(extend >= 0);

            _max_depth += extend;
            _extension %= ONE_PLY;
            _double_ext += extend > 1;
        }
    }


    /*
     * Used when the search failed to find a move before time ran out.
     */
    const Move* Context::first_valid_move()
    {
        rewind(0);
        return get_next_move(0 /* = no futility pruning */);
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

    #if 0 /* do not clear, carry over _best_move to next iteration */
        _best_move = Move();
    #endif

        _cancel = false;
        _can_prune = -1;

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

        _reset_window = false;
        _retry_next = false;
        _retry_beta = SCORE_MAX;

        rewind(0, true);
    }


    template<std::size_t... I>
    static constexpr std::array<int, sizeof ... (I)> window_bounds(std::index_sequence<I...>)
    {
        return { int(HALF_WINDOW * sqrt(I)) ... };
    }


    template<std::size_t... I>
    static constexpr std::array<int, sizeof ... (I)> window_ext_bounds(std::index_sequence<I...>)
    {
        return { int(HALF_WINDOW * pow(I, 2)) ... };
    }


    static const auto wnd_bounds = window_bounds(std::make_index_sequence<PLY_MAX>{});
    static const auto wnd_ext_bounds = window_ext_bounds(std::make_index_sequence<PLY_MAX>{});


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
                _alpha = std::max(SCORE_MIN, score - wnd_ext_bounds[iteration()]);
                _beta = _tt->_w_beta;
            }
            else if (score >= _tt->_w_beta)
            {
                _alpha = _tt->_w_alpha;
                _beta = std::min(SCORE_MAX, score + wnd_ext_bounds[iteration()]);
            }
            else
            {
                _alpha = std::max(SCORE_MIN, score - wnd_bounds[iteration()]);
                _beta = std::min(SCORE_MAX, score + wnd_bounds[iteration()]);

                if (_alpha > _tt->_w_alpha || _beta < _tt->_w_beta)
                {
                    /*
                     * bump up the clock if the window is shrinking, so that
                     * future lookups ignore entries from wider search windows
                     */
                    TranspositionTable::increment_clock();
                }
            }
        }

        _tt->_w_alpha = _alpha;
        _tt->_w_beta = _beta;

        _score = SCORE_MIN;
    }


    void Context::copy_move_state()
    {
        ASSERT(_move);
        ASSERT(_move._state);

        _statebuf = *_move._state;
        _state = &_statebuf;
        _move._state = _state;
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
     * Late move reduction and pruning.
     * https://www.chessprogramming.org/Late_Move_Reductions
     */
    LMRAction Context::late_move_reduce(int count)
    {
        ASSERT(!is_null_move());
        ASSERT(_parent);

        const int depth = this->depth();

        /* no reduction / pruning in qsearch */
        if (depth < 0)
            return LMRAction::None;

        if (depth < 3 || count < _parent->_full_depth_count || !can_reduce())
            return LMRAction::None;

        auto reduction = LMR._table[std::min(depth, PLY_MAX-1)][std::min(count, 63)];

        if (_move._group != MoveOrder::TACTICAL_MOVES)
        {
            reduction += !has_improved();
            reduction -= 2 * _parent->is_counter_move(_move);

            if (get_tt()->_w_beta <= get_tt()->_w_alpha + 2 * HALF_WINDOW && iteration() >= 13)
                ++reduction;

            reduction -= _parent->history_count(_move) / HISTORY_COUNT_HIGH;
        }

        reduction = std::max(1, reduction);

        if (reduction > depth && can_prune())
            return LMRAction::Prune;

        ASSERT(reduction > 0);
        _max_depth -= reduction;

        /*
         * https://www.chessprogramming.org/Late_Move_Reductions
         * "Classical implementation assumes a re-search at full depth
         * if the reduced depth search returns a score above alpha."
         */
        if (_algorithm == Algorithm::NEGASCOUT)
            _retry_above_alpha = RETRY::Reduced;

    #if EXTRA_STATS
        ++_tt->_reductions;
    #endif /* EXTRA_STATS */

        return LMRAction::Ok;
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
        ASSERT(_fifty < 100);
        ASSERT(is_repeated() <= 0);

        if (_ply == 0)
            return false;

        if (_ply + 1 >= PLY_MAX)
            return true;

        /* the only available move */
        if (is_singleton() && _ply <= 1)
            return true;

        if (depth() > 0
            || is_null_move()
            || is_retry()
            || state().promotion
            || is_check()
            /*
             * last move to search from current node, with score close to mate?
             * extend the search as to not miss a possible mate in the next move
             */
            || (_parent->_score < MATE_LOW && _parent->_score > SCORE_MIN && is_last_move())
           )
            return false;

        /* treat it as leaf for now but retry and extend if it beats alpha */
        if (is_reduced())
            _retry_above_alpha = RETRY::Reduced;

        return true;
    }


    /*
     * https://en.wikipedia.org/wiki/Extended_Position_Description
     */
    std::string Context::epd(const State& state)
    {
        return cython_wrapper::call(_epd, state);
    }


    int64_t Context::check_time_and_update_nps()
    {
        const auto now = std::chrono::steady_clock::now();
        const auto millisec = duration_cast<std::chrono::milliseconds>(
            now - _time_start.load()
        ).count();

        /*
         * Update nodes-per-second for the this thread.
         */
        if (millisec)
            _tt->set_nps((1000 * _tt->nodes()) / millisec);
        else
            _tt->set_nps(_tt->nodes());

        if (_time_limit >= 0 && millisec >= _time_limit)
        {
            cancel();
            return -1;
        }

        return millisec;
    }


    /* static */ void Context::set_time_limit_ms(int time_limit)
    {
        /*
         * Time limits can be updated from a different
         * thread, to support pondering in the background.
         */
        _cancel = false;
        _time_start = std::chrono::steady_clock::now();
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
                _time_limit = millisec / 10;
            }
            else if (val < 0)
            {
                _time_limit = millisec / 15;
            }
            else
            {
                auto estimated_moves_left = (state().is_endgame() || is_check()) ? 15 : 20;

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
                break;

            if (check_time_and_update_nps() < 0)
                break;
        }
#endif /* _POSIX_VERSION */

        return check_time_and_update_nps();
    }


    /*---------------------------------------------------------------------
     * MoveMaker
     *---------------------------------------------------------------------*/
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
                auto& move = ctxt.moves()[i];

                if (move._state == nullptr)
                {
                    ASSERT(move._group == MoveOrder::UNORDERED_MOVES);
                    ASSERT(move._score == 0);
                    break;
                }

                if (move._group >= MoveOrder::QUIET_MOVES)
                {
                    _count = i;
                    break;
                }

                /* retain previously calculated capture score */
                if (move._state->capture_value == 0)
                    move._score = 0;

                move._group = MoveOrder::UNORDERED_MOVES;
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

        auto& moves_list = ctxt.moves();
        moves_list.clear();

        if (ctxt.get_tt()->_initial_moves.empty())
        {
            ctxt.state().generate_pseudo_legal_moves(moves_list);
        }
        else
        {
            moves_list.swap(ctxt.get_tt()->_initial_moves);

            for (auto& move : moves_list)
            {
                move._state = nullptr;
                move._score = 0;
                move._group = MoveOrder::UNORDERED_MOVES;
            }
        }

        _count = int(moves_list.size());
        _current = 0;

        /* Initialize scratch buffers for making the moves. */
        Context::states(ctxt.tid(), ctxt._ply).resize(_count);

        /*
         * In quiescent search, only quiet moves are interesting.
         * If in check, no need to determine "quieteness", since
         * all legal moves are about getting out of check.
         */
        _group_quiet_moves = (ctxt.is_qsearch() && !ctxt.is_check());

        if (ctxt._ply * !ctxt.is_null_move() * !ctxt._excluded)
        {
            if (const auto move = lookup_pv(ctxt))
                ctxt._prev = *move;
        }
    }


    static INLINE const Move* match_killer(const KillerMoves* killers, const Move& move)
    {
        if (killers)
            for (size_t i = 0; i != 2; ++i)
                if ((*killers)[i] == move)
                    return &(*killers)[i];

        return nullptr;
    }


    static INLINE bool is_pawn_push(Context& ctxt, const Move& move)
    {
        return ctxt.state().pawns & BB_PASSED[ctxt.turn()] & BB_SQUARES[move.from_square()];
    }


    static INLINE bool is_attack_on_king(const Context& ctxt, const Move& move)
    {
        const auto king = ctxt.state().king(!ctxt.turn());
        return square_distance(king, move.to_square()) <= 2;
    }


    template<std::size_t... I>
    static constexpr std::array<double, sizeof ... (I)> thresholds(std::index_sequence<I...>)
    {
        auto logistic = [](int i) { return HISTORY_HIGH / (1 + exp(6 - i)); };
        return { logistic(I) ... };
    }


    static const auto hist_thresholds = thresholds(std::make_index_sequence<PLY_MAX>{});


    /*
     * Order moves in multiple phases (passes). The idea is to minimize make_move() calls,
     * which validate the legality of the move. The granularity (number of phases) should
     * not be too high, to keep the overhead of sorting the moves low.
     */
    void MoveMaker::order_moves(Context& ctxt, size_t start_at, score_t futility)
    {
        static constexpr int MAX_PHASE = 4;

        ASSERT(_phase <= MAX_PHASE);
        ASSERT(ctxt._tt);
        ASSERT(ctxt.moves()[start_at]._group == MoveOrder::UNORDERED_MOVES);

        const KillerMoves* killer_moves = nullptr;

        /* "confidence bar" for historical scores */
        const auto hist_high = hist_thresholds[ctxt.iteration()];

        _have_move = false;

        while (!_have_move && start_at < size_t(_count) && _phase < MAX_PHASE)
        {
            if (++_phase == 2)
            {
                killer_moves = ctxt._tt->get_killer_moves(ctxt._ply);
            }

            /********************************************************************/
            /* iterate over pseudo-legal moves                                  */
            /********************************************************************/
            auto& moves_list = ctxt.moves();
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

                switch (_phase)
                {
                case 1:
                    if (move == ctxt._prev)
                    {
                        make_move(ctxt, move, ctxt._ply ? MoveOrder::HASH_MOVES : MoveOrder::PREV_ITER);
                    }
                    else if (move == ctxt._tt_entry._hash_move)
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
                        make_capture(ctxt, move);
                    }
                    else if (auto k_move = match_killer(killer_moves, move))
                    {
                        make_move(ctxt, move, MoveOrder::KILLER_MOVES, k_move->_score);
                    }
                    break;

                case 3:
                    {   /* top historical scores, including counter-move bonus */
                        const auto score = ctxt.history_score(move);
                        if (score > hist_high)
                        {
                            make_move(ctxt, move, MoveOrder::HISTORY_COUNTERS, score);
                            break;
                        }
                    }

                    if ((ctxt.is_counter_move(move)
                        || move.from_square() == ctxt._capture_square
                        || is_pawn_push(ctxt, move)
                        || is_attack_on_king(ctxt, move)
                        )
                        && make_move(ctxt, move, MoveOrder::TACTICAL_MOVES))
                    {
                        move._score = ctxt.history_score(move);
                    }
                    break;

                case 4:
                    if (!make_move(ctxt, move, futility))
                    {
                        break;
                    }
                    move._score = ctxt.history_score(move);

                    if (move._score >= HISTORY_LOW
                        /* computing forks and pins is expensive */
                        && (move._state->has_fork(!move._state->turn)
                            || is_direct_check(move)
                            || move._state->is_pinned(move._state->turn)))
                    {
                        move._group = MoveOrder::TACTICAL_MOVES;
                    }
                    else
                    {
                        move._group = MoveOrder::LATE_MOVES;
                    }
                    break;

                default:
                    ASSERT(false);
                    break;
                }
            }
        }

        ASSERT(_count >= 0);

        if (size_t(_count) <= start_at)
        {
            ASSERT(ctxt.moves()[start_at]._group >= MoveOrder::QUIET_MOVES);
            _need_sort = false;
        }
        else if (_need_sort)
        {
            sort_moves(ctxt, start_at);
        }

    #if !defined(NO_ASSERT)
        for (size_t i = _count; i < ctxt.moves().size(); ++i)
        {
            ASSERT(ctxt.moves()[i]._group >= MoveOrder::QUIET_MOVES);
        }
    #endif /* NO_ASSERT */
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
