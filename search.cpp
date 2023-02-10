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
/*
 * TranspositionTable and related data structs.
 * Search algorithms: Negamax with TranspositionTable, MTD(f)
 */
#include <memory>
#include <sstream>
#include <utility>
#include "context.h"
#include "search.h"
#include "thread_pool.hpp"
#include "utility.h"

#if __linux__
#include <sys/sysinfo.h>
#elif __APPLE__
#include <sys/sysctl.h>
#elif _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef ERROR
#undef max
#undef min
#endif /* _WIN32 */


using namespace chess;
using namespace search;

constexpr size_t ONE_MEGABYTE = 1024 * 1024;


template<bool Debug = false>
static void log_pv(const TranspositionTable& tt, const Context* ctxt, const char* info)
{
    if constexpr(Debug)
    {
        std::ostringstream out;

        out << info << ": ";
        for (const auto& move : tt._pv)
            out << move << " ";
        if (ctxt)
            out << " pos=" << ctxt->epd();
        Context::log_message(LogLevel::INFO, out.str());
    }
}


static size_t mem_avail()
{
#if __linux__
    struct sysinfo info = {};
    if (sysinfo(&info) == 0)
        return info.freeram;

#elif __APPLE__
    unsigned long mem = 0;
    size_t len = sizeof(mem);
    static int mib[2] = { CTL_HW, HW_USERMEM };

    if (sysctl(mib, std::extent<decltype(mib)>::value, &mem, &len, nullptr, 0) == 0)
        return mem;
#elif _WIN32
    MEMORYSTATUSEX statex = {};
    statex.dwLength = sizeof (statex);

    if (::GlobalMemoryStatusEx(&statex))
        return statex.ullAvailVirtual;
#endif
    /* failover to psutil via Cython */
    return static_cast<size_t>(cython_wrapper::call(search::Context::_vmem_avail));
}


TranspositionTable::HashTable TranspositionTable::_table(DEFAULT_HASH_TABLE_SIZE, mem_avail());
MovesCache TranspositionTable::_moves_cache(1031);


/* static */ size_t TranspositionTable::max_hash_size()
{
    const size_t cur_size = _table.byte_capacity();
    const size_t max_mem = mem_avail() + cur_size;

    return max_mem / ONE_MEGABYTE;
}


/* static */ size_t TranspositionTable::get_hash_size()
{
    return _table.byte_capacity() / ONE_MEGABYTE;
}


/* static */ void TranspositionTable::set_hash_size(size_t MB)
{
    if (_table.resize(MB, mem_avail()))
    {
        std::ostringstream out;
        out << "set_hash_size: requested=" << MB << " new="
            << get_hash_size() << " free=" << mem_avail() / ONE_MEGABYTE;

        Context::log_message(LogLevel::DEBUG, out.str());
    }
    else
    {
        Context::log_message(LogLevel::ERROR, "set_hash_size: failed.");
    }
}


void TranspositionTable::clear()
{
    _iteration = 0;
    _eval_depth = 0;

    _w_alpha = SCORE_MIN;
    _w_beta = SCORE_MAX;
    _reset_window = false;

    _check_nodes = 0;
    _eval_count = 0;
    _endgame_nodes = 0;
    _futility_prune_count = 0;
    _history_counters = 0;
    _history_counters_hit = 0;
    _hits = 0;
    _late_move_prune_count = 0;
    _nodes = 0;
    _nps = 0; /* nodes per second */
    _null_move_cutoffs = 0;
    _null_move_failed = 0;
    _null_move_not_ok = 0;
    _qsnodes = 0;
    _reductions = 0;
    _retry_reductions = 0;

    for (auto color : { BLACK, WHITE })
    {
        _countermoves[color].clear();
        _hcounters[color].clear();
    }
}


/* static */ void TranspositionTable::clear_shared_hashtable()
{
    assert_param_ref();

    _table.clear();
    _moves_cache.clear();
}


/* static */ void TranspositionTable::increment_clock()
{
    _table.increment_clock();
}


/* static */ double TranspositionTable::usage()
{
    return (100.0 * _table.size()) / _table.capacity();
}


void TranspositionTable::update_stats(const Context& ctxt)
{
    if (ctxt.is_qsearch())
        ++_qsnodes;

    if (ctxt.is_check())
        ++_check_nodes;

    if (ctxt.state().is_endgame())
        ++_endgame_nodes;
}


void TranspositionTable::store(Context& ctxt, TT_Entry& entry, score_t alpha, int depth)
{
    ASSERT(ctxt._score > SCORE_MIN);
    ASSERT(ctxt._score < SCORE_MAX);
    ASSERT(alpha < ctxt._beta);
    ASSERT(ctxt._alpha >= alpha);

    if (entry.is_valid())
    {
        if  (!entry.matches(ctxt.state()))
        {
            entry._type = TT_Type::NONE;
            entry._hash_move = BaseMove();
        }
    }

    /* Store or reset hash move */
    auto move = ctxt._best_move;

    if (move || entry.is_lower())
        entry._hash_move = move;

    entry._value = ctxt._score;

    if (entry._value >= ctxt._beta)
        entry._type = TT_Type::LOWER;
    else if (entry._value <= alpha)
        entry._type = TT_Type::UPPER;
    else
        entry._type = TT_Type::EXACT;

    entry._hash = ctxt.state().hash();
    entry._depth = depth;
    entry._age = _table.clock();
}


void TranspositionTable::store_killer_move(const Context& ctxt)
{
    ASSERT(ctxt._score); /* do not store draws */

#if KILLER_MOVE_HEURISTIC
    ASSERT(ctxt._ply < PLY_MAX);

    const auto& move = ctxt._cutoff_move;
    if (!move)
        return;

    ASSERT(!ctxt.state().is_capture(move));

    auto& killers = _killer_moves[ctxt._ply];

    if (killers[0] != move)
    {
        killers[1] = killers[0];
        killers[0] = move;
        killers[0]._score = ctxt._score;
        killers[0]._state = nullptr; /* prevent accidental use */
    }
#endif /* KILLER_MOVE_HEURISTIC */
}


inline void log_invalid_pv(
    const std::string& func,
    const PV& pv,
    const Context& start,
    const BaseMove& move)
{
    std::ostringstream out;
    out << "invalid: " << move << " pv=";
    for (const auto& m : pv)
        out << m << " ";
    out << move << " start=" << start.epd() << " func=" << func;
    Context::log_message(LogLevel::WARN, out.str());
}


template<bool Debug>
static bool is_valid_pv_move(
    const std::string& func,
    const PV& pv,
    const Context& start,
    const State& pos,
    const BaseMove& move)
{
    if constexpr(Debug)
    {
        if (   (pos.piece_type_at(move.from_square()) == PieceType::NONE)
            || (pos.occupied_co(pos.turn) & chess::BB_SQUARES[move.to_square()])
            || (pos.kings & chess::BB_SQUARES[move.to_square()])
           )
        {
            log_invalid_pv(func, pv, start, move);
            return false;
        }
    }
    return true;
}


template<bool Debug>
void TranspositionTable::get_pv_from_table(Context& root, const Context& ctxt, PV& pv)
{
    auto state = ctxt.state().clone();

    ASSERT(Context::epd(state) == ctxt.epd());
    ASSERT(state.hash() == ctxt.state().hash());

    /* keep track of state hashes, to detect cycles */
    std::unordered_set<size_t> visited;

    auto move = ctxt._best_move;

    while (move)
    {
        if (!is_valid_pv_move<Debug>(__func__, pv, root, state, move))
            break;

        state.apply_move(move);

        /* Guard against infinite loops. */
        if (!visited.insert(state.hash()).second)
            break;

        if constexpr(Debug)
        {
            if (state.is_check(!state.turn))
            {
                log_invalid_pv(__func__, pv, root, move);
                break;
            }
        }

        /* Add the move to the principal variation. */
        pv.emplace_back(move);

        auto p = _table.lookup_read(state);
        if (!p)
            break;

        ASSERT(p->matches(state));

        move = p->_hash_move;
    }

    if (abs(root._score) < MATE_HIGH && state.is_checkmate())
    {
        /* The parity of the PV length tells which side is winning. */
        /* Subtract one for the move that lead to the root position */
        root._mate_detected = int(pv.size()) - 1;
    }
}


template<bool Debug> void TranspositionTable::store_pv(Context& root)
{
    ASSERT(root._best_move);

    _pvBuilder.clear();

    for (auto ctxt = &root; true; )
    {
        _pvBuilder.emplace_back(ctxt->_move);
        auto next = ctxt->next_ply();

        if (next->is_null_move())
            break;

        if ((next->_move == ctxt->_best_move)
            && is_valid_pv_move<Debug>(__func__, _pvBuilder, root, ctxt->state(), next->_move))
        {
            ASSERT(next->_parent == ctxt);
            ctxt = next;
            continue;
        }

        get_pv_from_table<Debug>(root, *ctxt, _pvBuilder);
        break;
    }

    if (_pvBuilder.size() > _pv.size() || !std::equal(_pvBuilder.begin(), _pvBuilder.end(), _pv.begin()))
    {
        _pv.swap(_pvBuilder);
        log_pv<Debug>(*this, &root, "store_pv");
    }
}


/*
 * https://www.stmintz.com/ccc/index.php?id=76542
 */
bool verify_null_move(Context& ctxt, Context& null_move_ctxt)
{
    /* consistency checks */
    ASSERT(null_move_ctxt.is_null_move());
    ASSERT(ctxt.next_move_index() == 0);
    ASSERT(!ctxt._best_move);
    ASSERT(ctxt.turn() != null_move_ctxt.turn());
    ASSERT(&ctxt == null_move_ctxt._parent);

    null_move_ctxt.rewind();

    null_move_ctxt._null_move_allowed[null_move_ctxt.turn()] = false;
    null_move_ctxt._score = SCORE_MIN;
    null_move_ctxt._alpha = ctxt._beta - 1;
    null_move_ctxt._beta  = ctxt._beta;

    const auto score = negamax(null_move_ctxt, *null_move_ctxt.get_tt());

    if (score >= ctxt._beta)
        return true; /* verification successful */

    if (ctxt.is_cancelled())
        return false;

    /*
     * null move refuted? update capture_square and mate_detected
     */
    if (const auto& counter_move = null_move_ctxt._best_move)
    {
        ASSERT(score > SCORE_MIN);

        if (null_move_ctxt.state().is_capture(counter_move))
            ctxt._capture_square = counter_move.to_square();

        if (-score > MATE_HIGH)
        {
            ctxt._mate_detected = CHECKMATE + score + 1;
            ASSERT(ctxt._mate_detected > 0);
        }
    }

    if constexpr(EXTRA_STATS)
        ++ctxt.get_tt()->_null_move_failed;

    return false;
}


/*
 * https://www.chessprogramming.org/Multi-Cut
 */
static bool multicut(Context& ctxt, TranspositionTable& table)
{
    if (ctxt.is_root()
        || !ctxt._multicut_allowed
        || ctxt.depth() <= 5
        || ctxt.is_pv_node()
        || ctxt._excluded
        || ctxt.is_mate_bound()
        || ctxt.is_evasion()
        || ctxt.is_check()
       )
        return false;

    const auto& state = ctxt.state();

    if (state.passed_pawns(!state.turn, BB_RANK_2 | BB_RANK_7) != BB_EMPTY)
        return false;

    /* side to move has only king and pawns? */
    if (state.just_king_and_pawns())
        return false;

    int move_count = 0, cutoffs = 0;
    const auto reduction = (ctxt.depth() - 1) / 2;

    BaseMove best_move;
    score_t best_score = SCORE_MIN;

    /*
     * A take on the idea from https://skemman.is/bitstream/1946/9180/1/research-report.pdf
     * The paper proposes trying the heuristic only at nodes that have produced cutoffs at
     * lower depths, to increase its chances to succeed. Here, the idea is to try the heuristic
     * regardless, but lower the count of cutoffs required to "succeed" if the position has
     * produced cutoffs before.
     */
    const auto min_cutoffs = MULTICUT_C - (ctxt.depth() > 5
        && ctxt._tt_entry.is_lower()
        && ctxt._tt_entry._value + MULTICUT_MARGIN >= ctxt._beta);

    while (auto next_ctxt = ctxt.next(false, 0, move_count))
    {
        next_ctxt->_multicut_allowed = false;
        next_ctxt->_max_depth -= reduction;

        const auto score = -negamax(*next_ctxt, table);

        if (ctxt.is_cancelled())
            return false;

        if (score >= ctxt._beta)
        {
            if (!best_move || score > best_score)
            {
                best_score = score;
                best_move = next_ctxt->_move;
            }

            if (++cutoffs >= min_cutoffs)
            {
                ctxt._score = score >= MATE_HIGH ? ctxt._beta : score;
                ctxt._best_move = best_move;

                /* Store it in the TT as a cutoff move. */
                ctxt._alpha = ctxt._score;

                return true;
            }
        }

        if (++move_count >= MULTICUT_M)
            break;
    }

    ASSERT(ctxt._score == SCORE_MIN);
    ctxt.rewind();

    ASSERT(ctxt.next_move_index() == 0);

    return false;
}


static INLINE void update_pruned(Context& ctxt, const Context& next, size_t& count)
{
    ++ctxt._pruned_count;

    if constexpr(EXTRA_STATS)
        ++count;
}


/*
 * Syzygy endgame tablebase probing (https://www.chessprogramming.org/Syzygy_Bases).
 * This implementation simply calls back into the python-chess library.
 */
static INLINE bool probe_endtables(Context& ctxt)
{
    int v;

    if (  !ctxt.is_root()
        && ctxt._fifty == 0
        && ctxt.state().is_endgame()
        && Context::tb_cardinality() > 0
        && popcount(ctxt.state().occupied()) <= Context::tb_cardinality()
        && cython_wrapper::call(Context::_tb_probe_wdl, ctxt.state(), &v))
    {
        if (v < -1)
            ctxt._score = MATE_LOW + ctxt._ply;
        else if (v > 1)
            ctxt._score = MATE_HIGH - ctxt._ply;
        else
            ctxt._score = v;
        return true;
    }
    return false;
}


score_t search::negamax(Context& ctxt, TranspositionTable& table)
{
    ASSERT(ctxt._beta > SCORE_MIN);
    ASSERT(ctxt._score <= ctxt._alpha);
    ASSERT(ctxt._alpha < ctxt._beta);
    ASSERT(ctxt.is_root() || !ctxt._move || ctxt._move._group < MoveOrder::UNORDERED_MOVES);

    ASSERT(ctxt.get_tt() == &table);

    if (ctxt.is_root())
    {
        /* Do not probe end tables if the number of pieces at root
         * position has dropped below the end tables cardinality
         * (the root of the search may already be in the tables).
         */
        table._probe_endtables =
            table._iteration > 3
            && Context::tb_cardinality() > 0
            && popcount(ctxt.state().occupied()) > Context::tb_cardinality();
    }
    else
    {
        if (ctxt._fifty >= 100)
            return 0; /* draw by fifty moves rule */

        zobrist_update(ctxt._parent->state(), ctxt._move, *ctxt._state);

        if (ctxt.is_repeated() > 0)
            return 0;

        /*
         * Mating distance pruning: skip the search if a shorter mate was found.
         * https://www.chessprogramming.org/Mate_Distance_Pruning
         */
        ctxt._alpha = std::max(checkmated(ctxt._ply), ctxt._alpha);
        ctxt._beta = std::min(checkmating(ctxt._ply + 1), ctxt._beta);

        if (ctxt._alpha >= ctxt._beta)
        {
            return ctxt._score = ctxt._alpha;
        }
    }

    ctxt.eval_incremental();

    /*
     * https://www.chessprogramming.org/Node_Types#PV
     */
    ctxt._is_pv = ctxt._algorithm > NEGAMAX
        && !ctxt.is_null_move()
        && (ctxt.is_leftmost() || ctxt._alpha + 1 < ctxt._beta);

    /* reduce by one ply at expected cut nodes */
    if (!ctxt.is_pv_node()
        && !ctxt.is_null_move()
        && ctxt.depth() > 7
        && ctxt.can_reduce())
    {
        --ctxt._max_depth;
    }

    if (ctxt._alpha + 1 < ctxt._beta)
    {
        ASSERT(ctxt._algorithm != NEGASCOUT || ctxt.is_leftmost() || ctxt.is_retry());
    }

    if constexpr(!COUNT_VALID_MOVES_AS_NODES)
        ++table._nodes;

    const auto alpha = ctxt._alpha;

    if (ctxt.is_leaf())
    {
        ctxt._score = ctxt.evaluate();

        ASSERT(ctxt._score > SCORE_MIN);
        ASSERT(ctxt._score < SCORE_MAX);
    }
    /* transposition table lookup */
    else if (const auto* p = table.lookup(ctxt))
    {
        ASSERT(ctxt._score == *p);
        ASSERT(!ctxt._excluded);

        return *p;
    }
    else if (table._probe_endtables && probe_endtables(ctxt))
    {
        table.store<TT_Type::EXACT>(ctxt, alpha, ctxt.depth() + 2 * ctxt.tb_cardinality());
        return ctxt._score;
    }
    else if (multicut(ctxt, table))
    {
        ASSERT(ctxt._score < SCORE_MAX);
#if !CACHE_HEURISTIC_CUTOFFS
        return ctxt._score;
#endif /* !CACHE_HEURISTIC_CUTOFFS */
    }
    else
    {
        ASSERT(ctxt._alpha < ctxt._beta);

    #if WITH_NNUE
        const auto eval = ctxt._eval;
    #else
        const auto eval = ctxt._tt_entry._value;
    #endif

    #if REVERSE_FUTILITY_PRUNING
        /*
         * Reverse futility pruning: static eval stored in TT beats beta by a margin and
         * not in check, and no move w/ scores above MATE_HIGH in the hash table? Prune.
         */
        if (   !ctxt.is_root()
            && !ctxt._excluded /* no reverse pruning during singular extension */
            && !ctxt.is_pv_node()
            && ctxt.depth() > 0
            && ctxt.depth() < 7
            && eval < MATE_HIGH
            && eval > ctxt._beta
                + std::max<score_t>(REVERSE_FUTILITY_MARGIN * ctxt.depth(), ctxt.improvement())
            && !ctxt.is_check())
        {
            ASSERT(eval > SCORE_MIN);
            ASSERT(eval < SCORE_MAX);

            return eval;
        }
    #endif /* REVERSE_FUTILITY_PRUNING */

    #if RAZORING
        if (ctxt.depth() > 0
            && eval < alpha - RAZOR_INTERCEPT - RAZOR_DEPTH_COEFF * pow2(ctxt.depth())
            && eval + eval_captures(ctxt) < alpha)
        {
            return alpha;
        }
    #endif /* RAZORING */

        /* Reduce depth by 2 if PV node not found in the TT (idea from SF). */
        if (ctxt._ply
            && ctxt.is_pv_node()
            && ctxt.depth() >= 6
            && !ctxt._tt_entry.is_valid()
            && ctxt.can_reduce()
           )
            ctxt._max_depth -= 2;

        bool null_move = ctxt.is_null_move_ok();

        if constexpr(EXTRA_STATS)
            table._null_move_not_ok += !null_move;

        const auto root_depth = table._iteration;
        int move_count = 0, futility = -1;

        /* iterate over moves */
        while (auto next_ctxt = ctxt.next(null_move, futility, move_count))
        {
            if (next_ctxt->is_null_move())
            {
                null_move = false;
            }
            else
            {
                if constexpr(EXTRA_STATS)
                    table._history_counters += next_ctxt->_move._group == MoveOrder::HISTORY_COUNTERS;

                /* Futility pruning, 2nd pass. */
                if (futility > 0)
                {
                    ASSERT(move_count > 0);
                    const auto val = futility - next_ctxt->evaluate_material();

                    if ((val < ctxt._alpha || val < ctxt._score) && next_ctxt->can_prune<true>())
                    {
                        update_pruned(ctxt, *next_ctxt, table._futility_prune_count);
                        continue;
                    }
                }

                /*
                 * Do not extend at root, or if already deeper than twice the depth at root
                 */

                if (!ctxt.is_root() && ctxt._ply < root_depth * 2)
                {
                #if SINGULAR_EXTENSION
                   /*
                    * https://www.chessprogramming.org/Singular_Extensions
                    *
                    * Implementation adapted from idea in SF:
                    * Check if the move matches a lower-bound TT entry (beta cutoff);
                    * if it does, search with reduced depth; if the result of the search
                    * does not beat beta, it means the move is singular (the only cutoff
                    * in the current position).
                    */
                    if (ctxt.depth() >= (ctxt.is_pv_node() ? 7 : 5)
                        && ctxt._tt_entry.is_lower()
                        && next_ctxt->_move._group == MoveOrder::HASH_MOVES
                    #if WITH_NNUE
                        && abs(ctxt._eval) < SINGULAR_EVAL_MARGIN
                    #endif
                        && abs(ctxt._tt_entry._value) < MATE_HIGH
                        && !ctxt._excluded
                        && ctxt._tt_entry._depth >= ctxt.depth() - 3)
                    {
                        const auto s_beta = std::max(int(ctxt._tt_entry._value) - ctxt.singular_margin(), MATE_LOW);
                        /*
                         * Hack: use ply + 2 for the singular search to avoid clobbering
                         * _move_maker's _moves / _states stacks for the current context.
                         */
                        ContextBuffer buf;
                        auto s_ctxt = ctxt.clone(buf, ctxt._ply + 2);

                        s_ctxt->set_tt(ctxt.get_tt());
                        s_ctxt->_excluded = next_ctxt->_move;
                        s_ctxt->_max_depth = s_ctxt->_ply + (ctxt.depth() - 1) / 2;
                        s_ctxt->_alpha = s_beta - 1;
                        s_ctxt->_beta = s_beta;
                        s_ctxt->_score = SCORE_MIN;

                        const auto eval = negamax(*s_ctxt, table);

                        if (eval < s_beta)
                        {
                            next_ctxt->_extension += ONE_PLY;
                            if (!ctxt.is_pv_node() && eval + DOUBLE_EXT_MARGIN < s_beta)
                            {
                                next_ctxt->_extension += ONE_PLY;
                            }
                        }
                        /*
                         * Got another fail-high from the (reduced) search that skipped the known
                         * cutoff move, so there must be multiple cutoffs, do 2nd multicut pruning.
                         */
                        else if (s_beta >= ctxt._beta)
                        {
                        #if CACHE_HEURISTIC_CUTOFFS
                            /* Store it in the TT as a cutoff move. */
                            ctxt._alpha = ctxt._score = s_beta;

                            ++move_count;
                            /*
                             * Same as with null move pruning below, make sure that
                             * the move is updated in the TT when storing the result.
                             */
                            ctxt._cutoff_move = ctxt._best_move = next_ctxt->_move;
                            break;
                        #else
                            return s_beta;
                        #endif /* CACHE_HEURISTIC_CUTOFFS */
                        }
                        else if (ctxt._tt_entry._value >= ctxt._beta && next_ctxt->can_reduce())
                        {
                            next_ctxt->_max_depth -= 2;
                        }
                    }
                #endif /* SINGULAR_EXTENSION */

                    next_ctxt->extend(); /* apply fractional extensions */
                }

                /* Late-move reduction and pruning */
                if (move_count && next_ctxt->late_move_reduce(move_count) == LMRAction::Prune)
                {
                    update_pruned(ctxt, *next_ctxt, table._late_move_prune_count);
                    continue;
                }

                if (!next_ctxt->is_retry())
                {
                    ++move_count;
                    if (futility < 0)
                        futility = ctxt.futility_margin();
                }
            }

            /*
             * Recursively search next move.
             */
            const auto move_score = -negamax(*next_ctxt, table);

            if (ctxt.is_cancelled())
            {
                if (move_score < SCORE_MAX)
                    ctxt._score = move_score;
                return ctxt._score;
            }

            if (ctxt.is_beta_cutoff(next_ctxt, move_score))
            {
                ASSERT(ctxt._score == move_score);
                ASSERT(ctxt._cutoff_move || next_ctxt->is_null_move());

                if (next_ctxt->is_null_move())
                {
                    if (ctxt.should_verify_null_move() && !verify_null_move(ctxt, *next_ctxt))
                    {
                        ctxt._alpha = alpha;
                        ctxt._score = SCORE_MIN;
                        continue;
                    }

                    if constexpr(EXTRA_STATS)
                        ++table._null_move_cutoffs;

                    /* verification not expected to modify ctxt._score */
                    ASSERT(ctxt._score == move_score);

                    /* do not trust checkmate results from heuristic */
                    if (ctxt._score > MATE_HIGH)
                        ctxt._score = ctxt._beta;

                #if CACHE_HEURISTIC_CUTOFFS
                    /*
                     * Put the hash move (if available) back in the TT when
                     * storing this result; could skip storing and just return
                     * ctxt._score from here, but that hurts MTD(f) performance.
                     * This should only be needed in the multiple cores case,
                     * with another thread possibly writing to the same entry
                     * in between the current thread's probe time and store time.
                     */
                    if (ctxt._tt_entry.is_lower() && ctxt._tt_entry._hash_move)
                    {
                        ctxt._best_move = ctxt._cutoff_move = ctxt._tt_entry._hash_move;
                    }
                #else
                    return ctxt._score;
                #endif /* CACHE_HEURISTIC_CUTOFFS */
                }
                else if (next_ctxt->is_capture() + next_ctxt->is_promotion() == 0)
                {
                    /*
                     * Store data for move reordering heuristics.
                     */

                    /* sanity checks */
                    ASSERT (next_ctxt->_move);
                    ASSERT (next_ctxt->_move == ctxt._cutoff_move);
                    ASSERT (!ctxt.state().is_capture(ctxt._cutoff_move));

                    /* zero-score moves may mean draw (path-dependent) */
                    if (move_score && ctxt.depth() > 0)
                    {
                        if (ctxt._ply < PLY_HISTORY_MAX && abs(move_score) < MATE_HIGH)
                        {
                            auto& h = table._plyHistory[ctxt._ply][ctxt.turn()][next_ctxt->_move];
                            h.first += next_ctxt->improvement() / ctxt.depth();
                            ++h.second;
                        }

                        if (ctxt.depth() >= COUNTER_MOVE_MIN_DEPTH)
                            table.store_countermove(ctxt);

                        table.store_killer_move(ctxt);

                        if (next_ctxt->depth() >= HISTORY_MIN_DEPTH)
                            table.history_update_cutoffs(next_ctxt->_move);
                    }
                }
                if constexpr(EXTRA_STATS)
                    table._history_counters_hit += (next_ctxt->_move._group == MoveOrder::HISTORY_COUNTERS);

                break; /* found a cutoff */
            }
            else if (next_ctxt->depth() >= HISTORY_MIN_DEPTH && !next_ctxt->is_capture())
            {
                const auto depth = std::max(1, next_ctxt->depth());
                const auto failed_low = next_ctxt->is_pv_node()
                    || next_ctxt->is_extended()
                    || (move_score + HISTORY_FAIL_LOW_MARGIN / depth <= alpha);
                table.history_update_non_cutoffs(next_ctxt->_move, failed_low);
            }

            /*
             * If the 1st move fails low at root it may be unlikely for the
             * subsequent moves to improve things much (assuming reasonable
             * move ordering); readjust the aspiration window and retry.
             *
             * https://www.chessprogramming.org/PVS_and_Aspiration
             */
            if (ASPIRATION_WINDOW
                && ctxt.is_root()
                && move_count == 1
                && move_score < MATE_HIGH
                && move_score < table._w_alpha
                && ctxt.tid() == 0 /* main thread */
                && ctxt.evaluate<false>() < table._w_alpha)
            {
                ASSERT(!next_ctxt->is_null_move());
                table._reset_window = true;

                return move_score;
            }
        }
        ASSERT(ctxt._score <= ctxt._alpha || ctxt._best_move);
    #if 0
        /*
         * since v0.98 Context::next() checks that at least one move
         * has been searched before returning nullptr on cancellation
         */
        if (!ctxt.is_cancelled())
    #endif
        {
            if (!move_count && !ctxt.has_moves())
            {
                /* checkmate or stalemate? */
                ctxt._score = ctxt.evaluate_end();

                ASSERT(ctxt._score > SCORE_MIN);
                ASSERT(ctxt._score < SCORE_MAX);
            }
    #if !NO_ASSERT
            else if (!ctxt._excluded && !ctxt.is_cancelled())
            {
                /* algorithm invariants */
                ASSERT(ctxt._score > SCORE_MIN);
                ASSERT(ctxt._score < SCORE_MAX);
                ASSERT(ctxt._alpha >= ctxt._score);
            }
    #endif /* NO_ASSERT */
        }
    }

    /*
     * Do not store 0 scores in the TT - they could be draws by repetition,
     * which are path-dependent; and the general wisdom is to not store root
     * nodes either (https://www.stmintz.com/ccc/index.php?id=93686)
     */
    if (ctxt._score
        && ctxt._ply
        && !ctxt._excluded
        && !ctxt.is_qsearch()
        && !ctxt.is_cancelled()
       )
        table.store(ctxt, alpha, ctxt.depth());

    return ctxt._score;
}


/*
 * https://en.wikipedia.org/wiki/MTD(f)
 */
score_t search::mtdf(Context& ctxt, score_t first, TranspositionTable& table)
{
    ASSERT_ALWAYS(ctxt._algorithm == Algorithm::MTDF);
    ASSERT_ALWAYS(ctxt.is_root());

    auto lower = ctxt._alpha;
    auto upper = ctxt._beta;

    auto g = first;

    ASSERT(ctxt.get_tt() == &table);

    while (lower < upper)
    {
#if MTDF_CSTAR_BISECT
        /*
            https://people.csail.mit.edu/plaat/mtdf.html

            "In MTD terms the idea of C* is to bisect the interval formed by the upper
            and lower bounds, reducing the number of AlphaBetaWithMemory calls. On the
            down side, bisection yields a value for the search window, beta, that turns
            out to be not as efficient as MTD(f)'s choice."
        */

        auto b = (lower + upper + 1) / 2;
#else

        auto b = std::max(g, lower + 1);

#endif /* MTDF_CSTAR_BISECT */

        ctxt._score = std::max<score_t>(SCORE_MIN, b - 1);
        ctxt._alpha = b - 1;
        ctxt._beta = b;

        g = negamax(ctxt, table);

        ASSERT(g < SCORE_MAX); /* sanity check */

        if (ctxt.is_cancelled())
            break;

        if (g < b)
            upper = g;
        else
            lower = g;

        ctxt.rewind(0, MTDF_REORDER_MOVES);
    }

    return (ctxt._score = g);
}


static score_t search_iteration(Context& ctxt, TranspositionTable& table, score_t prev_score)
{
    score_t score = 0;

    switch (ctxt._algorithm)
    {
    case Algorithm::NEGAMAX:
    case Algorithm::NEGASCOUT:
        score = negamax(ctxt, table);
        break;

    case Algorithm::MTDF:
        score = mtdf(ctxt, prev_score, table);
        break;

    default:
        ASSERT_ALWAYS(false);
    }

    if (ctxt._best_move)
    {
        ctxt._prev = ctxt._best_move; /* save for next iteration */
        table.store_pv(ctxt);
    }

    return score;
}


#if SMP
/****************************************************************************
 * Lazy SMP. https://www.chessprogramming.org/Lazy_SMP
 ****************************************************************************/
using ThreadPool = thread_pool<std::vector<std::function<void()>>, false>;

static std::unique_ptr<ThreadPool> threads;

static size_t start_pool()
{
    Context::ensure_stacks();

    if (!threads || threads->get_thread_count() + 1 != size_t(SMP_CORES))
    {
        if (SMP_CORES <= 1)
            return 0;

        threads = std::make_unique<ThreadPool>(SMP_CORES - 1);
    }

    return threads->get_thread_count();
}

struct TaskData
{
    Context* _ctxt = nullptr;
    TranspositionTable _tt;
    ContextBuffer _raw_mem;
};


/*
 * Ctor and dtor set up / clean up a multi-threaded search iteration.
 */
class SMPTasks
{
    SMPTasks(const SMPTasks&) = delete;
    SMPTasks& operator=(const SMPTasks&) = delete;

    Context& _root;

public:
    /* static, preserve TTs between iterations */
    static std::vector<TaskData> _tables;


    SMPTasks(Context& ctxt, TranspositionTable& table, score_t score)
        : _root(ctxt)
    {
        const auto thread_count = start_pool();
        ASSERT(thread_count + 1 == size_t(SMP_CORES));

        if (table._iteration == 1)
        {
            _tables.resize(thread_count);
            for (size_t i = 0; i != thread_count; ++i)
                _tables[i]._tt = table;

            /* run 1st iteration on one thread only */
            return;
        }

        for (size_t i = 0; i < thread_count; ++i)
        {
            _tables[i]._tt._iteration = table._iteration;

            /* copy principal variation from main thread */
            if (_tables[i]._tt._pv.empty())
                _tables[i]._tt._pv = table._pv;

            /* get the previous moves before clobbering the Context */
            BaseMove hash_move, prev_best;
            if (_tables[i]._ctxt)
            {
                hash_move = _tables[i]._ctxt->_tt_entry._hash_move;
                prev_best = _tables[i]._ctxt->_prev;
            }

            _tables[i]._ctxt = _root.clone(_tables[i]._raw_mem);
            _tables[i]._tt._w_alpha = _tables[i]._ctxt->_alpha;
            _tables[i]._tt._w_beta = _tables[i]._ctxt->_beta;
            _tables[i]._ctxt->_max_depth += (i % 2) == 0;
            _tables[i]._ctxt->set_tt(&_tables[i]._tt);
            _tables[i]._ctxt->_tt_entry._hash_move = hash_move;

            if (prev_best)
                _tables[i]._ctxt->_prev = prev_best;

            /* pass context and TT pointers to thread task */
            auto t_ctxt = _tables[i]._ctxt;
            auto tt = &_tables[i]._tt;

            threads->push_task([t_ctxt, tt, score]() mutable {

                tt->_tid = ThreadPool::thread_id();
                try
                {
                    search_iteration(*t_ctxt, *tt, score);
                }
                catch(const std::exception& e)
                {
                    Context::log_message(LogLevel::ERROR, e.what());
                }
            });
        }
    }

    ~SMPTasks()
    {
        if (threads)
        {
            Context::cancel();
            threads->wait_for_tasks();
        }
    }

    void do_report()
    {
        if (Context::_report)
        {
            static std::vector<Context*> ctxts;
            ctxts.clear();

            for (auto& table : _tables)
            {
                if (table._ctxt && table._ctxt->get_tt())
                {
                    ctxts.emplace_back(table._ctxt);
                    ASSERT(ctxts.back()->get_tt() == &table._tt);
                }
            }
            cython_wrapper::call(Context::_report, Context::_engine, ctxts);
        }
    }
};

#else

/* dummy structs */
struct TaskData
{
    TranspositionTable _tt;
};
struct SMPTasks
{
    static std::vector<TaskData> _tables;
    Context* _ctxt = nullptr;
    SMPTasks(Context& ctxt, TranspositionTable&, score_t) : _ctxt(&ctxt)
    {}
    void do_report()
    {
        if (Context::_report)
        {
            static std::vector<Context*> ctxts;
            ctxts.clear();
            ctxts.emplace_back(_ctxt);
            cython_wrapper::call(Context::_report, Context::_engine, ctxts);
        }
    }
};
#endif /* SMP */

std::vector<TaskData> SMPTasks::_tables;


/*
 * Search with iterative deepening.
 */
score_t search::iterative(Context& ctxt, TranspositionTable& table, int max_iter_count)
{
    score_t score = 0, prev_score = 0;
    max_iter_count = std::min(PLY_MAX, max_iter_count);

    for (int i = 1; i != max_iter_count;)
    {
        table._iteration = i;
        ASSERT(ctxt.iteration() == i);

        ctxt.set_search_window(score, prev_score);

        ctxt.reinitialize();

        {   /* SMP scope start */
            SMPTasks tasks(ctxt, table, score);
            const auto iter_score = search_iteration(ctxt, table, score);

            if (ctxt.is_cancelled())
                break;

            score = iter_score; /* retain the score for completed iterations */

            if (table._reset_window)
            {
            #if 0 /* debug */
                std::cout << "WINDOW RESET(" << i << "): " << score << " (";
                std::cout << table._w_alpha << ", " << table._w_beta << ")\n";
            #endif

                ctxt.cancel();
                table._reset_window = false;

                continue;
            }

            tasks.do_report();

        }   /* SMP scope end */

        ASSERT(ctxt.iteration() == ctxt._max_depth);

        /* post iteration info to Cython */
        if (Context::_on_iter)
        {
            IterationInfo info = { score, table.nodes(), 0, 0 };
            for (auto& t : SMPTasks::_tables)
                info.nodes += t._tt.nodes();

            const auto ms = Context::elapsed_milliseconds();
            info.knps = ms ? info.nodes / ms : info.nodes;
            info.milliseconds = ms;

            (*Context::_on_iter)(Context::_engine, &ctxt, &info);
        }
        ++i;
    }

    return score;
}


/*
 * Shift ply history and killer moves tables by two.
 * Should be called at the beginning of each new search.
 */
void TranspositionTable::shift()
{
    _pv.clear();

    shift_left_2(_killer_moves.begin(), _killer_moves.end());
    shift_left_2(_plyHistory.begin(), _plyHistory.end());
}
