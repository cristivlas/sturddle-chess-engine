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
/*
 * TranspositionTable and related data structs.
 * Search algorithms: Negamax with TranspositionTable, MTD(f)
 */
#if __linux__
#include <sys/sysinfo.h>
#endif

#include <sstream>
#include <utility>
#include "context.h"
#include "shared_hash_table.h"
#include "thread_pool.hpp"
#include "search.h"
#include "utility.h"

#if HAVE_INT128
/* primes.hpp requires __int128 */
    #define USE_PRIMES_HPP true
    #include "primes.hpp"
#else
    #define USE_PRIMES_HPP false
#endif

using namespace chess;
using namespace search;



static void print_move(std::ostream& out, const Move& move)
{
    out << move.uci() << " score=" << move._score << "\n";
}


void TT_Entry::print(std::ostream& out) const
{
    print_move(out << "hash_move=", _hash_move);
    out << "(" << _alpha << ", " << _value << ", " << _beta << ") depth=" << _depth;
    out << " capt=" << _capt << " eval=" << _eval << "\n";
}


const score_t* TT_Entry::lookup_score(Context& ctxt) const
{
    if (_depth >= ctxt.depth() && is_valid())
    {
        if (is_lower())
        {
            ctxt._alpha = std::max(ctxt._alpha, _value);
        }
        else if (is_upper())
        {
            ctxt._beta = std::min(ctxt._beta, _value);
        }
        else if (ctxt._alpha <= _alpha && ctxt._beta >= _beta)
        {
            ASSERT(_value > _alpha && _value < _beta);
            return &_value;
        }

        if (ctxt._alpha >= ctxt._beta)
        {
            return &_value;
        }
    }

    return nullptr;
}


/*
 * Thanks to Thomas Neumann for primes.hpp
 * http://databasearchitects.blogspot.com/2020/01/all-hash-table-sizes-you-will-ever-need.html
 */
static inline size_t pick_prime(size_t n)
{
#if USE_PRIMES_HPP
    return primes::Prime::pick(n).get();
#else
    return n;
#endif /* USE_PRIMES_HPP */
}

static constexpr int ONE_MEGABYTE = 1024 * 1024;


TranspositionTable::TablePtr TranspositionTable::_table =
    std::make_shared<SharedHashTable>(pick_prime(TRANSPOSITION_TABLE_SLOTS));


static size_t mem_avail()
{
    unsigned long mem = 0;
#if __linux__
    struct sysinfo info = {};
    if (sysinfo(&info) == 0)
    {
        mem = info.freeram;
    }
#else
    mem = unsigned long(cython_wrapper::call(search::Context::_vmem_avail));
#endif /* __linux__ */

    return mem;
}


/* static */ size_t TranspositionTable::max_hash_size()
{
    ASSERT_ALWAYS(_table);

    size_t cur_size = SharedHashTable::size_in_bytes(_table->capacity());
    size_t max_mem = mem_avail() > 2 * cur_size ? mem_avail() : cur_size;

    return max_mem / ONE_MEGABYTE;
}


/* static */ size_t TranspositionTable::get_hash_size()
{
    ASSERT_ALWAYS(_table);
    return SharedHashTable::size_in_bytes(_table->capacity()) / ONE_MEGABYTE;
}


/* static */ void TranspositionTable::set_hash_size(size_t MB)
{
    ASSERT_ALWAYS(_table);

    const auto max_size = max_hash_size(); /* in Megabytes */

    /* convert requested size to requested capacity */
    auto req_cap = (MB * ONE_MEGABYTE) / SharedHashTable::size_in_bytes(1);

    /* prime number close to requested capacity */
    auto prime_cap = pick_prime(req_cap);

    while (true)
    {
        if (prime_cap == _table->capacity())
            return;

        auto size = SharedHashTable::size_in_bytes(prime_cap) / ONE_MEGABYTE;
        if (size < max_size)
            break;

        if (req_cap == 0)
        {
            Context::log_message(LogLevel::ERROR, "hash: cannot resize");
            return;
        }
        else
        {
            --req_cap;
            prime_cap = pick_prime(req_cap);
        }
    }
    _table->resize(prime_cap);

    std::ostringstream out;
    out << "hash: req=" << MB << " new=" << get_hash_size() << " free=" << mem_avail() / ONE_MEGABYTE;
    Context::log_message(LogLevel::INFO, out.str());
}


/* static */ void TranspositionTable::clear_shared_hashtable()
{
    _table->clear();

#if TUNING_ENABLED
    /* hack */
    Context::init();
#endif
}


/* static */ void TranspositionTable::increment_clock()
{
    _table->increment_clock();
}


/* static */ size_t TranspositionTable::size()
{
    return _table->size();
}


/* static */ double TranspositionTable::usage()
{
    return (100.0 * _table->size()) / _table->capacity();
}


Move TranspositionTable::lookup_countermove(const Context& ctxt) const
{
#if USE_BUTTERFLY_TABLES
    return _countermoves[ctxt.turn()].lookup(ctxt._move);
#else
    const auto pt = ctxt.state().piece_type_at(ctxt._move.to_square());
    return _countermoves[ctxt.turn()].lookup(pt, ctxt._move);
#endif /* USE_BUTTERFLY_TABLES */
}


const score_t* TranspositionTable::lookup(Context& ctxt)
{
    if (ctxt._excluded)
        return nullptr;

    /* evaluate repetitions rather than using stored value */
    if (ctxt.is_repeated() > 0)
        return nullptr;

    if (auto p = _table->lookup(ctxt.state(), false))
    {
        ASSERT(p->matches(ctxt.state()));
        ctxt._tt_entry = *p;

        ++_hits;
    }

    /* http://www.talkchess.com/forum3/viewtopic.php?topic_view=threads&p=305236&t=30788 */
    if (ctxt._ply && !ctxt.is_pv_node())
    {
        if (auto value = ctxt._tt_entry.lookup_score(ctxt))
        {
            ctxt._score = *value;
            return value;
        }
    }

    if (ctxt._move)
        ctxt.set_counter_move(lookup_countermove(ctxt));

    return nullptr;
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


void TranspositionTable::store(Context& ctxt, TT_Entry& entry, score_t alpha)
{
    ASSERT(ctxt._score > SCORE_MIN);
    ASSERT(ctxt._score < SCORE_MAX);
    ASSERT(alpha < ctxt._beta);
    ASSERT(ctxt._alpha >= alpha);

    if (entry.is_valid() && !entry.matches(ctxt.state()))
    {
        entry = TT_Entry(entry._lock);
    }
   /*
    * Another thread has completed a deeper search from the time the current
    * thread has started searching (and probed the cache) in this position?
    */
    else if (entry._depth > ctxt.depth() && entry._version > ctxt._tt_entry._version)
    {
        return;
    }

    ++entry._version;

    /* Store hash move (cutoff or best) */
    auto move = ctxt._cutoff_move;

    if (!move && ctxt.best())
        move = ctxt.best()->_move;

    if (move
        || (entry.is_lower() && ctxt._score < entry._value)
        || (entry.is_upper() && ctxt._score > entry._value)
       )
        entry._hash_move = move;

    entry._alpha = alpha;
    entry._beta = ctxt._beta;
    entry._value = ctxt._score;
    entry._hash = ctxt.state().hash();
    entry._depth = ctxt.depth();

    /* cache static eval */
    if (ctxt._tt_entry._eval > SCORE_MIN)
        entry._eval = ctxt._tt_entry._eval;

    /* cache capture estimates */
    if (ctxt._tt_entry._capt > SCORE_MIN)
        entry._capt = ctxt._tt_entry._capt;

    entry._singleton = ctxt._tt_entry._singleton;
}


void TranspositionTable::store(Context& ctxt, score_t alpha)
{
    ASSERT(ctxt._score > SCORE_MIN);
    ASSERT(ctxt._score < SCORE_MAX);

    update_stats(ctxt);

    if (auto p = _table->lookup(ctxt.state(), true, ctxt.depth()))
    {
        store(ctxt, *p, alpha);
    }
}


void TranspositionTable::store_countermove(const Context& ctxt)
{
    if (ctxt._move)
    {
        ASSERT(ctxt._cutoff_move);
#if USE_BUTTERFLY_TABLES
        _countermoves[ctxt.turn()][ctxt._move] = ctxt._cutoff_move;
#else
        const auto pt = ctxt.state().piece_type_at(ctxt._move.to_square());
        _countermoves[ctxt.turn()].lookup(pt, ctxt._move) = ctxt._cutoff_move;
#endif /* USE_BUTTERFLY_TABLES */
    }
}


void TranspositionTable::store_killer_move(const Context& ctxt)
{
    ASSERT(ctxt._score); /* do not store draws */

#if KILLER_MOVE_HEURISTIC
    const auto& move = ctxt._cutoff_move;
    if (!move)
        return;

    ASSERT(!ctxt.state().is_capture(move));

    if (_killer_moves.size() <= size_t(ctxt._ply))
        _killer_moves.resize(2 * ctxt._ply + 1);

    auto& killers = _killer_moves[ctxt._ply];

    if (killers[0] != move)
    {
        killers[1] = killers[0];
        killers[0] = move;
        killers[0]._score = ctxt._score;
    }

#endif /* KILLER_MOVE_HEURISTIC */
}


void TranspositionTable::get_pv_from_table(
    Context& root,
    const Context& ctxt,
    BaseMovesList& pv,
    bool print)
{
    auto state = ctxt.state().clone();

    ASSERT(Context::_epd(state) == ctxt.epd());
    ASSERT(state.hash() == ctxt.state().hash());

    /* keep track of state hashes, to detect cycles */
    std::unordered_set<size_t> visited;

    auto move = ctxt._tt_entry._hash_move;
    if (!move) /* try the hash table */
        if (auto p = _table->lookup(state, false))
            move = p->_hash_move;

    while (move)
    {
        if (print)
            std::cout << "[" << move.uci() << "] ";

        /* Legality check, in case of a (low probability) hash collision. */
        if (state.piece_type_at(move.to_square()) == PieceType::KING)
            break;

        state.apply_move(move);

        /* Another legality check, and guard against infinite loops. */
        if (state.is_check(!state.turn) || !visited.insert(state.hash()).second)
            break;

        /* Add the move to the principal variation. */
        pv.emplace_back(move);

        auto p = _table->lookup(state, false);
        if (!p)
            break;

        ASSERT(p->matches(state));

        move = p->_hash_move;
    }

    if (state.is_checkmate())
    {
        /* The parity of the PV length tells which side is winning. */
        /* Subtract one for the move that lead to the root position */
        root._mate_detected = int(pv.size()) - 1;
    }
}


void TranspositionTable::store_pv(Context& root, bool print)
{
    ASSERT(root.best());

    _pv.clear();

    for (auto ctxt = &root; true; )
    {
        _pv.emplace_back(ctxt->_move);

        if (print && ctxt->_ply)
            std::cout << ctxt->_move.uci() << " ";

        if (auto next = ctxt->best())
        {
            ctxt = next.get();
            continue;
        }

        get_pv_from_table(root, *ctxt, _pv, print);
        break;
    }

    if (print)
        std::cout << "\n";
}


/*
 * https://www.chessprogramming.org/History_Heuristic
 * https://www.chessprogramming.org/Relative_History_Heuristic
 */
const std::pair<int, int>&
TranspositionTable::historical_counters(const Context& ctxt, const Move& move) const
{
    ASSERT(move);
    const auto turn = ctxt.turn();

#if USE_BUTTERFLY_TABLES
    return _hcounters[turn].lookup(move);
#else
    const auto pt = ctxt.state().piece_type_at(move.from_square());
    return _hcounters[turn].lookup(pt, move);
#endif /* USE_BUTTERFLY_TABLES */
}


float TranspositionTable::history_score(const Context& ctxt, const Move& move) const
{
    const auto& counters = historical_counters(ctxt, move);
    return counters.second < 1 ? 0 : (100.0 * counters.first) / counters.second;
}


void TranspositionTable::history_update_non_cutoffs(const Move& move)
{
    if (move)
    {
        ASSERT(move._state);
        ASSERT(move._state->capture_value == 0);

        const auto turn = !move._state->turn; /* side that moved */
#if USE_BUTTERFLY_TABLES
        ++_hcounters[turn][move].second;
#else
        const auto pt = move._state->piece_type_at(move.to_square());
        ++_hcounters[turn].lookup(pt, move).second;
#endif /* USE_BUTTERFLY_TABLES */
    }
}


void TranspositionTable::history_update_cutoffs(const Move& move)
{
    ASSERT(move);
    ASSERT(move._state);
    ASSERT(move._state->capture_value == 0);

    const auto turn = !move._state->turn; /* side that moved */

#if USE_BUTTERFLY_TABLES
    auto& counts = _hcounters[turn][move];
#else
    const auto pt = move._state->piece_type_at(move.to_square());
    ASSERT(pt != PieceType::NONE);
    auto& counts = _hcounters[turn].lookup(pt, move);
#endif /* USE_BUTTERFLY_TABLES */
    ++counts.first;
    ++counts.second;
}


/*
 * https://www.stmintz.com/ccc/index.php?id=76542
 */
bool verify_null_move(Context& ctxt, Context& null_move_ctxt)
{
    /* consistency checks */
    ASSERT(null_move_ctxt.is_null_move());
    ASSERT(ctxt.next_move_index() == 0);
    ASSERT(!ctxt.best());
    ASSERT(ctxt.turn() != null_move_ctxt.turn());
    ASSERT(&ctxt == null_move_ctxt._parent);

    null_move_ctxt.rewind();

    null_move_ctxt._null_move_allowed[null_move_ctxt.turn()] = false;
    null_move_ctxt._score = SCORE_MIN;
    null_move_ctxt._alpha = ctxt._beta - 1;
    null_move_ctxt._beta  = ctxt._beta;

    const auto score = negamax(null_move_ctxt, *ctxt.get_tt());

    if (ctxt.is_cancelled())
        return false;

    if (score >= ctxt._beta)
        return true; /* verification successful */

    /*
     * null move refuted? update capture_square and mate_detected
     */
    if (const auto& best = null_move_ctxt.best())
    {
        if (best->is_capture())
            ctxt._capture_square = best->_move.to_square();

        if (-score > MATE_HIGH)
        {
            ASSERT(best->_score == -score);

            ctxt._mate_detected = CHECKMATE + score + 1;
            ASSERT(ctxt._mate_detected > 0);
        }
    }

    ++ctxt.get_tt()->_null_move_failed;
    return false;
}


/*
 * https://www.chessprogramming.org/Multi-Cut
 */
static bool multicut(Context& ctxt, TranspositionTable& table)
{
    if (ctxt._ply == 0
        || !ctxt._multicut_allowed
        || ctxt.depth() <= MULTICUT_REDUCTION + 1
        || ctxt.is_singleton()
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

    ASSERT(!ctxt._excluded);

    int move_count = 0, cutoffs = 0;

    ContextPtr best_move;

    /*
     * A take on the idea from https://skemman.is/bitstream/1946/9180/1/research-report.pdf
     * The paper proposes trying the heuristic only at nodes that have produced cutoffs at
     * lower depths, to increase its chances to succeed. Here, the idea is to try the heuristic
     * regardless, but lower the count of cutoffs required to "succeed" if the position has
     * produced cutoffs before.
     */
    const auto min_cutoffs = MULTICUT_C - (ctxt.depth() > 5 && ctxt._tt_entry.is_lower());

    while (auto next_ctxt = ctxt.next())
    {
        next_ctxt->_multicut_allowed = false;
        next_ctxt->_max_depth -= MULTICUT_REDUCTION;

        auto score = -negamax(*next_ctxt, table);

        if (ctxt.is_cancelled())
            return false;

        if (score >= ctxt._beta)
        {
            if (!best_move || score > -best_move->_score)
                best_move = next_ctxt;

            if (++cutoffs >= min_cutoffs)
            {
                ctxt._score = score >= MATE_HIGH ? ctxt._beta : score;

                /* Store it in the TT as a cutoff move. */
                ctxt._alpha = ctxt._score;

                /* Fix-up best move just in case this becomes the new PV. */
            #if LAZY_STATE_COPY
                best_move->copy_move_state();
            #endif
                ctxt.set_best(best_move);
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


inline void
update_pruned(TranspositionTable& table, Context& ctxt, const Context& next, size_t& count)
{
    ASSERT(!next.is_capture());

    ++ctxt._pruned_count;
    ++count;
}


score_t search::negamax(Context& ctxt, TranspositionTable& table)
{
    ASSERT(ctxt._beta > SCORE_MIN);
    ASSERT(ctxt._score <= ctxt._alpha);

    ctxt.set_tt(&table);
    /*
     * https://www.chessprogramming.org/Node_Types#PV
     */
    ctxt._is_pv = ctxt._algorithm > NEGAMAX
        && !ctxt.is_null_move()
        && (ctxt.is_leftmost() || ctxt._alpha + 1 < ctxt._beta);

    if (ctxt._alpha + 1 < ctxt._beta)
    {
        ASSERT(ctxt._algorithm != NEGASCOUT || ctxt.is_leftmost() || ctxt.is_retry());
    }

    if (const auto* p = table.lookup(ctxt))
    {
        ASSERT(ctxt._score == *p);
        ASSERT(!ctxt._excluded);

        return *p;
    }

    table._nodes += !COUNT_VALID_MOVES_AS_NODES;

    const auto alpha = ctxt._alpha;

    if (ctxt.is_leaf())
    {
        ctxt._is_terminal = true;
        ctxt._score = ctxt.evaluate();

        ASSERT(ctxt._score > SCORE_MIN);
        ASSERT(ctxt._score < SCORE_MAX);
    }
    else if (!multicut(ctxt, table) && !ctxt.is_cancelled())
    {
        ASSERT(ctxt._alpha < ctxt._beta);

    #if REVERSE_FUTILITY_PRUNING
        /*
         * Reverse futility pruning: static eval stored in TT beats beta by a margin and
         * not in check, and no move w/ scores above MATE_HIGH in the hash table? Prune.
         */
        if (ctxt._ply != 0
            && !ctxt.is_singleton()
            && !ctxt._excluded
            && !ctxt.is_pv_node()
            && ctxt.depth() > 0
            && ctxt.depth() < 7
            && ctxt._tt_entry._eval < MATE_HIGH
            && ctxt._tt_entry._eval > ctxt._beta + (50 - ctxt.is_improving()) * ctxt.depth()
            && !ctxt.is_check())
        {
            ASSERT(ctxt._tt_entry._eval > SCORE_MIN);
            return ctxt._tt_entry._eval;
        }
    #endif /* REVERSE_FUTILITY_PRUNING */

        /* Reduce depth by 2 if PV node not found in the TT (idea from SF). */
        if (ctxt._ply
            && ctxt.is_pv_node()
            && ctxt.depth() >= 6
            && !ctxt._tt_entry.is_valid()
            && ctxt.can_reduce()
           )
            ctxt._max_depth -= 2;

        bool null_move = ctxt.is_null_move_ok();
        table._null_move_not_ok += !null_move;

        auto moves_count = 0;

        const bool prune_ok = ctxt.can_forward_prune();
        const auto futility = prune_ok ? ctxt.futility_margin() : 0;

        /* iterate over moves */
        while (auto next_ctxt = ctxt.next(std::exchange(null_move, false), false, futility))
        {
            if (next_ctxt->_move._group == MoveOrder::HISTORY_COUNTERS)
                ++table._history_counters;

            if (!next_ctxt->is_null_move())
            {
                /* Futility pruning */
                if (futility && moves_count)
                {
                    ASSERT(prune_ok);
                    ASSERT(moves_count > 0);

                    const auto val = futility - next_ctxt->evaluate_material();

                    if ((val < ctxt._alpha || val < ctxt._score) && next_ctxt->can_prune(moves_count))
                    {
                        update_pruned(table, ctxt, *next_ctxt, table._futility_prune_count);
                        continue;
                    }
                }

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
                if (ctxt._ply != 0
                    && !next_ctxt->is_singleton()
                    && ctxt.depth() >= 7
                    && ctxt._tt_entry.is_lower()
                    && abs(ctxt._tt_entry._value) < MATE_HIGH
                    && next_ctxt->_move == ctxt._tt_entry._hash_move
                    && ctxt._tt_entry._depth >= ctxt.depth() - 3)
                {
                    ASSERT(!ctxt._excluded);
                    ASSERT(ctxt._tt_entry.is_valid());

                    auto s_beta = std::max(ctxt._tt_entry._value - ctxt.singular_margin(), SCORE_MIN + 1);

                    /*
                     * Hack: use ply + 2 for the singular search to avoid clobbering
                     * the current context _move_maker's _moves and _states stacks.
                     */
                    auto s_ctxt = ctxt.clone(ctxt._ply + 2);

                    s_ctxt->set_initial_moves(ctxt.get_moves());
                    s_ctxt->_excluded = next_ctxt->_move;
                    s_ctxt->_max_depth = s_ctxt->_ply + (ctxt.depth() - 1) / 2;
                    s_ctxt->_alpha = s_beta - 1;
                    s_ctxt->_beta = s_beta;
                    s_ctxt->_score = SCORE_MIN;

                    const auto value = negamax(*s_ctxt, table);

                    if (ctxt.is_cancelled())
                        break;

                    if (value < s_beta && value > SCORE_MIN)
                    {
                        next_ctxt->_extension += ONE_PLY;
                    }
                    /*
                     * Got another fail-high from the (reduced) search that skipped the known
                     * cutoff move, so there must be multiple cutoffs, do 2nd multicut pruning.
                     */
                    else if (s_beta >= ctxt._beta)
                    {
                        /* Store it in the TT as a cutoff move. */
                        ctxt._alpha = ctxt._score = s_beta;
                        /*
                         * Same as with null move pruning below, make sure that
                         * the move is updated in the TT when storing the result.
                         */
                        ctxt._cutoff_move = next_ctxt->_move;
                        break;
                    }
                    else if (ctxt._tt_entry._value >= ctxt._beta && next_ctxt->can_reduce())
                    {
                        next_ctxt->_max_depth -= 2;
                    }
                }
            #endif /* SINGULAR_EXTENSION */

                next_ctxt->extend();

                /* Late-move reduction and pruning */
                if (moves_count && next_ctxt->late_move_reduce(prune_ok) == LMR::Prune)
                {
                    update_pruned(table, ctxt, *next_ctxt, table._late_move_prune_count);
                    continue;
                }

                if (!next_ctxt->is_retry())
                    ++moves_count;
            }

            /*
             * Recursively search next move.
             */
            const auto move_score = -negamax(*next_ctxt, table);

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
                    ++table._null_move_cutoffs;

                    /* verification not expected to modify ctxt._score */
                    ASSERT(ctxt._score == move_score);

                    /* do not trust checkmate results from heuristic */
                    if (ctxt._score > MATE_HIGH)
                        ctxt._score = ctxt._beta;

                    /*
                     * Put the hash move (if available) back in the TT when
                     * storing this result; could skip storing and just return
                     * ctxt._score from here, but that hurts MTD(f) performance.
                     * This should only be needed in the multiple cores case,
                     * with another thread possibly writing to the same entry
                     * in between the current thread's probe time and store time.
                     */
                    if (ctxt._tt_entry.is_lower() && ctxt._tt_entry._hash_move)
                        ctxt._cutoff_move = ctxt._tt_entry._hash_move;
                }
                else
                {
                    table.store_countermove(ctxt);

                    /* update non-capturing killer moves */
                    if (!next_ctxt->is_capture())
                    {
                        /* sanity checks */
                        ASSERT (next_ctxt->_move);
                        ASSERT (next_ctxt->_move == ctxt._cutoff_move);
                        ASSERT (!ctxt.state().is_capture(ctxt._cutoff_move));

                        /* zero-score moves may mean draw (path-dependent) */
                        if (move_score && !next_ctxt->is_qsearch())
                            table.store_killer_move(ctxt);

                        table.history_update_cutoffs(next_ctxt->_move);
                    }
                }

                table._history_counters_hit +=
                    (next_ctxt->_move._group == MoveOrder::HISTORY_COUNTERS);

                break; /* found a cutoff */
            }
            else if (!next_ctxt->is_capture())
            {
                table.history_update_non_cutoffs(next_ctxt->_move);
            }

            /*
             * If the 1st move fails low at root it may not be likely for
             * subsequent moves to improve things much (assuming reasonable
             * move ordering); readjust the aspiration window and retry, so
             * that the upper bound gets more accurate.
             * https://www.chessprogramming.org/PVS_and_Aspiration
             */
            if (ctxt._ply == 0 && moves_count == 1 && move_score < table._w_alpha)
            {
                ASSERT(!next_ctxt->is_null_move());
                table._reset_window = true;
                return move_score;
            }
        }
        ASSERT(ctxt._score <= ctxt._alpha || ctxt.best());

        if (!ctxt.is_cancelled())
        {
            if (!ctxt.has_moves())
            {
                /* checkmate or stalemate? */
                ctxt._score = ctxt.evaluate_end();

                ASSERT(ctxt._score > SCORE_MIN);
                ASSERT(ctxt._score < SCORE_MAX);
            }
            else if (!ctxt._excluded)
            {
                /* algorithm invariants */
                ASSERT(ctxt._score > SCORE_MIN);
                ASSERT(ctxt._score < SCORE_MAX);
                ASSERT(ctxt._alpha >= ctxt._score);
            }
        }
    }

    /*
     * Do not store 0 scores in the TT - they could be draws by repetition,
     * which are path-dependent; and the general wisdom is to not store root
     * nodes either (https://www.stmintz.com/ccc/index.php?id=93686)
     */
    if (ctxt._score && ctxt._ply && !ctxt._excluded && !ctxt.is_cancelled())
        table.store(ctxt, alpha);

    return ctxt._score;
}


/*
 * https://en.wikipedia.org/wiki/MTD(f)
 */
score_t search::mtdf(Context& ctxt, score_t first, TranspositionTable& table)
{
    ASSERT_ALWAYS(ctxt._algorithm == Algorithm::MTDF);
    ASSERT_ALWAYS(ctxt._ply == 0);

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

        ctxt._score = std::max(SCORE_MIN, b - 1);
        ctxt._alpha = b - 1;
        ctxt._beta = b;

        g = negamax(ctxt, table);

        ASSERT(g < SCORE_MAX); /* sanity check */

        if (table._reset_window || ctxt.is_cancelled())
            break;

        if (g < b)
            upper = g;
        else
            lower = g;

        ctxt.rewind(0, MTDF_REORDER_MOVES);
    }

    return (ctxt._score = g);
}


/*
 * Iterative deepening.
 */
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

    return score;
}


#if SMP
/****************************************************************************
 * Lazy SMP. https://www.chessprogramming.org/Lazy_SMP
 ****************************************************************************/
static std::unique_ptr<thread_pool> pool;


static size_t start_pool()
{
    if (!pool || pool->get_thread_count() + 1 != size_t(Context::cpu_cores()))
    {
        if (Context::cpu_cores() <= 1)
            return 0;

        pool = std::make_unique<thread_pool>(Context::cpu_cores() - 1);
    }

    return pool->get_thread_count();
}

struct TaskData
{
    ContextPtr _ctxt;
    TranspositionTable _tt;
};


class SMPTasks
{
    SMPTasks(const SMPTasks&) = delete;
    SMPTasks& operator=(const SMPTasks&) = delete;

    Context& _root;

public:
    static std::vector<TaskData> _tables;

    SMPTasks(Context& ctxt, TranspositionTable& table, score_t score)
        : _root(ctxt)
    {
        const auto thread_count = start_pool();
        ASSERT(thread_count + 1 == size_t(Context::cpu_cores()));

        if (table._iteration == 1)
        {
            std::vector<TaskData>(thread_count).swap(_tables);

            /* run 1st iteration on one thread only */
            return;
        }

        for (size_t i = 0; i < thread_count; ++i)
        {
            _tables[i]._tt._iteration = table._iteration;
            _tables[i]._tt._w_alpha = table._w_alpha;
            _tables[i]._tt._w_beta = table._w_beta;

            /* copy principal variation from main thread */
            if (_tables[i]._tt._pv.empty())
                _tables[i]._tt._pv = table._pv;

            auto t_ctxt = _root.clone();
            t_ctxt->set_tt(&_tables[i]._tt);
            t_ctxt->_tid = int(_tables.size() - i);
            t_ctxt->_max_depth += (i % 2) == 0;

            t_ctxt->set_initial_moves(_root.get_moves());

            if (auto& prev_ctxt = _tables[i]._ctxt)
            {
                if (prev_ctxt->_tt_entry._hash_move)
                    t_ctxt->_tt_entry._hash_move = prev_ctxt->_tt_entry._hash_move;
            }

            ASSERT(t_ctxt->_tid > 0);

            pool->push_task([&, i, t_ctxt, score]() mutable {
                try
                {
                    _tables[i]._ctxt = t_ctxt;

                    if (search_iteration(*t_ctxt, *t_ctxt->get_tt(), score) > SCORE_MIN)
                    {
                        if (t_ctxt->best())
                            t_ctxt->get_tt()->store_pv(*t_ctxt);
                    }
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
        if (pool)
        {
            Context::cancel();
            pool->wait_for_tasks();
        }
        if (Context::_report)
        {
            std::vector<ContextPtr> ctxts;
            for (const auto& table : _tables)
                ctxts.emplace_back(table._ctxt);

            cython_wrapper::call(Context::_report, Context::_engine, ctxts);
        }

        for (auto& t : _tables)
        {
            t._ctxt.reset();
        }
    }
};


std::vector<TaskData> SMPTasks::_tables;

#else
struct SMPTasks /* dummy */
{
    SMPTasks(const Context&, TranspositionTable&, score_t) {}
};

#endif /* SMP */


/*
 * Search with iterative deepening.
 */
score_t search::iterative(Context& ctxt, TranspositionTable& table, int max_iter_count)
{
    score_t score = 0;

    for (int i = 1; i != max_iter_count; )
    {
        table._iteration = i;

        ASSERT(ctxt.iteration() == i);

        ctxt.set_search_window(score);
        ctxt.reinitialize();

        {   /* SMP scope */
            SMPTasks tasks(ctxt, table, score);
            const auto iter_score = search_iteration(ctxt, table, score);

            if (ctxt.is_cancelled())
                break;

            score = iter_score; /* retain the score for completed iterations */

            if (table._reset_window)
            {
                ctxt.cancel();
                table._reset_window = false;

                continue; /* keep looping at same depth */
            }
        }

        if (const auto& best = ctxt.best())
        {
            ctxt._prev = best->_move;
            table.store_pv(ctxt);
        }

        ASSERT(ctxt.ref_count());
        ASSERT(ctxt.iteration() == ctxt._max_depth);

        cython_wrapper::call(Context::_on_iter, Context::_engine, ContextPtr(&ctxt), score);

        ++i;
    }

    return score;
}
