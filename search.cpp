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
#elif __APPLE__
#include <sys/sysctl.h>
#endif

#include <memory>
#include <sstream>
#include <utility>
#include "context.h"
#include "search.h"
#include "thread_pool.hpp"
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


template<bool Debug = false>
static void log_pv(const TranspositionTable& tt, const char* info)
{
    if constexpr(Debug)
    {
        std::ostringstream out;

        out << info << ": ";
        for (const auto& move : tt._pv)
            out << move << " ";

        Context::log_message(LogLevel::DEBUG, out.str());
    }
}


const int16_t* TT_Entry::lookup_score(Context& ctxt) const
{
    if (_depth >= ctxt.depth())
    {
        ASSERT(is_valid());

        if (is_lower())
        {
            ctxt._alpha = std::max<score_t>(ctxt._alpha, _value);
        }
        else if (is_upper())
        {
            ctxt._beta = std::min<score_t>(ctxt._beta, _value);
        }
        else
        {
            return &_value;
        }

        if (ctxt._alpha >= ctxt._beta)
        {
            ASSERT(_value >= ctxt._beta);
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


TranspositionTable::HashTablePtr TranspositionTable::_table =
    std::make_shared<TranspositionTable::HashTable>(pick_prime(TRANSPOSITION_TABLE_SLOTS));


static size_t mem_avail()
{
    unsigned long mem = 0;
#if __linux__
    struct sysinfo info = {};
    if (sysinfo(&info) == 0)
    {
        mem = info.freeram;
    }
#elif __APPLE__
    size_t len = sizeof(mem);
    static int mib[2] = { CTL_HW, HW_USERMEM };

    sysctl(mib, std::extent<decltype(mib)>::value, &mem, &len, nullptr, 0);

#else
    /* failover to psutil via Cython */
    mem = static_cast<unsigned long>(cython_wrapper::call(search::Context::_vmem_avail));
#endif /* __linux__ */

    return mem;
}


/* static */ size_t TranspositionTable::max_hash_size()
{
    ASSERT_ALWAYS(_table);

    const size_t cur_size = HashTable::size_in_bytes(_table->capacity());
    const size_t max_mem = mem_avail() + cur_size;

    return max_mem / ONE_MEGABYTE;
}


/* static */ size_t TranspositionTable::get_hash_size()
{
    ASSERT_ALWAYS(_table);
    return HashTable::size_in_bytes(_table->capacity()) / ONE_MEGABYTE;
}


/* static */ void TranspositionTable::set_hash_size(size_t MB)
{
    ASSERT_ALWAYS(_table);

    const auto max_size = max_hash_size(); /* in Megabytes */

    /* convert requested size to requested capacity */
    auto req_cap = (MB * ONE_MEGABYTE) / HashTable::size_in_bytes(1);

    /* prime number close to requested capacity */
    auto prime_cap = pick_prime(req_cap);

    while (true)
    {
        if (prime_cap == _table->capacity())
            return;

        auto size = HashTable::size_in_bytes(prime_cap) / ONE_MEGABYTE;
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
    Context::log_message(LogLevel::DEBUG, out.str(), false);
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
    _table->clear();
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


BaseMove TranspositionTable::lookup_countermove(const Context& ctxt) const
{
#if USE_BUTTERFLY_TABLES
    return _countermoves[ctxt.turn()].lookup(ctxt._move);
#else
    const auto pt = ctxt.state().piece_type_at(ctxt._move.to_square());
    return _countermoves[ctxt.turn()].lookup(pt, ctxt._move);
#endif /* USE_BUTTERFLY_TABLES */
}


const int16_t* TranspositionTable::lookup(Context& ctxt)
{
    if (ctxt._ply == 0 || ctxt._excluded)
        return nullptr;

    /* expect repetitions to be dealt with before calling into this function */
    ASSERT(!ctxt.is_repeated());

    if (const auto p = _table->lookup_read(ctxt.state()))
    {
        ASSERT(p->matches(ctxt.state()));
        ctxt._tt_entry = *p;
#if EXTRA_STATS
        ++_hits;
#endif /* EXTRA_STATS */
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
            entry._eval = SCORE_MIN;
            entry._type = TT_Type::NONE;
            entry._hash_move = BaseMove();
        }
       /*
        * Another thread has completed a deeper search from the time the current
        * thread has started searching (and probed the cache) in this position?
        */
        else if (entry._depth > depth && entry._version > ctxt._tt_entry._version)
        {
            return;
        }
    }

    ++entry._version;

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

    if (ctxt._tt_entry._eval != SCORE_MIN)
        entry._eval = ctxt._tt_entry._eval;

    entry._capt = ctxt._tt_entry._capt;
    entry._king_safety = ctxt._tt_entry._king_safety;
}


void TranspositionTable::store(Context& ctxt, score_t alpha, int depth)
{
    ASSERT(ctxt._score > SCORE_MIN);
    ASSERT(ctxt._score < SCORE_MAX);

#if EXTRA_STATS
    update_stats(ctxt);
#endif /* EXTRA_STATS */

    if (auto p = _table->lookup_write(ctxt.state(), depth))
    {
        store(ctxt, *p, alpha, depth);
    }
}


void TranspositionTable::store_countermove(Context& ctxt)
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

        ctxt.set_counter_move(ctxt._cutoff_move);
    }
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


template<bool Debug>
void TranspositionTable::get_pv_from_table(Context& root, const Context& ctxt, PV& pv)
{
    auto state = ctxt.state().clone();

    ASSERT(Context::epd(state) == ctxt.epd());
    ASSERT(state.hash() == ctxt.state().hash());

    /* keep track of state hashes, to detect cycles */
    std::unordered_set<size_t> visited;

    auto move = ctxt._best_move;

    if (!move)
        move = ctxt._tt_entry._hash_move;

    while (move)
    {
        if constexpr(Debug)
            std::cout << move << " ";

        /* Legality check, in case of a (low probability) hash collision. */
        if (state.piece_type_at(move.to_square()) == PieceType::KING)
            break;

        state.apply_move(move);

        /* Another legality check, and guard against infinite loops. */
        if (state.is_check(!state.turn) || !visited.insert(state.hash()).second)
            break;

        /* Add the move to the principal variation. */
        pv.emplace_back(move);

        auto p = _table->lookup_read(state);
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


template<bool Debug>
void TranspositionTable::store_pv(Context& root)
{
    PV pv;

    ASSERT(root._best_move);

    for (auto ctxt = &root; true; )
    {
        pv.emplace_back(ctxt->_move);

        if constexpr(Debug)
        {
            if (ctxt->_ply)
                std::cout << ctxt->_move << " ";
        }

        if (auto next = ctxt->next_ply())
        {
            if (next->is_null_move())
            {
                if (!_pv.empty())
                    return;
                else if constexpr(Debug)
                    std::cout << "NULL ";
            }
            if (ctxt->_best_move && next->_move == ctxt->_best_move)
            {
                ctxt = next;
                continue;
            }
        }

        if constexpr(Debug)
            std::cout << ctxt->_score << " hash: ";

        get_pv_from_table<Debug>(root, *ctxt, pv);
        break;
    }

    if constexpr(Debug)
        std::cout << "\n";

    if (pv.size() > _pv.size() || !std::equal(pv.begin(), pv.end(), _pv.begin()))
    {
        _pv.swap(pv);
        log_pv<Debug>(*this, "store_pv");
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

#if EXTRA_STATS
    ++ctxt.get_tt()->_null_move_failed;
#endif /* EXTRA_STATS */

    return false;
}


/*
 * https://www.chessprogramming.org/Multi-Cut
 */
static bool multicut(Context& ctxt, TranspositionTable& table)
{
    if (ctxt._ply == 0
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
    const auto min_cutoffs = MULTICUT_C - (ctxt.depth() > 5 && ctxt._tt_entry.is_lower());

    while (auto next_ctxt = ctxt.next(false, 0, move_count))
    {
        next_ctxt->_multicut_allowed = false;
        next_ctxt->_max_depth -= reduction;

        auto score = -negamax(*next_ctxt, table);

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

                /* Store it in the TT as a cutoff move. */
                ctxt._alpha = ctxt._score;

                /* Fix-up best move */
                ctxt.next_ply()->_move = ctxt._best_move = best_move;

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


score_t search::negamax(Context& ctxt, TranspositionTable& table)
{
    ASSERT(ctxt._beta > SCORE_MIN);
    ASSERT(ctxt._score <= ctxt._alpha);
    ASSERT(ctxt._alpha < ctxt._beta);
    ASSERT(ctxt._ply == 0 || !ctxt._move || ctxt._move._group < MoveOrder::UNORDERED_MOVES);

    ctxt.set_tt(&table);

    if (ctxt._ply != 0)
    {
        if (ctxt._fifty >= 100 || ctxt.is_repeated() > 0)
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

    #if REVERSE_FUTILITY_PRUNING
        /*
         * Reverse futility pruning: static eval stored in TT beats beta by a margin and
         * not in check, and no move w/ scores above MATE_HIGH in the hash table? Prune.
         */
        if (ctxt._ply != 0
            && !ctxt._excluded /* no reverse pruning during singular extension */
            && !ctxt.is_pv_node()
            && ctxt.depth() > 0
            && ctxt.depth() < 7
            && ctxt._tt_entry._eval < MATE_HIGH
            && ctxt._tt_entry._eval > ctxt._beta
                + std::max<score_t>(REVERSE_FUTILITY_MARGIN * ctxt.depth(), ctxt.improvement())
            && !ctxt.is_check())
        {
            ASSERT(ctxt._tt_entry._eval > SCORE_MIN);
            ASSERT(ctxt._tt_entry._eval < SCORE_MAX);

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

    #if EXTRA_STATS
        table._null_move_not_ok += !null_move;
    #endif /* EXTRA_STATS */

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
            #if EXTRA_STATS
                table._history_counters += next_ctxt->_move._group == MoveOrder::HISTORY_COUNTERS;
            #endif /* EXTRA_STATS */

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

                if (ctxt._ply != 0 && ctxt._ply < root_depth * 2)
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
                    if (ctxt.depth() >= 7
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
                         * _move_maker's _moves / _states stacks for the current context.
                         */
                        ContextBuffer buf;
                        auto s_ctxt = ctxt.clone(buf, ctxt._ply + 2);

                        s_ctxt->set_tt(ctxt.get_tt());
                        s_ctxt->set_moves(ctxt);
                        s_ctxt->_excluded = next_ctxt->_move;
                        s_ctxt->_max_depth = s_ctxt->_ply + (ctxt.depth() - 1) / 2;
                        s_ctxt->_alpha = s_beta - 1;
                        s_ctxt->_beta = s_beta;
                        s_ctxt->_score = SCORE_MIN;

                        const auto value = negamax(*s_ctxt, table);

                        if (value < s_beta && value > SCORE_MIN)
                        {
                            next_ctxt->_extension += ONE_PLY;

                            if (ctxt._double_ext <= DOUBLE_EXT_MAX
                                && !ctxt.is_pv_node()
                                && value + DOUBLE_EXT_MARGIN < s_beta)
                            {
                                ++next_ctxt->_max_depth;
                                ++next_ctxt->_double_ext;
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

                #if EXTRA_STATS
                    ++table._null_move_cutoffs;
                #endif /* EXTRA_STATS */

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

            #if EXTRA_STATS
                table._history_counters_hit += (next_ctxt->_move._group == MoveOrder::HISTORY_COUNTERS);
            #endif /* EXTRA_STATS */

                break; /* found a cutoff */
            }
            else if (next_ctxt->depth() >= HISTORY_MIN_DEPTH && !next_ctxt->is_capture())
            {
                const auto depth = std::max(1, next_ctxt->depth());
                const auto failed_low = next_ctxt->is_pv_node()
                    || (move_score + HISTORY_FAIL_LOW_MARGIN / depth <= table._w_alpha);

                table.history_update_non_cutoffs(next_ctxt->_move, failed_low);
            }

            /*
             * If the 1st move fails low at root it maybe unlikely for the
             * subsequent moves to improve things much (assuming reasonable
             * move ordering); readjust the aspiration window and retry.
             *
             * https://www.chessprogramming.org/PVS_and_Aspiration
             */
            if (ASPIRATION_WINDOW
                && ctxt._ply == 0
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
            if (!ctxt.has_moves())
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
 using ThreadPool = thread_pool<int>;
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

        #if VARIABLE_WINDOW
            _tables[i]._ctxt->_alpha = std::max<int>(SCORE_MIN, _root._alpha - 2.5 * (i + 1));
            _tables[i]._ctxt->_beta = std::min<int>(SCORE_MAX, _root._beta + 2.5 * (i + 1));
        #endif

            _tables[i]._tt._w_alpha = _tables[i]._ctxt->_alpha;
            _tables[i]._tt._w_beta = _tables[i]._ctxt->_beta;

            _tables[i]._ctxt->_max_depth += (i % 2) == 0;

            _tables[i]._ctxt->set_tt(&_tables[i]._tt);
            _tables[i]._ctxt->set_moves(_root);
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
    max_iter_count = std::min(PLY_MAX, max_iter_count);

    for (int i = 1; i != max_iter_count;)
    {
        table._iteration = i;
        ASSERT(ctxt.iteration() == i);

        ctxt.set_search_window(score);

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

            cython_wrapper::call(Context::_on_iter, Context::_engine, &ctxt, &info);
        }

        ++i;
    }

    return score;
}


/*
 * Shift PV and killer moves tables by two.
 * Should be called at the beginning of each new search.
 */
void TranspositionTable::shift()
{
    if (_pv.size() >= 2)
    {
        std::rotate(_pv.begin(), _pv.begin() + 2, _pv.end());
        _pv.resize(_pv.size() - 2);
    }

    shift_left_2(_killer_moves.begin(), _killer_moves.end());
    shift_left_2(_plyHistory.begin(), _plyHistory.end());

    log_pv(*this, "shift");
}
