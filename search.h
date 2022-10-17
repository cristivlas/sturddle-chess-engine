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

#include <algorithm>
#include <limits>
#include <memory>
#include <thread>
#include "chess.h"
#include "shared_hash_table.h"

constexpr int ONE_PLY = 16; /* fractional extensions */
constexpr int PLY_MAX = 100;

constexpr int PLY_HISTORY_MAX = 20;

constexpr score_t SCORE_MIN = -30000;
constexpr score_t SCORE_MAX =  30000;

constexpr score_t CHECKMATE = SCORE_MAX - 1;

#if MTDF_CSTAR_BISECT
    constexpr score_t MATE_HIGH = SCORE_MAX / 2;
#else
    constexpr score_t MATE_HIGH = SCORE_MAX - PLY_MAX;
#endif /* MTDF_CSTAR_BISECT */

constexpr score_t MATE_LOW  = -MATE_HIGH;

/* Aspiration window */
constexpr score_t HALF_WINDOW = chess::WEIGHT[chess::PieceType::PAWN] / 4;


constexpr score_t checkmated(int ply)
{
    return -CHECKMATE + ply;
}


constexpr score_t checkmating(int ply)
{
    return CHECKMATE + ply;
}


namespace search
{
    struct Context;
    class TranspositionTable;

    /*
     * Search algorithms
     */
    score_t negamax(Context&, TranspositionTable&);
    score_t mtdf(Context&, score_t, TranspositionTable&);
    score_t iterative(Context&, TranspositionTable&, int);


    using BaseMove = chess::BaseMove;
    using PV = std::vector<BaseMove>;
    using Color = chess::Color;
    using Move = chess::Move;
    using MovesList = chess::MovesList;
    using State = chess::State;

    /*
     * Butterfly table for history counts and counter-move heuristics.
     * https://www.chessprogramming.org/index.php?title=Butterfly_Boards
     */
    template<typename T> struct MoveTable
    {
        MoveTable()
        {
            clear();
        }

        INLINE void clear()
        {
            std::fill_n(&_table[0][0], 64 * 64, T());
        }

        INLINE T& operator[](const Move& move)
        {
            ASSERT(move);
            return _table[move.from_square()][move.to_square()];
        }


        INLINE const T& lookup(const Move& move) const
        {
            ASSERT(move);
            return _table[move.from_square()][move.to_square()];
        }

        T _table[64][64] = {};
    };


    template<typename T> struct PieceMoveTable
    {
        PieceMoveTable()
        {
            clear();
        }

        INLINE void clear()
        {
            std::fill_n(&_table[0][0], 7 * 64, T());
        }

        INLINE T& lookup(chess::PieceType piece_type, const Move& move)
        {
            ASSERT(piece_type != chess::PieceType::NONE);
            ASSERT(move);
            return _table[piece_type][move.to_square()];
        }

        INLINE const T& lookup(chess::PieceType piece_type, const Move& move) const
        {
            return const_cast<PieceMoveTable*>(this)->lookup(piece_type, move);
        }

        T _table[7][64] = {};
    };

    using KillerMoves = std::array<Move, 2>;
    using KillerMovesTable = std::array<KillerMoves, PLY_MAX>;


    enum class TT_Type : int8_t
    {
        NONE = 0,
        EXACT,
        LOWER,
        UPPER,
    };


#pragma pack(push, 4)
    class TT_Entry
    {
        template<typename T> friend class SharedHashTable;
        key_t       _lock = 0;

    public:
        TT_Type     _type = TT_Type::NONE;
        uint8_t     _age = 0;
        int8_t      _depth = std::numeric_limits<int8_t>::min();
        BaseMove    _hash_move;
        int16_t     _value = SCORE_MIN;
        uint64_t    _hash = 0;

    #if !NO_ASSERT
        void*       _owner = nullptr;
    #endif /* NO_ASSERT */

        INLINE bool is_lower() const { return _type == TT_Type::LOWER; }
        INLINE bool is_upper() const { return _type == TT_Type::UPPER; }
        INLINE bool is_valid() const { return _type != TT_Type::NONE; }

        INLINE bool matches(const State& state) const
        {
            return is_valid() && _hash == state.hash();
        }

        template<typename C>
        INLINE const int16_t* lookup_score(C& ctxt) const
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
    };
#pragma pack(pop)

    /*
     * Hash table, counter moves, historical counts.
     * Under SMP the hash table is shared between threads.
     */
    class TranspositionTable
    {
        using HashTable = SharedHashTable<TT_Entry>;

    #if USE_BUTTERFLY_TABLES
        using HistoryCounters = MoveTable<std::pair<int, int>>;
        using IndexedMoves = MoveTable<BaseMove>;
    #else
        using HistoryCounters = PieceMoveTable<std::pair<int, int>>;
        using IndexedMoves = PieceMoveTable<BaseMove>;
    #endif /* USE_BUTTERFLY_TABLES */

        using PlyHistoryCounters = std::array<MoveTable<std::pair<int, int>>, 2>;
        using PlyHistory = std::array<PlyHistoryCounters, PLY_HISTORY_MAX>;

        /* https://www.chessprogramming.org/Countermove_Heuristic */
        IndexedMoves        _countermoves[2];

        KillerMovesTable    _killer_moves; /* killer moves at each ply */
        HistoryCounters     _hcounters[2]; /* History heuristic counters. */
        static HashTable    _table;        /* shared hashtable */

    public:
        TranspositionTable() = default;
        ~TranspositionTable() = default;

               void clear();
        static void clear_shared_hashtable();
        static void increment_clock();

               void init() { clear(); shift(); increment_clock(); }

        int _tid = 0;
        int _iteration = 0;
        int _eval_depth = 0;
        PV  _pv; /* principal variation */
        PlyHistory _plyHistory;

        MovesList _moves;
        uint64_t _moves_hash = uint64_t(-1);

        /* search window bounds */
        score_t _w_alpha = SCORE_MIN;
        score_t _w_beta = SCORE_MAX;
        bool _reset_window = false;
        bool _probe_endtables = false;
        /* Stats for current thread */
        size_t _check_nodes = 0;
        size_t _eval_count = 0;
        size_t _endgame_nodes = 0;
        size_t _futility_prune_count = 0;
        size_t _history_counters = 0;
        size_t _history_counters_hit = 0;
        size_t _hits = 0;
        size_t _late_move_prune_count = 0;
        size_t _nodes = 0;
        size_t _nps = 0; /* nodes per second */
        size_t _null_move_cutoffs = 0;
        size_t _null_move_failed = 0;
        size_t _null_move_not_ok = 0;
        size_t _qsnodes = 0;
        size_t _reductions = 0;
        size_t _retry_reductions = 0;

        template<typename C>
        BaseMove lookup_countermove(const C& ctxt) const;

        const KillerMoves* get_killer_moves(int ply) const
        {
            ASSERT(ply < PLY_MAX);
            return &_killer_moves[ply];
        }

        const PV& get_pv() const { return _pv; }

        /* Reconstruct PV from hash table moves. Called by store_pv. */
        template<bool Debug=false> void get_pv_from_table(Context&, const Context&, PV&);

        template<typename C> const int16_t* lookup(C& ctxt);
        template<typename C> void store(C& ctxt, score_t alpha, int depth);

        void store(Context&, TT_Entry&, score_t, int depth);

        template<typename C> void store_countermove(C& ctxt);
        void store_killer_move(const Context&);

        template<bool Debug=false> void store_pv(Context&);

        const std::pair<int, int>& historical_counters(const State&, Color, const Move&) const;
        float history_score(int ply, const State&, Color, const Move&) const;

        size_t hits() const { return _hits; }
        size_t nodes() const { return _nodes; }

        /* number of occupied slots in the shared hashtable */
        static size_t size();

        /* percent usage (size over capacity) */
        static double usage();

        /* nodes per second */
        size_t nps() const { return _nps; }
        void set_nps(size_t nps) { _nps = nps; }

        void shift();

        void history_update_cutoffs(const Move&);
        void history_update_non_cutoffs(const Move&, bool failed_low);

        void update_stats(const Context&);

        static size_t max_hash_size();

        /* return size of hash table */
        static size_t get_hash_size();

        /* set hash table size in MB */
        static void set_hash_size(size_t);
    };


    /*
     * https://www.chessprogramming.org/History_Heuristic
     * https://www.chessprogramming.org/Relative_History_Heuristic
     */
    INLINE const std::pair<int, int>&
    TranspositionTable::historical_counters(
        const State& state,
        Color turn,
        const Move& move) const
    {
        ASSERT(move);

    #if USE_BUTTERFLY_TABLES
        return _hcounters[turn].lookup(move);
    #else
        const auto pt = state.piece_type_at(move.from_square());
        return _hcounters[turn].lookup(pt, move);
    #endif /* USE_BUTTERFLY_TABLES */
    }


    INLINE float
    TranspositionTable::history_score(
        int ply,
        const State& state,
        Color turn,
        const Move& move) const
    {
        float score = 0;
        if (ply < PLY_HISTORY_MAX)
        {
            const auto& h = _plyHistory[ply][turn].lookup(move);
            if (h.second)
                score = h.first / double(h.second * HISTORY_SCORE_DIV);
        }
        const auto& counters = historical_counters(state, turn, move);
        ASSERT(counters.first <= counters.second);

        return score + (counters.second < 1 ? 0 : (100.0 * counters.first) / counters.second);
    }


    INLINE void TranspositionTable::history_update_non_cutoffs(const Move& move, bool low)
    {
        if (move)
        {
            ASSERT(move._state);
            ASSERT(move._state->capture_value == 0);

            const auto turn = !move._state->turn; /* side that moved */
        #if USE_BUTTERFLY_TABLES
            auto& counters = _hcounters[turn][move];
        #else
            const auto pt = move._state->piece_type_at(move.to_square());
            auto& counters = _hcounters[turn].lookup(pt, move);
        #endif /* USE_BUTTERFLY_TABLES */

            ++counters.second;

            counters.first = std::max(0, counters.first - low * HISTORY_FAIL_LOW_PENALTY);
        }
    }


    INLINE void TranspositionTable::history_update_cutoffs(const Move& move)
    {
        ASSERT(move);
        ASSERT(move._state);
        ASSERT(move._state->capture_value == 0);

        const auto turn = !move._state->turn; /* side that moved */

    #if USE_BUTTERFLY_TABLES
        auto& counts = _hcounters[turn][move];
    #else
        const auto pt = move._state->piece_type_at(move.to_square());
        ASSERT(pt != chess::PieceType::NONE);
        auto& counts = _hcounters[turn].lookup(pt, move);
    #endif /* USE_BUTTERFLY_TABLES */
        ++counts.first;
        ++counts.second;
    }


    template<typename C>
    INLINE BaseMove TranspositionTable::lookup_countermove(const C& ctxt) const
    {
    #if USE_BUTTERFLY_TABLES
        return _countermoves[ctxt.turn()].lookup(ctxt._move);
    #else
        const auto pt = ctxt.state().piece_type_at(ctxt._move.to_square());
        return _countermoves[ctxt.turn()].lookup(pt, ctxt._move);
    #endif /* USE_BUTTERFLY_TABLES */
    }


    template<typename C>
    INLINE const int16_t* TranspositionTable::lookup(C& ctxt)
    {
        if (ctxt._ply == 0 || ctxt._excluded)
            return nullptr;

        /* expect repetitions to be dealt with before calling into this function */
        ASSERT(!ctxt.is_repeated());

        if (const auto p = _table.lookup_read(ctxt.state()))
        {
            ASSERT(p->matches(ctxt.state()));
            ctxt._tt_entry = *p;

            if constexpr(EXTRA_STATS)
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


    template<typename C>
    INLINE void TranspositionTable::store(C& ctxt, score_t alpha, int depth)
    {
        ASSERT(ctxt._score > SCORE_MIN);
        ASSERT(ctxt._score < SCORE_MAX);

        if constexpr(EXTRA_STATS)
            update_stats(ctxt);

        if (auto p = _table.lookup_write(ctxt.state(), depth))
            store(ctxt, *p, alpha, depth);
    }


    template<typename C>
    INLINE void TranspositionTable::store_countermove(C& ctxt)
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
} /* namespace */
