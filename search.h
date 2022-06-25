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

constexpr score_t SCORE_MIN = -100000;
constexpr score_t SCORE_MAX =  100000;

constexpr score_t CHECKMATE = SCORE_MAX - 1;
constexpr score_t MATE_HIGH = SCORE_MAX - PLY_MAX;
constexpr score_t MATE_LOW  = -MATE_HIGH;

/* Aspiration window */
constexpr score_t HALF_WINDOW = chess::WEIGHT[chess::PieceType::PAWN] / 4;


namespace search
{
    struct Context;
    class TranspositionTable;

#if USE_MOVES_CACHE
    using MovesCache = SharedHashTable<struct Moves, 1>;
    extern std::shared_ptr<MovesCache> moves_cache;
#endif /* USE_MOVES_CACHE */

    /*
     * Search algorithms
     */
    score_t negamax(Context&, TranspositionTable&);
    score_t mtdf(Context&, score_t, TranspositionTable&);
    score_t iterative(Context&, TranspositionTable&, int);


    using BaseMove = chess::BaseMove;
    using BaseMovesList = std::vector<BaseMove>;
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

        inline void clear()
        {
            std::fill_n(&_table[0][0], 64 * 64, T());
        }

        inline T& operator[](const Move& move)
        {
            ASSERT(move);
            return _table[move.from_square()][move.to_square()];
        }

        inline const T& lookup(const Move& move) const
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

        inline void clear()
        {
            std::fill_n(&_table[0][0], 7 * 64, T());
        }

        inline T& lookup(chess::PieceType piece_type, const Move& move)
        {
            ASSERT(piece_type != chess::PieceType::NONE);
            ASSERT(move);
            return _table[piece_type][move.to_square()];
        }

        inline const T& lookup(chess::PieceType piece_type, const Move& move) const
        {
            return const_cast<PieceMoveTable*>(this)->lookup(piece_type, move);
        }

        T _table[7][64] = {};
    };

    using KillerMoves = std::array<Move, 2>;
    using KillerMovesTable = std::array<KillerMoves, PLY_MAX>;


    struct TT_Entry
    {
        uint64_t    _hash = 0;
        uint16_t    _age = 0;
        int16_t     _depth = std::numeric_limits<int16_t>::min();
        int32_t     _capt = SCORE_MIN;
        score_t     _alpha = SCORE_MIN;
        score_t     _beta = SCORE_MAX;
        score_t     _value = SCORE_MIN;
        score_t     _eval = SCORE_MIN;
        score_t     _king_safety = SCORE_MIN;
        score_t     _threats = SCORE_MIN;
        BaseMove    _hash_move;
        bool        _singleton = false;
        uint32_t    _version = 0;
        void*       _lock = nullptr;

        explicit TT_Entry(void* lock = nullptr) : _lock(lock) {}

        inline bool is_lower() const { return _value >= _beta; }
        inline bool is_upper() const { return _value <= _alpha; }
        inline bool is_valid() const { return _version != 0; }

        inline bool matches(const State& state) const
        {
            return is_valid() && _hash == state.hash();
        }

        const score_t* lookup_score(Context&) const;
    };


    /*
     * Hash table, counter moves, historical counts.
     * Under SMP the hash table is shared between threads.
     */
    class TranspositionTable
    {
        using HashTable = SharedHashTable<TT_Entry>;
        using HashTablePtr = std::shared_ptr<HashTable>;

    #if USE_BUTTERFLY_TABLES
        using HistoryCounters = MoveTable<std::pair<int, int>>;
        using IndexedMoves = MoveTable<BaseMove>;
    #else
        using HistoryCounters = PieceMoveTable<std::pair<int, int>>;
        using IndexedMoves = PieceMoveTable<BaseMove>;
    #endif /* USE_BUTTERFLY_TABLES */

        using PlyHistoryCounters = std::array<MoveTable<float>, 2>;
        using PlyHistory = std::array<PlyHistoryCounters, PLY_HISTORY_MAX>;

        /* https://www.chessprogramming.org/Countermove_Heuristic */
        IndexedMoves        _countermoves[2];

        KillerMovesTable    _killer_moves; /* killer moves at each ply */
        HistoryCounters     _hcounters[2]; /* History heuristic counters. */
        static HashTablePtr _table;        /* shared hashtable */

    public:
        TranspositionTable() = default;
        ~TranspositionTable() = default;

               void clear();
        static void clear_shared_hashtable();
        static void increment_clock();

        int _tid = 0;
        int _iteration = 0;
        int _eval_depth = 0;
        BaseMovesList _pv; /* principal variation */
        PlyHistory _plyHistory;

        MovesList _initial_moves;

        /* search window bounds */
        score_t _w_alpha = SCORE_MIN;
        score_t _w_beta = SCORE_MAX;
        bool _reset_window = false;

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

        Move lookup_countermove(const Context&) const;

        const KillerMoves* get_killer_moves(int ply) const
        {
            ASSERT(ply < PLY_MAX);
            return &_killer_moves[ply];
        }

        const BaseMovesList& get_pv() const { return _pv; }

        template<bool Debug=false>
        void get_pv_from_table(Context&, const Context&, BaseMovesList&);

        const score_t* lookup(Context&);

        void store(Context&, score_t alpha, int depth);
        void store(Context&, TT_Entry&, score_t, int depth);

        void store_countermove(Context&);
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
    inline const std::pair<int, int>&
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


    inline float
    TranspositionTable::history_score(
        int ply,
        const State& state,
        Color turn,
        const Move& move) const
    {
        const auto& counters = historical_counters(state, turn, move);
        ASSERT(counters.first <= counters.second);

        return (ply < PLY_HISTORY_MAX ? _plyHistory[ply][turn].lookup(move) : 0)
            + (counters.second < 1 ? 0 : (100.0 * counters.first) / counters.second);
    }


    inline void TranspositionTable::history_update_non_cutoffs(const Move& move, bool low)
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


    inline void TranspositionTable::history_update_cutoffs(const Move& move)
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
}
