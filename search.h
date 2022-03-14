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

#include <limits>
#include <memory>
#include <thread>
#include "chess.h"

constexpr int ONE_PLY = 16; /* for fractional extensions */
constexpr int PLY_MAX = 100;

constexpr score_t SCORE_MIN = -100000;
constexpr score_t SCORE_MAX =  100000;

constexpr score_t CHECKMATE = SCORE_MAX - 1;
constexpr score_t MATE_HIGH = SCORE_MAX - PLY_MAX;
constexpr score_t MATE_LOW  = -MATE_HIGH;

/* Aspiration window */
constexpr score_t HALF_WINDOW = chess::WEIGHT[chess::PAWN] / 4;


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
    using BaseMovesList = std::vector<BaseMove>;
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
    using KillerMovesTable = std::vector<KillerMoves>;


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
        void print(std::ostream&) const;
    };


    /*
     * Hash table, counter moves, historical counts.
     * Under SMP the hash table is shared between threads.
     */
    class TranspositionTable
    {
        using TablePtr = std::shared_ptr<class SharedHashTable>;

    #if USE_BUTTERFLY_TABLES
        using HistoryCounters = MoveTable<std::pair<int, int>>;
        using IndexedMoves = MoveTable<Move>;
    #else
        using HistoryCounters = PieceMoveTable<std::pair<int, int>>;
        using IndexedMoves = PieceMoveTable<Move>;
    #endif /* USE_BUTTERFLY_TABLES */

        /* https://www.chessprogramming.org/Countermove_Heuristic */
        IndexedMoves        _countermoves[2];

        KillerMovesTable    _killer_moves; /* killer moves at each ply */
        HistoryCounters     _hcounters[2]; /* History heuristic counters. */

    public:
        TranspositionTable() = default;
        ~TranspositionTable() = default;

        static void clear_shared_hashtable();
        static void increment_clock();

        int _iteration = 0;
        int _eval_depth = 0;
        BaseMovesList _pv; /* principal variation */

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
            return size_t(ply) < _killer_moves.size() ? &_killer_moves[ply] : nullptr;
        }

        const BaseMovesList& get_pv() const { return _pv; }
        void get_pv_from_table(Context&, const Context&, BaseMovesList&, bool);

        const score_t* lookup(Context&);

        void store(Context&, score_t alpha);
        void store(Context&, TT_Entry&, score_t);

        void store_countermove(const Context&);
        void store_killer_move(const Context&);

        void store_pv(Context&, bool = false);

        const std::pair<int, int>& historical_counters(const Context&, const Move&) const;
        float history_score(const Context&, const Move&) const;

        size_t hits() const { return _hits; }
        size_t nodes() const { return _nodes; }

        /* number of occupied slots in the shared hashtable */
        static size_t size();

        /* percent usage (size over capacity) */
        static double usage();

        /* nodes per second */
        size_t nps() const { return _nps; }
        void set_nps(size_t nps) { _nps = nps; }

        void history_update_cutoffs(const Move&);
        void history_update_non_cutoffs(const Move&);

        void update_stats(const Context&);

        static size_t max_hash_size();

        /* return size of hash table */
        static size_t get_hash_size();

        /* set hash table size in MB */
        static void set_hash_size(size_t);

    private:
        static TablePtr _table; /* shared hashtable */
    };

}
