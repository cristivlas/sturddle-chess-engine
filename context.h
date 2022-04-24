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

#include <atomic>
#include <chrono>
#include <map>
#include <mutex>
#include <string>
#include <unordered_set> /* unordered_multiset */
#include "Python.h"
#include "config.h"
#include "intrusive.h"
#include "search.h"
#include "utility.h"

/* Configuration API */
struct Param { int val = 0; int min_val; int max_val; };

extern std::map<std::string, Param> _get_param_info();
extern void _set_param(const std::string&, int value, bool echo=false);
extern std::map<std::string, int> _get_params();


namespace search
{
    using time = std::chrono::time_point<std::chrono::system_clock>;

    using Bitboard = chess::Bitboard;
    using Color = chess::Color;
    using Square = chess::Square;

    using ContextPtr = intrusive_ptr<struct Context>;
    using HistoryPtr = intrusive_ptr<struct History>;


    enum Algorithm : int
    {
        NEGAMAX,
        NEGASCOUT,
        MTDF,
    };


    /* For detecting repeated positions */
    struct History : public RefCounted<History>
    {
        History() = default;

        void insert(const State& s) { _positions.insert(s); }
        size_t count(const State& s) const { return _positions.count(s); }

        std::unordered_multiset<State, Hasher<State>> _positions;
        int _fifty = 0;
    };


    /*
     * Helper for making and ordering moves
     */
    class MoveMaker
    {
    public:
        static constexpr size_t MAX_MOVE = 2 * PLY_MAX;

        /* Note: top-half of the _moves buffers is reserved for do_exchanges */
        static THREAD_LOCAL MovesList _moves[MAX_MOVE];

        MoveMaker() = default;

        const Move* get_next_move(Context& ctxt, score_t futility = 0);

        int current(Context&);

        bool has_moves(Context&);
        bool have_skipped_moves() { return _have_pruned_moves || _have_quiet_moves; }
        bool is_last(Context&);
        bool is_singleton(Context&);

        void set_ply(int ply) { _ply = ply; }

        int rewind(Context&, int where, bool reorder);

        /* SMP: copy the root moves from the main thread to the other workers. */
        void set_initial_moves(const MovesList& moves);

        const MovesList& get_moves() const
        {
            ASSERT(_count >= 0);
            return moves();
        }

    private:
        void ensure_moves(Context&);

        void generate_unordered_moves(Context&);
        const Move* get_move_at(Context& ctxt, int index, score_t futility = 0);

        void make_capture(Context&, Move&);
        bool make_move(Context&, Move&, score_t futility = 0);
        bool make_move(Context&, Move&, MoveOrder, score_t = 0);
        void mark_as_illegal(Move&);
        void order_moves(Context&, size_t start_at, score_t futility);
        void sort_moves(Context&, size_t start_at);

        inline MovesList& moves() const
        {
            ASSERT(size_t(_ply) < MAX_MOVE);
            return _moves[_ply];
        }

        inline std::vector<State>& states() const
        {
            ASSERT(size_t(_ply) < MAX_MOVE);
            return _states[_ply];
        }

        MoveMaker(const MoveMaker&) = delete;
        MoveMaker& operator=(const MoveMaker&) = delete;

        int         _ply = 0;
        int         _count = -1;
        int         _current = -1;
        int         _phase = 0; /* move ordering phase */
        bool        _group_quiet_moves = false;
        bool        _have_move = false;
        bool        _have_quiet_moves = false;
        bool        _have_pruned_moves = false;
        bool        _need_sort = false;
        size_t      _state_index = 0;
        MovesList   _initial_moves;

        static THREAD_LOCAL std::vector<State> _states[MAX_MOVE];
    };


    inline void MoveMaker::mark_as_illegal(Move& move)
    {
        move._group = MoveOrder::ILLEGAL_MOVES;

        ASSERT(_count > 0);
        --_count;
    }


    enum class LMRAction : int
    {
        None = 0,
        Ok,
        Prune,
    };

    /* Reason for retrying */
    enum class RETRY : uint8_t
    {
        None = 0,
        Reduced,
        PVS,
    };


    /*
     * The context of a searched node.
     */
    struct Context : public RefCounted<Context>
    {
        friend class RefCounted<Context>;

    private:
        ~Context();

    public:
        Context() = default;
        Context(const Context&) = delete;
        Context& operator=(const Context&) = delete;

        static void* operator new(size_t);
        static void* operator new(size_t, void*);
        static void operator delete(void*, size_t) noexcept;

        ContextPtr clone(int ply = 0) const;

        /* "parent" move in the graph; is "opponent" a better name? */
        Context*    _parent = nullptr;
        int         _tid = 0;
        int         _ply = 0;
        int         _max_depth = 0;

        Algorithm   _algorithm = Algorithm::NEGAMAX;

        score_t     _alpha = SCORE_MIN;
        score_t     _beta = SCORE_MAX;
        score_t     _score = SCORE_MIN; /* dynamic eval score */
        score_t     _retry_beta = SCORE_MAX; /* NEGASCOUT only */

        bool        _futility_pruning = true;
        bool        _is_null_move = false; /* for null-move pruning */
        bool        _is_pv = false;
        bool        _is_retry = false;
        bool        _is_singleton = false;
        bool        _is_terminal = false; /* debug only */
        bool        _multicut_allowed = true;
        bool        _null_move_allowed[2] = { true, true };
        RETRY       _retry_above_alpha = RETRY::None;
        bool        _retry_next = false;
        int         _double_ext = 0;
        int         _extension = 0; /* count pending fractional extensions */
        int         _fifty = 0;
        int         _full_depth_count = late_move_reduction_count();
        int         _mate_detected = 0;
        int         _pruned_count = 0;
        int         _prune_reason = 0; /* debug */

        HistoryPtr  _history;

        Move        _cutoff_move;   /* from current state to the next */
        Move        _move;          /* from parent to current state */
        BaseMove    _prev;          /* best move from previous iteration */
        BaseMove    _excluded;      /* singular extension search */

        State*      _state = nullptr;
        TT_Entry    _tt_entry;
        Square      _capture_square = Square::UNDEFINED;

        const ContextPtr& best() const { return _best; }
        void set_best(const ContextPtr& best) { best->_parent = this; _best = best; }

        static void cancel();
        static int  cpu_cores();

        bool        can_forward_prune() const;
        bool        can_prune(int) const;
        bool        can_prune_move(const Move&i, int) const;
        bool        can_reduce();

        int64_t     check_time_and_update_nps(); /* return elapsed milliseconds */
        void        copy_move_state();

        int         depth() const { return _max_depth - _ply; }

        static int  double_ext_margin();
        std::string epd() const;

        /* Static evaluation */
        score_t     _evaluate();    /* no repetitions, no fifty-moves rule */
        score_t     evaluate();     /* call _evaluate and do the above */
        score_t     evaluate_end();
        score_t     evaluate_material(bool with_piece_squares = true) const;
        int         eval_king_safety();

        void        extend();       /* fractional extensions */
        ContextPtr  first_move();
        score_t     futility_margin();

        bool        has_improved(score_t margin = 0) { return improvement() > margin; }
        bool        has_moves() { return _move_maker.has_moves(*this); }

    #if 0
        bool has_passed_pawns() const
        {
            return state().passed_pawns(WHITE, ~(BB_RANK_4 | BB_RANK_5)) != 0
                || state().passed_pawns(BLACK, ~(BB_RANK_4 | BB_RANK_5)) != 0;
        }
    #endif

        score_t     improvement();
        static void init();

        bool        is_beta_cutoff(const ContextPtr&, score_t);
        static bool is_cancelled() { return _cancel; }
        bool        is_capture() const { return state().capture_value != 0; }
        bool        is_check() const { return state().is_check(); }
        bool        is_counter_move(const Move&) const;
        bool        is_evasion() const;
        bool        is_extended() const;
        bool        is_king_safe() const;
        bool        is_last_move();
        bool        is_leftmost() const { return _ply == 0 || _leftmost; }
        bool        is_leaf(); /* treat as terminal node ? */
        bool        is_qsearch() const { return _ply > _max_depth; }
        bool        is_mate_bound() const;
        bool        is_null_move_ok(); /* ok to generate null move? */
        bool        is_null_move() const { return _is_null_move; }
        bool        is_promotion() const { return state().promotion; }
        bool        is_pv_node() const { return _is_pv; }
        bool        is_recapture() const;
        bool        is_reduced() const;
        bool        is_pvs_ok() const;
        int         is_repeated() const;
        bool        is_retry() const { return _is_retry; }
        bool        is_singleton() const { return _is_singleton; }
        int         iteration() const { ASSERT(_tt); return _tt->_iteration; }

        LMRAction   late_move_reduce(bool prune, int move_count);
        static int  late_move_reduction_count();

        static void log_message(LogLevel, const std::string&, bool force = true);

        int64_t     nanosleep(int nanosec);
        ContextPtr  next(bool null_move = false, bool = false, score_t = 0);
        int         next_move_index() { return _move_maker.current(*this); }
        bool        on_next();

        void        reinitialize();
        int         rewind(int where = 0, bool reorder = false);

        void        set_counter_move(const Move& move) { _counter_move = move; }
        void        set_search_window(score_t, bool reset = false);

        static void set_time_limit_ms(int milliseconds);
        void        set_time_info(int time_left /* millisec */, int moves_left);
        void        set_tt(TranspositionTable* tt) { _tt = tt; }

        bool        should_verify_null_move() const;
        int         singular_margin() const;

        Color       turn() const { return state().turn; }

        const State& state() const { ASSERT(_state); return *_state; }
        TranspositionTable* get_tt() const { return _tt; }

        const MovesList& get_moves() const
        {
            return _move_maker.get_moves();
        }

        void set_initial_moves(const MovesList& moves)
        {
            _move_maker.set_initial_moves(moves);
        }

        /* retrieve PV from TT */
        const BaseMovesList& get_pv() const { return get_tt()->get_pv(); }

        /*
         * Python callbacks
         */
        static PyObject*    _engine; /* searcher instance */

        static std::string  (*_epd)(const State&);
        static void         (*_log_message)(int, const std::string&, bool);
        static void         (*_on_iter)(PyObject*, ContextPtr, score_t);
        static void         (*_on_next)(PyObject*, int64_t);
        static std::string  (*_pgn)(ContextPtr);
        static void         (*_print_state)(const State&);
        static void         (*_report)(PyObject*, std::vector<ContextPtr>&);
        static size_t       (*_vmem_avail)();

    private:
        const Move* get_next_move(score_t);
        bool has_cycle(const State&) const;

        float history_score(const Move&) const;
        int repeated_count(const State&) const;

        ContextPtr  _best; /* best search result */
        State       _statebuf;
        bool        _leftmost = false;
        mutable int _repetitions = -1;
        Move        _counter_move;
        friend class MoveMaker;
        MoveMaker   _move_maker;

        static std::atomic_bool _cancel;
        static std::mutex _mutex; /* update time limit from another thread */

        static asize_t  _callback_count;
        static int      _time_limit; /* milliseconds */
        static time     _time_start;

        TranspositionTable* _tt = nullptr;
    };


    inline bool Context::can_forward_prune() const
    {
        return _parent
            && (_parent->_mate_detected == 0 || _parent->_mate_detected % 2)
            && !is_pv_node()
            && (_max_depth >= 6 || !is_qsearch())
            && !_excluded
            && state().pushed_pawns_score <= 1
            && !state().just_king_and_pawns()
            && (depth() >= 6 || is_king_safe())
            && !is_check();
    }


    inline bool Context::can_prune(int count) const
    {
        ASSERT(_ply > 0);
        ASSERT(!is_null_move());
        ASSERT(_move);

        if (is_singleton() || is_extended() || is_pv_node() || is_repeated())
            return false;

        return _parent->can_prune_move(_move, count);
    }


    inline bool Context::can_prune_move(const Move& move, int count) const
    {
        ASSERT(move && move._state && move != _move);
        ASSERT(repeated_count(*move._state) == 0);
        ASSERT(can_forward_prune()); /* pre-req */

        if (move.promotion() == chess::PieceType::QUEEN
            || (move._state->capture_value != 0)
            || (move._group <= MoveOrder::HISTORY_COUNTERS)
            || (move.from_square() == _capture_square)
            || is_counter_move(move)
            || (move == _tt_entry._hash_move)
            || (move._state->is_check())
            || (move._group == MoveOrder::TACTICAL_MOVES && count < 13)
           )
            return false;

        ASSERT(!is_check());

        return true;
    }


    inline bool Context::can_reduce()
    {
        ASSERT(!is_null_move());

        return _ply != 0
            && !is_retry()
        #if 0
            && !is_singleton()
        #endif
            && !is_extended()
            && _move._group >= MoveOrder::WINNING_CAPTURES
            && state().pushed_pawns_score <= 1
            && _move.from_square() != _parent->_capture_square
            && !state().is_check();
    }


    inline int Context::double_ext_margin()
    {
        return DOUBLE_EXT_MARGIN;
    }


    inline float Context::history_score(const Move& move) const
    {
        ASSERT(_tt);
        ASSERT(move);
        ASSERT(move != _move);

        return _tt->history_score(state(), turn(), move)
            + COUNTER_MOVE_BONUS * is_counter_move(move);
    }


    inline bool Context::is_counter_move(const Move& move) const
    {
        return _counter_move == move && depth() >= COUNTER_MOVE_MIN_DEPTH;
    }


    inline bool Context::is_extended() const
    {
        ASSERT(_parent);
        return _max_depth > _parent->_max_depth;
    }


    inline bool Context::is_king_safe() const
    {
        return _tt_entry._king_safety == SCORE_MIN
            || SIGN[!state().turn] * _tt_entry._king_safety >= KING_SAFETY_MARGIN;
    }


    inline bool Context::is_mate_bound() const
    {
        return _tt_entry._value >= MATE_HIGH && _tt_entry._depth >= depth();
    }


    inline bool Context::is_recapture() const
    {
        ASSERT(_parent);
        return _parent->is_capture() && (_move.to_square() == _parent->_move.to_square());
    }


    inline bool Context::is_reduced() const
    {
        ASSERT(_parent);
        return _max_depth < _parent->_max_depth;
    }


    inline bool Context::is_pvs_ok() const
    {
        return (_algorithm == NEGASCOUT) && !is_retry() && !is_leftmost();
    }


    /*
     * Check how much time is left, update NPS, call optional Python callback.
     */
    inline bool Context::on_next()
    {
        if (_tid == 0 && ++_callback_count >= CALLBACK_PERIOD)
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


    inline bool Context::should_verify_null_move() const
    {
        return depth() >= NULL_MOVE_MIN_VERIFICATION_DEPTH;
    }


    inline int Context::singular_margin() const
    {
        return depth() * SINGULAR_MARGIN;
    }

} /* namespace */
