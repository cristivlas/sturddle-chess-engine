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


constexpr auto FIRST_EXCHANGE_PLY = PLY_MAX;


/* Configuration API */
struct Param { int val = 0; int min_val; int max_val; std::string group; };

extern std::map<std::string, Param> _get_param_info();
extern void _set_param(const std::string&, int value, bool echo=false);
extern std::map<std::string, int> _get_params();


namespace search
{
    using time = std::chrono::time_point<std::chrono::steady_clock>;

    using Bitboard = chess::Bitboard;
    using Color = chess::Color;
    using Square = chess::Square;

    using ContextPtr = intrusive_ptr<struct Context>;
    using HistoryPtr = intrusive_ptr<struct History>;

    using atomic_bool = std::atomic<bool>;
    using atomic_int = std::atomic<int>;
    using atomic_time = std::atomic<time>; /* sic */


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

        INLINE MovesList& moves() const
        {
            ASSERT(size_t(_ply) < MAX_MOVE);
            return _moves[_ply];
        }

        INLINE std::vector<State>& states() const
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


    struct Free
    {
        Free* _next = nullptr;
    };


    /*
     * The context of a searched node.
     */
    struct Context : public Free, public RefCounted<Context>
    {
        friend class MoveMaker;
        friend class RefCounted<Context>;

    private:
        ~Context() = default;

    public:
        Context() = default;
        Context(const Context&) = delete;
        Context& operator=(const Context&) = delete;

    #if RECYCLE_CONTEXTS
        static void* operator new(size_t);
        static void operator delete(void*, size_t) noexcept;
    #endif /* RECYCLE_CONTEXTS */

        ContextPtr clone(int ply = 0) const;

        /* parent move in the graph */
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

        HistoryPtr  _history;

        Move        _cutoff_move;   /* from current state to the next */
        Move        _move;          /* from parent to current state */
        BaseMove    _prev;          /* best move from previous iteration */
        BaseMove    _excluded;      /* singular extension search */

        State*      _state = nullptr;
        TT_Entry    _tt_entry;
        Square      _capture_square = Square::UNDEFINED;

        const ContextPtr& best() const { return _best; }

        void set_best(const ContextPtr& best)
        {
            best->_parent = this;
            _best = best;
        }

        static void cancel();
        static int  cpu_cores();

        bool        can_forward_prune() const;
        bool        can_prune() const;
        bool        can_prune_move(const Move&) const;
        bool        can_reduce() const;

        int64_t     check_time_and_update_nps(); /* return elapsed milliseconds */
        void        copy_move_state();

        int         depth() const { return _max_depth - _ply; }

        std::string epd() const;

        /* Static evaluation */
        score_t     _evaluate();    /* no repetitions, no fifty-moves rule */
        score_t     evaluate();     /* call _evaluate and do the above */
        score_t     evaluate_end();
        score_t     evaluate_material(bool with_piece_squares = true) const;
        int         eval_king_safety(int piece_count);
        int         eval_threats(int piece_count);

        void        extend();       /* fractional extensions */
        ContextPtr  first_move();
        score_t     futility_margin();

        bool        has_improved(score_t margin = 0) { return improvement() > margin; }
        bool        has_moves() { return _move_maker.has_moves(*this); }

        int         history_count(const Move&) const;
        float       history_score(const Move&) const;

        score_t     improvement();
        static void init();

        bool        is_beta_cutoff(const ContextPtr&, score_t);
        static bool is_cancelled() { return _cancel; }
        bool        is_capture() const { return state().capture_value != 0; }
        bool        is_check() const { return state().is_check(); }
        bool        is_counter_move(const Move&) const;
        bool        is_evasion() const;
        bool        is_extended() const;
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

        LMRAction   late_move_reduce(int move_count);
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

        int repeated_count(const State&) const;

        ContextPtr          _best; /* best search result */
        mutable int         _can_prune = -1;
        State               _statebuf;
        bool                _leftmost = false;
        mutable int         _repetitions = -1;
        Move                _counter_move;
        MoveMaker           _move_maker;

        TranspositionTable* _tt = nullptr;

        /* search can be cancelled from any thread */
        static atomic_bool  _cancel;

        static asize_t      _callback_count;
        static atomic_int   _time_limit; /* milliseconds */
        static atomic_time  _time_start;
    };


    /*
    * Late-move pruning counts (initialization idea borrowed from Crafty)
    */
    template<std::size_t... I>
    constexpr std::array<int, sizeof ... (I)> lmp(std::index_sequence<I...>)
    {
        return { static_cast<int>(LMP_BASE + pow(I + .5, 1.9)) ... };
    }


    template<bool Debug = false>
    int do_exchanges(const State&, Bitboard, score_t = 0, int = FIRST_EXCHANGE_PLY);

    /*
     * Evaluate same square exchanges
     */
    template<bool StaticExchangeEvaluation>
    score_t eval_exchanges(const Move& move)
    {
        score_t val = 0;

        if (move)
        {
            ASSERT(move._state);
            ASSERT(move._state->piece_type_at(move.to_square()));

            if constexpr(StaticExchangeEvaluation)
            {
                /*
                 * Approximate without playing the moves.
                 */
                val = estimate_static_exchanges(*move._state, move._state->turn, move.to_square());
            }
            else
            {
                auto mask = chess::BB_SQUARES[move.to_square()];
                val = do_exchanges<DEBUG_CAPTURES != 0>(*move._state, mask);
            }
        }

        return val;
    }



    /* Evaluate material plus piece-squares from the point of view
     * of the side that just moved. This is different from the other
     * evaluate methods which apply the perspective of the side-to-move.
     * Side-effect: caches simple score inside the State object.
     */
    INLINE int eval_material_and_piece_squares(State& state)
    {
        if (state.simple_score == 0)
            state.simple_score = state.eval_simple();

        return state.simple_score * SIGN[!state.turn];
    }


    INLINE void incremental_update(Move& move, const Context& ctxt)
    {
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
    }


    INLINE bool is_direct_check(const Move& move)
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


    INLINE bool is_quiet(const State& state, const Context* ctxt = nullptr)
    {
    #if 0
        return state.promotion != chess::PieceType::QUEEN /* ignore under-promotions */
    #else
        return state.promotion == chess::PieceType::NONE
    #endif
            && state.capture_value == 0
            && state.pushed_pawns_score <= 1
            && (!ctxt || !ctxt->is_evasion())
            && !state.is_check();
    }


    /*
     * Standing pat: other side made a capture, after which side-to-move's
     * material evaluation still beats beta, or can't possibly beat alpha?
     */
    INLINE bool is_standing_pat(const Context& ctxt)
    {
        if (ctxt.is_capture())
        {
            const auto score = ctxt.evaluate_material();
            return score >= ctxt._beta || score + chess::WEIGHT[chess::QUEEN] < ctxt._alpha;
        }

        return false;
    }


    INLINE bool Context::can_forward_prune() const
    {
        if (_can_prune == -1)
        {
            _can_prune =
                (_parent != nullptr)
                && !is_pv_node()
                && (_max_depth >= 6 || !is_qsearch())
                && !_excluded
                && (state().pushed_pawns_score <= 1)
                && !state().just_king_and_pawns()
                && (_parent->_mate_detected == 0 || _parent->_mate_detected % 2)
                && !is_check();
        }
        return _can_prune > 0;
    }


    INLINE bool Context::can_prune() const
    {
        ASSERT(_ply > 0);
        ASSERT(!is_null_move());
        ASSERT(_move);

        return !is_singleton()
            && !is_extended()
            && !is_pv_node()
            && !is_repeated()
            && _parent->can_prune_move(_move);
    }


    INLINE bool Context::can_prune_move(const Move& move) const
    {
        ASSERT(move && move._state && move != _move);

        return (move != _tt_entry._hash_move)
            && (move._state->capture_value == 0)
            && (move.promotion() == chess::PieceType::NONE)
            && (move.from_square() != _capture_square)
            && !is_counter_move(move)
            && can_forward_prune()
            && !move._state->is_check();
    }


    INLINE bool Context::can_reduce() const
    {
        ASSERT(!is_null_move());

        return (_ply != 0)
            && !is_retry()
            && !is_singleton()
            && (state().pushed_pawns_score <= 1)
            && !is_extended()
            && (_move.from_square() != _parent->_capture_square)
            && !is_recapture()
            && !state().is_check();
    }


    INLINE score_t Context::evaluate_material(bool with_piece_squares) const
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


    static constexpr double PHI = 1.61803398875;

    template<std::size_t... I>
    constexpr std::array<int, sizeof ... (I)> margins(std::index_sequence<I...>)
    {
        return { static_cast<int>(50 * I + pow(I + PHI, M_E)) ... };
    }


    INLINE score_t Context::futility_margin()
    {
        if (_ply == 0 || !_futility_pruning || depth() < 1)
            return 0;

        /*
         * No need to check for futile moves when material is above alpha,
         * since no move can immediately result in decrease of material
         * (margin of one PAWN for piece-square evaluation).
         */
        if (evaluate_material() > std::max(_alpha, _score) + chess::WEIGHT[chess::PAWN])
            return 0;

        static const auto fp_margins = margins(std::make_index_sequence<PLY_MAX>{});

        return fp_margins[depth()] * can_forward_prune();
    }


    INLINE const Move* Context::get_next_move(score_t futility)
    {
        auto move = _move_maker.get_next_move(*this, futility);

        if (move && *move == _excluded)
        {
            move = _move_maker.get_next_move(*this, futility);
        }

        return move;
    }


    INLINE int Context::history_count(const Move& move) const
    {
        return _tt->historical_counters(state(), turn(), move).first;
    }


    INLINE float Context::history_score(const Move& move) const
    {
        ASSERT(_tt);
        ASSERT(move);
        ASSERT(move != _move);

        return COUNTER_MOVE_BONUS * is_counter_move(move)
            + _tt->history_score(_ply, state(), turn(), move);
    }


    INLINE bool Context::is_counter_move(const Move& move) const
    {
        return _counter_move == move;
    }


    INLINE bool Context::is_extended() const
    {
        ASSERT(_parent);
        return _max_depth > _parent->_max_depth;
    }


    INLINE bool Context::is_mate_bound() const
    {
        return _tt_entry._value >= MATE_HIGH && _tt_entry._depth >= depth();
    }


    INLINE bool Context::is_recapture() const
    {
        ASSERT(_parent);
        return _parent->is_capture() && (_move.to_square() == _parent->_move.to_square());
    }


    INLINE bool Context::is_reduced() const
    {
        ASSERT(_parent);
        return _max_depth < _parent->_max_depth;
    }


    INLINE bool Context::is_pvs_ok() const
    {
        return (_algorithm == NEGASCOUT) && !is_retry() && !is_leftmost();
    }


    /* static */ INLINE int Context::late_move_reduction_count()
    {
        return LATE_MOVE_REDUCTION_COUNT;
    }


    /*
     * Reduction formula based on ideas from SF and others.
     */
    INLINE int null_move_reduction(Context& ctxt)
    {
        return NULL_MOVE_REDUCTION
            + ctxt.depth() / NULL_MOVE_DEPTH_DIV
            + std::min(3, (ctxt.evaluate_material() - ctxt._beta) / NULL_MOVE_DIV);
    }


    /*
     * Get the next move and wrap it into a Context object.
     */
    INLINE ContextPtr Context::next(bool null_move, bool first_move, score_t futility)
    {
        ASSERT(first_move || _alpha < _beta);

        const bool retry = _retry_next;
        _retry_next = false;

        if (!first_move && !_excluded && !on_next() && !is_check())
            return ContextPtr();

        /* null move must be tried before actual moves */
        ASSERT(!null_move || next_move_index() == 0);

        const auto move = null_move ? nullptr : get_next_move(futility);
        if (!move && !null_move)
            return ContextPtr();

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
        ctxt->_double_ext = _double_ext;
        ctxt->_extension = _extension;
        ctxt->_move_maker.set_ply(ctxt->_ply);
        ctxt->_is_retry = retry;
        ctxt->_is_singleton = !ctxt->is_null_move() && _move_maker.is_singleton(*this);
        ctxt->_history = _history;
        ctxt->_futility_pruning = _futility_pruning && FUTILITY_PRUNING;
        ctxt->_multicut_allowed = _multicut_allowed && MULTICUT;

        for (auto side : { chess::BLACK, chess::WHITE })
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
                else if (state().pawns & chess::BB_SQUARES[move->from_square()])
                    ctxt->_fifty = 0;
                else
                    ctxt->_fifty = (_ply == 0 ? _history->_fifty : _fifty) + 1;
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
     * Check how much time is left, update NPS, call optional Python callback.
     */
    INLINE bool Context::on_next()
    {
        if (_tid == 0 && ++_callback_count >= CALLBACK_PERIOD)
        {
            _callback_count = 0; /* reset */

            const auto millisec = check_time_and_update_nps();

            if (millisec < 0) /* time is up? */
                return false;

            if (_on_next)
                cython_wrapper::call(_on_next, _engine, millisec);
        }

        return !is_cancelled(); /* check again in case _on_next called cancel() */
    }


    INLINE int Context::rewind(int where, bool reorder)
    {
        return _move_maker.rewind(*this, where, reorder);
    }


    INLINE bool Context::should_verify_null_move() const
    {
        return depth() >= NULL_MOVE_MIN_VERIFICATION_DEPTH;
    }


    INLINE int Context::singular_margin() const
    {
        return SINGULAR_MARGIN * depth();
    }


    INLINE int MoveMaker::current(Context& ctxt)
    {
        ensure_moves(ctxt);
        return _current;
    }


    INLINE void MoveMaker::ensure_moves(Context& ctxt)
    {
        ASSERT(!moves().empty() || _count <= 0);

        if (_count < 0)
        {
            ASSERT(_current < 0);
            ASSERT(_phase == 0);

            generate_unordered_moves(ctxt);

            ASSERT(_count >= 0);
            ASSERT(_current >= 0);
        }
    }


    INLINE const Move* MoveMaker::get_next_move(Context& ctxt, score_t futility)
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


    INLINE const Move* MoveMaker::get_move_at(Context& ctxt, int index, score_t futility)
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


    INLINE bool MoveMaker::has_moves(Context& ctxt)
    {
        ensure_moves(ctxt);
        ASSERT(_count >= 0);

        return (_count > 0) && get_move_at(ctxt, 0);
    }


    INLINE bool MoveMaker::is_last(Context& ctxt)
    {
        ensure_moves(ctxt);
        ASSERT(_count >= 0);

        return _current >= _count || !get_move_at(ctxt, _current);
    }


    /*
     * Is the current move the only available move?
     */
    INLINE bool MoveMaker::is_singleton(Context& ctxt)
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


    INLINE void MoveMaker::make_capture(Context& ctxt, Move& move)
    {
        if (make_move(ctxt, move) && move._score == 0)
        {
            ASSERT(move._state->capture_value);

            /* Use part of the static eval as the sorting score. */
            move._score = eval_material_and_piece_squares(*move._state);

            /*
             * Now determine which capture group it belongs to.
             */
            auto capture_gain = move._state->capture_value;

            auto other = ctxt.state().weight(ctxt.state().piece_type_at(move.from_square()));
            if (other >= capture_gain)
            {
                other = eval_exchanges<true>(move);

                if (EXCHANGES_DETECT_CHECKMATE && other < MATE_LOW)
                {
                    move._score = -other;
                    move._group = MoveOrder::WINNING_CAPTURES;

                    if (DEBUG_CAPTURES)
                        ctxt.log_message(
                            LogLevel::DEBUG,
                            move.uci() + ": " + std::to_string(move._score) + " " + ctxt.epd());
                    return;
                }
            }
            capture_gain -= other;

            if (capture_gain < 0)
            {
                move._group = MoveOrder::LOSING_CAPTURES;
            }
            else
            {
                move._group = MoveOrder::LAST_MOVED_CAPTURE
                    + (move.to_square() != ctxt._move.to_square()) * (1 + (capture_gain == 0));
            }
        }
    }


    /*
     * Return false if the move is not legal, or pruned.
     */
    INLINE bool MoveMaker::make_move(Context& ctxt, Move& move, score_t futility)
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
        {
            return (_have_move = true);
        }

        ctxt.state().clone_into(*move._state);

        ASSERT(move._state->capture_value == 0);

        /* capturing the king is an illegal move (Louis XV?) */
        if (move._state->kings & chess::BB_SQUARES[move.to_square()])
        {
            mark_as_illegal(move);
            return false;
        }

        static const auto LMP = lmp(std::make_index_sequence<PLY_MAX>{});

        /*
         * Prune (before verifying move legality, thus saving is_check() calls).
         * Late-move prune before making the move.
         */
        if (_phase > 2
            && ctxt.depth() > 0
            && _current >= LMP[ctxt.depth()]
            && ctxt.can_forward_prune())
        {
            _have_pruned_moves = true;
            ++ctxt._pruned_count;
            move._group = MoveOrder::PRUNED_MOVES;
            return false;
        }

        move._state->apply_move(move);

        if (_group_quiet_moves && is_quiet(*move._state))
        {
            _have_quiet_moves = true;
            move._group = MoveOrder::QUIET_MOVES;
            return false;
        }

        incremental_update(move, ctxt);

        /* Futility-prune after making the move (state is needed for simple eval). */
        if (futility > 0)
        {
            const auto val = futility + move._state->simple_score * SIGN[!move._state->turn];

            if ((val < ctxt._alpha || val < ctxt._score) && ctxt.can_prune_move(move))
            {
                _have_pruned_moves = true;
                ++ctxt._pruned_count;
                move._group = MoveOrder::PRUNED_MOVES;

            #if EXTRA_STATS

                ++ctxt.get_tt()->_futility_prune_count;

            #endif /* EXTRA_STATS */

                return false;
            }
        }

        if (move._state->is_check(ctxt.turn()))
        {
            mark_as_illegal(move); /* can't leave the king in check */
            return false;
        }

        if (_group_quiet_moves && is_standing_pat(ctxt))
        {
            ASSERT(move._state->capture_value);
            _have_quiet_moves = true;
            move._group = MoveOrder::QUIET_MOVES;
            return false;
        }
        /* consistency check */
        ASSERT((move._state->capture_value != 0) == ctxt.state().is_capture(move));

        ctxt.get_tt()->_nodes += COUNT_VALID_MOVES_AS_NODES;
        return (_have_move = true);
    }


    INLINE bool MoveMaker::make_move(Context& ctxt, Move& move, MoveOrder group, score_t score)
    {
        if (!make_move(ctxt, move))
        {
            ASSERT(move._group >= MoveOrder::QUIET_MOVES);
            return false;
        }

        ASSERT(move._group == MoveOrder::UNORDERED_MOVES);
        move._group = group;
        move._score = score;

        return true;
    }


    INLINE void MoveMaker::mark_as_illegal(Move& move)
    {
        move._group = MoveOrder::ILLEGAL_MOVES;

        ASSERT(_count > 0);
        --_count;
    }


    INLINE void MoveMaker::sort_moves(Context& /* ctxt */, size_t start_at)
    {
        ASSERT(start_at < moves().size());

        auto& moves_list = moves();
        const auto last = moves_list.end();
        const auto first = moves_list.begin() + start_at;

        insertion_sort(first, last, [&](const Move& lhs, const Move& rhs)
            {
                return (lhs._group == rhs._group && lhs._score > rhs._score)
                    || (lhs._group < rhs._group);
            });

        _need_sort = false;
    }

} /* namespace */
