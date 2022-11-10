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
#include <memory>
#include <string>
#include <type_traits>
#include <unordered_set> /* unordered_multiset */
#include "Python.h"
#include "config.h"
#include "search.h"
#include "utility.h"


extern bool USE_NNUE;

constexpr auto FIRST_EXCHANGE_PLY = PLY_MAX;

/* Configuration API */
struct Param { int val = 0; int min_val; int max_val; std::string group; };
extern std::map<std::string, Param> _get_param_info();
extern void _set_param(const std::string&, int value, bool echo=false);
extern std::map<std::string, int> _get_params();
extern void assert_param_ref();

struct NNUE
{
    static constexpr int NO_SQUARE = 64;

    static void init(const std::string& data_dir);
    static int eval(const chess::BoardPosition&); /* test */
    static int eval_fen(const std::string& fen); /* test */

    static INLINE constexpr int piece(chess::PieceType ptype, chess::Color color)
    {
        return 7 - ptype + 6 * (color == chess::BLACK);
    }

    static void log_init_message();
};


namespace search
{
    using time = std::chrono::time_point<std::chrono::steady_clock>;

    using Bitboard = chess::Bitboard;
    using Color = chess::Color;
    using Square = chess::Square;

    using HistoryPtr = std::unique_ptr<struct History>;

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
    struct History
    {
        History() = default;

        void insert(const State& s) { _positions.insert(s); }
        size_t count(const State& s) const { return _positions.count(s); }

        std::unordered_multiset<State, Hasher<State>> _positions;
        int _fifty = 0;
    };


    /*
     * Late-move pruning counts (initialization idea borrowed from Crafty)
     */
    template<std::size_t... I>
    static constexpr std::array<int, sizeof ... (I)> lmp(std::index_sequence<I...>)
    {
        return { static_cast<int>(LMP_BASE + pow(I + .5, 1.9)) ... };
    }


    static const auto LMP = lmp(std::make_index_sequence<PLY_MAX>{});


    /*
     * Helper for making and ordering moves
     */
    class MoveMaker
    {
        static constexpr int MAX_PHASE = 4;

    public:
        MoveMaker() = default;

        const Move* get_next_move(Context& ctxt, score_t futility = 0);

        /*
         * upperbound of legal moves, which may include pruned and quiet moves
         */
        int count() const { return _count; }
        int current(Context&);

        bool has_moves(Context&);
        bool have_skipped_moves() { return _have_pruned_moves || _have_quiet_moves; }
        bool is_last(Context&);
        bool is_singleton(Context&);

        int rewind(Context&, int where, bool reorder);

    private:
        bool can_late_move_prune(const Context& ctxt) const;
        void ensure_moves(Context&);

        void generate_unordered_moves(Context&);
        const Move* get_move_at(Context& ctxt, int index, score_t futility = 0);

        void make_capture(Context&, Move&);
        template<bool LateMovePrune> bool make_move(Context&, Move&, score_t futility = 0);
        template<bool LateMovePrune> bool make_move(Context&, Move&, MoveOrder, float = 0);

        void mark_as_illegal(Move&);
        void mark_as_pruned(Context&, Move&);

        void order_moves(Context&, size_t start_at, score_t futility_margin);

        template<int Phase>
        void order_moves_phase(
            Context&,
            MovesList&,
            size_t  start_at,
            size_t  count,
            score_t futility_margin);

        void sort_moves(Context&, size_t start_at, size_t count);

        MoveMaker(const MoveMaker&) = delete;
        MoveMaker& operator=(const MoveMaker&) = delete;

        bool        _group_quiet_moves = false;
        bool        _have_move = false;
        bool        _have_quiet_moves = false;
        bool        _have_pruned_moves = false;
        bool        _need_sort = false;
        int8_t      _phase = 0; /* move ordering phase */
        int         _count = -1;
        int         _current = -1;
        size_t      _state_index = 0;
    };


    enum class LMRAction : int { None = 0, Ok, Prune };

    /* Reason for retrying */
    enum class RETRY : uint8_t { None = 0, Reduced, PVS };


    struct IterationInfo
    {
        score_t score;
        size_t nodes;
        double knps;
        int milliseconds;
    };


    /*
     * The context of a searched node.
     */
    struct Context
    {
        using ContextStack = std::array<struct ContextBuffer, PLY_MAX>;

        /* Note: half of the moves stack are reserved for do_exchanges. */
        static constexpr size_t MAX_MOVE = 2 * PLY_MAX;
        using MoveStack = std::array<MovesList, MAX_MOVE>;

        using StateStack = std::array<std::vector<State>, PLY_MAX>;

        friend class MoveMaker;

        Context() = default;
        ~Context() = default;

        Context(const Context&) = delete;
        Context& operator=(const Context&) = delete;

        static void* operator new(size_t, void* p) { return p; }
        static void* operator new(size_t) = delete;
        static void operator delete(void*, size_t) noexcept = delete;

        /* parent move in the graph */
        Context*    _parent = nullptr;
        int         _ply = 0;
        int         _max_depth = 0;

        Algorithm   _algorithm = Algorithm::NEGAMAX;

        score_t     _alpha = SCORE_MIN;
        score_t     _beta = SCORE_MAX;
        score_t     _score = SCORE_MIN; /* dynamic eval score */
        score_t     _retry_beta = SCORE_MAX; /* NEGASCOUT only */
        mutable int _improvement = SCORE_MIN;

        bool        _futility_pruning = true;
        bool        _is_null_move = false; /* for null-move pruning */
        bool        _is_pv = false;
        bool        _is_retry = false;
        bool        _is_singleton = false;

        bool        _multicut_allowed = true;
        bool        _null_move_allowed[2] = { true, true };
        RETRY       _retry_above_alpha = RETRY::None;
        bool        _retry_next = false;

        int         _double_ext = 0;
        score_t     _eval = SCORE_MIN; /* static eval */
        int         _extension = 0; /* count pending fractional extensions */
        int         _fifty = 0;
        int         _full_depth_count = late_move_reduction_count();
        int         _mate_detected = 0;
        int         _pruned_count = 0;

        Move        _move;          /* from parent to current state */
        BaseMove    _best_move;
        BaseMove    _cutoff_move;   /* from current state to the next */
        BaseMove    _prev;          /* best move from previous iteration */
        BaseMove    _excluded;      /* singular extension search */

        State*      _state = nullptr;
        TT_Entry    _tt_entry;
        Square      _capture_square = Square::UNDEFINED;

        static void cancel() { _cancel.store(true, std::memory_order_relaxed); }

        bool        can_forward_prune() const;

        template<bool PruneCaptures = false> bool can_prune() const;
        template<bool PruneCaptures = false> bool can_prune_move(const Move&) const;

        bool        can_reduce() const;

        int64_t     check_time_and_update_nps(); /* return elapsed milliseconds */
        Context*    clone(ContextBuffer&, int ply = 0) const;

        int         depth() const { return _max_depth - _ply; }
        static int  elapsed_milliseconds();

        static void ensure_stacks();

        std::string epd() const { return epd(state()); }
        static std::string epd(const State&);

        /* Static evaluation */
        score_t     _evaluate();

        template<bool EvalCaptures = true> score_t evaluate();

        score_t     evaluate_end();
        score_t     evaluate_material(bool with_piece_squares = true) const;
        void        eval_incremental();
        score_t     static_eval();

        void        extend();       /* fractional extensions */
        const Move* first_valid_move();
        score_t     futility_margin();

        bool        has_improved(score_t margin = 0) const { return improvement() > margin; }
        bool        has_moves() { return _move_maker.has_moves(*this); }

        int         history_count(const Move&) const;
        float       history_score(const Move&) const;

        score_t     improvement() const;
        static void init();
        bool        is_beta_cutoff(Context*, score_t);
        static bool is_cancelled() { return _cancel.load(std::memory_order_relaxed); }
        bool        is_capture() const { return state().capture_value != 0; }
        bool        is_check() const { return state().is_check(); }
        bool        is_counter_move(const Move&) const;
        bool        is_evasion() const;
        bool        is_extended() const;
        bool        is_last_move();
        bool        is_leftmost() const { return _ply == 0 || _leftmost; }
        bool        is_leaf(); /* treat as terminal node ? */
        bool        is_mate_bound() const;
        bool        is_null_move_ok(); /* ok to generate null move? */
        bool        is_null_move() const { return _is_null_move; }
        bool        is_promotion() const { return state().promotion; }
        bool        is_pv_node() const { return _is_pv; }
        bool        is_pvs_ok() const;
        bool        is_qsearch() const { return _ply > _max_depth; }
        bool        is_recapture() const;
        bool        is_reduced() const;
        int         is_repeated() const;
        bool        is_retry() const { return _is_retry; }
        int         iteration() const { ASSERT(_tt); return _tt->_iteration; }

        LMRAction   late_move_reduce(int move_count);
        static int  late_move_reduction_count();

        static void log_message(LogLevel, const std::string&, bool force = true);

        int64_t     nanosleep(int nanosec);

        Context*    next(bool null_move, score_t, int move_count);

        template<bool Construct = false> Context* next_ply() const;

        int         next_move_index() { return _move_maker.current(*this); }
        bool        on_next();
        void        reinitialize();
        int         rewind(int where = 0, bool reorder = false);
        void        set_counter_move(const BaseMove& move) { _counter_move = move; }
        void        set_search_window(score_t score, score_t& prev_score);
        static void set_time_limit_ms(int milliseconds);
        void        set_time_info(int time_left /* millisec */, int moves_left, score_t eval);
        void        set_tt(TranspositionTable* tt) { _tt = tt; }
        bool        should_verify_null_move() const;
        int         singular_margin() const;
        int         tid() const { return _tt->_tid; }
        static int  time_limit() { return _time_limit.load(std::memory_order_relaxed); }
        Color       turn() const { return state().turn; }

        const State& state() const { ASSERT(_state); return *_state; }
        TranspositionTable* get_tt() const { return _tt; }

        const MovesList& moves() const { return moves(tid(), _ply); }
        MovesList& moves() { return moves(tid(), _ply); }

        void set_moves(const Context& from_ctxt)
        {
            auto first = from_ctxt.moves().begin();
            auto last = first + std::max(0, from_ctxt._move_maker.count());

            _tt->_moves.assign(first, last);
            _tt->_moves_hash = from_ctxt.state().hash();
        }

        /* retrieve PV from TT */
        const PV& get_pv() const { return get_tt()->get_pv(); }

        /* buffers for generating and making moves */
        static MovesList& moves(int tid, int ply);
        static std::vector<State>& states(int tid, int ply);

        const ContextStack& stack() const { return _context_stacks[tid()]; }

        static void set_syzygy_path(const std::string& path) { _syzygy_path = path; }
        static const std::string& syzygy_path() { return _syzygy_path; }
        static void set_tb_cardinality(int n) { _tb_cardinality = n; }
        static int tb_cardinality() { return _tb_cardinality.load(std::memory_order_relaxed); }

        /*
         * Python callbacks
         */
        static PyObject*    _engine; /* searcher instance */

        static std::string  (*_epd)(const State&);
        static void         (*_log_message)(int, const std::string&, bool);
        static void         (*_on_iter)(PyObject*, Context*, const IterationInfo*);
        static void         (*_on_move)(PyObject*, const std::string&, int);
        static void         (*_on_next)(PyObject*, int64_t);
        static std::string  (*_pgn)(Context*);
        static void         (*_print_state)(const State&);
        static void         (*_report)(PyObject*, std::vector<Context*>&);
        static bool         (*_tb_probe_wdl)(const State&, int*);
        static size_t       (*_vmem_avail)();

        static HistoryPtr   _history;

    private:
        const Move* get_next_move(score_t);
        bool has_cycle(const State&) const;

        int repeated_count(const State&) const;

        BaseMove            _counter_move;
        mutable int8_t      _can_prune = -1;

        mutable int8_t      _repetitions = -1;
        bool                _leftmost = false;
        MoveMaker           _move_maker;

        TranspositionTable* _tt = nullptr;

        /* search can be cancelled from any thread */
        static atomic_bool  _cancel;

        static size_t       _callback_count;
        static atomic_int   _time_limit; /* milliseconds */
        static atomic_time  _time_start;
        static std::string  _syzygy_path;
        static atomic_int   _tb_cardinality;

        static std::vector<ContextStack>    _context_stacks;
        static std::vector<MoveStack>       _move_stacks;
        static std::vector<StateStack>      _state_stacks;
    };


    /*
     * Helper data structure for allocating context on ContextStacks
     */
    static_assert(std::is_trivially_destructible<Context>::value);

    struct alignas(64) ContextBuffer
    {
        std::array<uint8_t, sizeof(Context)> _mem = { 0 };
        State _state; /* for null-move and clone() */

        Context* as_context() { return reinterpret_cast<Context*>(&_mem[0]); }
        const Context* as_context() const { return reinterpret_cast<const Context*>(&_mem[0]); }
    };


    template<bool Debug = false>
    int do_exchanges(const State&, Bitboard, score_t, int tid, int ply = FIRST_EXCHANGE_PLY);


    extern score_t eval_captures(Context& ctxt);


    /*
     * Evaluate same square exchanges
     */
    template<bool StaticExchangeEvaluation>
    score_t eval_exchanges(int tid, const Move& move)
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
                val = do_exchanges<DEBUG_CAPTURES != 0>(*move._state, mask, 0, tid);
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
        return state.eval_lazy() * SIGN[!state.turn];
    }


    INLINE void incremental_update(Move& move, const Context& ctxt)
    {
        ASSERT(move._state);

        if (move._state->simple_score == State::UNKNOWN_SCORE)
        {
            move._state->eval_apply_delta(move, ctxt.state());
        }

        /* post-condition */
        ASSERT(move._state->simple_score == move._state->eval_simple());
    }


    INLINE bool is_direct_check(const Move& move)
    {
        ASSERT(move);
        ASSERT(move._state);

        const auto& state = *move._state;
        if (state.attacks_mask(move.to_square(), state.occupied()) & state.kings & state.occupied_co(state.turn))
        {
            ASSERT(state.is_check());
            return true;
        }
        return false;
    }


    INLINE bool is_quiet(const State& state, const Context* ctxt = nullptr)
    {
        return state.promotion != chess::PieceType::QUEEN /* ignore under-promotions */
            && state.capture_value == 0
            && state.pushed_pawns_score <= 1
            && (!ctxt || !ctxt->is_evasion())
            && !state.is_check();
    }


    INLINE bool Context::can_forward_prune() const
    {
        if (_can_prune == -1)
        {
            _can_prune =
                (_parent != nullptr)
                && !is_pv_node()
                && !_excluded
                && (state().pushed_pawns_score <= 1)
                && !state().just_king_and_pawns()
                && (_parent->_mate_detected == 0 || _parent->_mate_detected % 2)
                && !is_check();
        }
        return _can_prune > 0;
    }


    template<bool PruneCaptures>
    INLINE bool Context::can_prune() const
    {
        ASSERT(_ply > 0);
        ASSERT(!is_null_move());
        ASSERT(_move);

        return !is_extended()
            && !is_pv_node()
            && !is_repeated()
            && _parent->can_prune_move<PruneCaptures>(_move);
    }


    template<bool PruneCaptures>
    INLINE bool Context::can_prune_move(const Move& move) const
    {
        ASSERT(move && move._state && move != _move);

        return (move != _tt_entry._hash_move)
            && (PruneCaptures || move._state->capture_value == 0)
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
            && (state().pushed_pawns_score <= 1)
            && !is_extended()
            && (_move.from_square() != _parent->_capture_square)
            && !is_recapture()
            && !state().is_check();
    }


    /* static */ INLINE int Context::elapsed_milliseconds()
    {
        const auto now = std::chrono::steady_clock::now();
        return std::chrono::duration_cast<std::chrono::milliseconds>(
            now - _time_start.load(std::memory_order_relaxed)
        ).count();
    }


#if !WITH_NNUE /* dummy */
    INLINE void Context::eval_incremental()
    {
    }
#endif /* !WITH_NNUE */


    /*
     * Use value from the TT if available,
     * else do a quick material evaluation.
     */
    INLINE score_t Context::static_eval()
    {
        return _eval == SCORE_MIN ? evaluate_material() : _eval;
    }


    template<bool EvalCaptures> INLINE score_t Context::evaluate()
    {
        ASSERT(_fifty < 100);
        ASSERT(_ply == 0 || !is_repeated());

        ++_tt->_eval_count;

        auto score = _evaluate();

        ASSERT(score > SCORE_MIN);
        ASSERT(score < SCORE_MAX);

        if constexpr(EvalCaptures)
        {
            if (abs(score) < MATE_HIGH)
            {
                /* 3. Captures */
                const auto capt = eval_captures(*this);

                ASSERT(capt <= CHECKMATE);

                if (capt >= MATE_HIGH)
                    score = capt - 1;
                else
                    score += capt;

                ASSERT(score > SCORE_MIN);
                ASSERT(score < SCORE_MAX);
            }
        }
        return score;
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


    /*
     * Futility pruning margins.
     */
    static constexpr double PHI = 1.61803398875;

    template<std::size_t... I>
    static constexpr std::array<int, sizeof ... (I)> margins(std::index_sequence<I...>)
    {
        return { static_cast<int>(50 * I + pow(I + PHI, M_E)) ... };
    }


    INLINE score_t Context::futility_margin()
    {
        if (_ply == 0 || !_futility_pruning || depth() < 1)
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

        const auto score = _tt->history_score(_ply, state(), turn(), move);
        return score + COUNTER_MOVE_BONUS * is_counter_move(move);
    }


    /*
     * Improvement for the side that just moved.
     */
    INLINE score_t Context::improvement() const
    {
        if (_improvement < 0)
        {
            if (_ply < 2 || _excluded || is_promotion())
            {
                _improvement = 0;
            }
            else
            {
                const auto prev = _parent->_parent;

                if (abs(_eval) < MATE_HIGH && abs(prev->_eval) < MATE_HIGH)
                {
                    _improvement = std::max(0, prev->_eval - _eval);
                }
                else
                {
                    _improvement = std::max(0,
                          eval_material_and_piece_squares(*_state)
                        - eval_material_and_piece_squares(*prev->_state));
                }
            }
        }
        return _improvement;
    }


    INLINE bool Context::is_counter_move(const Move& move) const
    {
        return _counter_move == move;
    }


    INLINE bool Context::is_evasion() const
    {
        return _parent && _parent->is_check();
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


    /*
     * Ok to generate a null-move?
     */
    INLINE bool Context::is_null_move_ok()
    {
        if (_ply == 0
            || _null_move_allowed[turn()] == false
            || _excluded
            || is_null_move() /* consecutive null moves are not allowed */
            || is_qsearch()
            || is_pv_node()
            || is_mate_bound()
            || is_repeated()
            || is_check()
            || state().just_king_and_pawns()
           )
            return false;

        ASSERT(depth() >= 0);

        return static_eval() >= _beta
            - NULL_MOVE_DEPTH_WEIGHT * depth()
            - improvement() / NULL_MOVE_IMPROVEMENT_DIV
            + NULL_MOVE_MARGIN;
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


    INLINE bool Context::has_cycle(const State& state) const
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


    INLINE int Context::repeated_count(const State& state) const
    {
        ASSERT(_history);
        return int(_history->count(state)) + has_cycle(state);
    }


    INLINE int Context::is_repeated() const
    {
        if (_repetitions < 0)
        {
            ASSERT(_history);
            _repetitions = repeated_count(state());

            ASSERT(_repetitions >= 0);
        }
        return _repetitions;
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
            + std::min(int(NULL_MOVE_MIN), (ctxt.static_eval() - ctxt._beta) / NULL_MOVE_DIV);
    }


    /*
     * Get the next move and wrap it into a Context object.
     */
    INLINE Context* Context::next(bool null_move, score_t futility, int move_count)
    {
        ASSERT(_alpha < _beta);

        const bool retry = _retry_next;
        _retry_next = false;

        if (!_excluded && !on_next() && move_count > 0)
            return nullptr;

        /* null move must be tried before actual moves */
        ASSERT(!null_move || move_count == 0);

        const Move* move;

        if (null_move)
            move = nullptr;
        else
            if ((move = get_next_move(futility)) == nullptr)
                return nullptr;

        ASSERT(null_move || move->_state);
        ASSERT(null_move || move->_group != MoveOrder::UNDEFINED);
        ASSERT(null_move || move->_group < MoveOrder::UNORDERED_MOVES);

        auto ctxt = next_ply<true>();

        if (move)
        {
            ASSERT(move->_state);
            ASSERT(ctxt->_is_null_move == false);
            ctxt->_move = *move;
            ctxt->_state = move->_state;
            ctxt->_leftmost = is_leftmost() && next_move_index() == 1;

        #if REPORT_CURRENT_MOVE
            /* Report (main thread only) the move being searched from the root. */
            if (_ply == 0
                && tid() == 0
                && _on_move
                && (_tt->_nodes % 1000) <= move_count
                && time_limit() > 250
               )
                (*_on_move)(_engine, move->uci(), move_count + 1);
        #endif /* REPORT_CURRENT_MOVE */
        }
        else
        {
            ASSERT(null_move);
            ASSERT(!ctxt->_move);
            ASSERT(ctxt->_state);
            state().clone_into(*ctxt->_state);
            ctxt->_state->_check = { 0, 0 };
            flip(ctxt->_state->turn);
            ctxt->_is_null_move = true;
        }

        ctxt->_algorithm = _algorithm;
        ctxt->_parent = this;
        ctxt->_max_depth = _max_depth;
        ctxt->_ply = _ply + 1;
        ctxt->_double_ext = _double_ext;
        ctxt->_extension = _extension;
        ctxt->_is_retry = retry;
        if (_ply == 0)
            ctxt->_is_singleton = !ctxt->is_null_move() && _move_maker.is_singleton(*this);
        ctxt->_futility_pruning = _futility_pruning && FUTILITY_PRUNING;
        ctxt->_multicut_allowed = _multicut_allowed && MULTICUT;

        for (auto side : { chess::BLACK, chess::WHITE })
            ctxt->_null_move_allowed[side] = _null_move_allowed[side] && (NULL_MOVE_REDUCTION > 0);

        ctxt->_tt = _tt;
        ctxt->_alpha = -_beta;

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
            ctxt->_beta = -_alpha;

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
            if constexpr(FIFTY_MOVES_RULE)
            {
                if (ctxt->is_capture())
                    ctxt->_fifty = 0;
                else if (state().pawns & chess::BB_SQUARES[move->from_square()])
                    ctxt->_fifty = 0;
                else
                    ctxt->_fifty = (_ply == 0 ? _history->_fifty : _fifty) + 1;
            }
        }

        ASSERT(ctxt->_alpha < ctxt->_beta);
        ASSERT(ctxt->_alpha >= ctxt->_score);
        ASSERT(ctxt->_score == SCORE_MIN);

        return ctxt;
    }


    /*
     * Check how much time is left, update NPS, call optional Python callback.
     */
    INLINE bool Context::on_next()
    {
        if (tid() == 0 && ++_callback_count >= CALLBACK_PERIOD)
        {
            _callback_count = 0; /* reset */
            const auto millisec = check_time_and_update_nps();

            if (millisec < 0) /* time is up? */
                return false;

            if (_on_next)
                cython_wrapper::call(_on_next, _engine, millisec);
        }

        return !is_cancelled();
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


    template<bool Construct> INLINE Context* Context::next_ply() const
    {
        ASSERT(_ply < PLY_MAX);

        auto& buffer = _context_stacks[tid()][_ply];

        if constexpr(Construct)
        {
            auto ctxt = new (buffer.as_context()) Context;
            ctxt->_state = &buffer._state;
            return ctxt;
        }
        else
        {
            return buffer.as_context();
        }
    }


    /* static */ INLINE MovesList& Context::moves(int tid, int ply)
    {
        ASSERT(ply >= 0);
        ASSERT(size_t(ply) < MAX_MOVE);

        return _move_stacks[tid][ply];
    }


    /* static */ INLINE std::vector<State>& Context::states(int tid, int ply)
    {
        ASSERT(ply >= 0);
        ASSERT(size_t(ply) < PLY_MAX);

        return _state_stacks[tid][ply];
    }


    INLINE bool MoveMaker::can_late_move_prune(const Context& ctxt) const
    {
        ASSERT(_phase > 2);
        return ctxt.depth() > 1 /* do not LMP leaf nodes */
            && _current >= LMP[ctxt.depth() - 1]
            && ctxt.can_forward_prune();
    }


    /*
     * MoveMaker Helper Class
     */
    INLINE int MoveMaker::current(Context& ctxt)
    {
        ensure_moves(ctxt);
        return _current;
    }


    INLINE void MoveMaker::ensure_moves(Context& ctxt)
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


    INLINE const Move* MoveMaker::get_next_move(Context& ctxt, score_t futility)
    {
        ensure_moves(ctxt);

        /* ensure_moves post-condition */
        ASSERT(_current <= _count);

        auto move = get_move_at(ctxt, _current, futility);

        if (move)
        {
            ++_current;
        }
        else
        {
            ASSERT(_current == _count || ctxt.moves()[_current]._group >= PRUNED_MOVES);
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

        auto& moves_list = ctxt.moves();
        ASSERT(!moves_list.empty());

        auto move = &moves_list[index];
        ASSERT(move->_group != MoveOrder::UNDEFINED);

        while (move->_group == MoveOrder::UNORDERED_MOVES)
        {
            if (_phase > 2 && can_late_move_prune(ctxt))
            {
                mark_as_pruned(ctxt, *move);
                return nullptr;
            }

            order_moves(ctxt, index, futility);
            move = &moves_list[index];
        }

        ASSERT(move->_group != MoveOrder::UNORDERED_MOVES);

        if (move->_group >= MoveOrder::PRUNED_MOVES)
        {
            ASSERT(index <= _count);
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
        ASSERT(_current > 0);

        if (_current > 1 || ctxt._pruned_count || have_skipped_moves())
            return false;

        ASSERT(_current == 1);

        if (_count == 1)
        {
        #if !defined(NO_ASSERT)
            /* checked for skipped moves above, make sure there aren't any */
            const auto& all_moves = ctxt.moves();
            for (const auto& move : all_moves)
            {
                ASSERT(move._group != MoveOrder::QUIET_MOVES);
                ASSERT(move._group != MoveOrder::PRUNED_MOVES);
            }
        #endif /* !(NO_ASSERT) */
            return true;
        }
        /*
         * get_move_at() is expensive;
         * use only if moving out of check.
         */
        return ctxt.is_check() && !get_move_at(ctxt, _current) && !have_skipped_moves();
    }


    INLINE void MoveMaker::make_capture(Context& ctxt, Move& move)
    {
        /* captures of the last piece moved by the opponent are handled separately */
        ASSERT(move.to_square() != ctxt._move.to_square());

        if (make_move<false>(ctxt, move))
        {
            ASSERT(move._state->capture_value);

            /*
             * Now determine which capture group it belongs to.
             */
            auto capture_gain = move._state->capture_value;
            auto other = ctxt.state().piece_weight_at(move.from_square());

            /* skip exchange evaluation if the capturer is worth less than the captured */

            if (other >= capture_gain && abs(ctxt._score) < MATE_HIGH)
            {
                other = eval_exchanges<true>(ctxt.tid(), move);

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
            #if FAVOR_SACRIFICES
                const auto eval = NNUE::eval(*move._state);
                /* if the neural net thinks the position is good for the side that moved... */
                if (eval < 0 && capture_gain - eval > SACRIFICE_MARGIN)
                {
                    move._group = MoveOrder::WINNING_CAPTURES;
                    move._score = -eval;
                }
            #endif /* FAVOR_SACRIFICES */
            }
            else
            {
                static_assert(MoveOrder::WINNING_CAPTURES + 1 == MoveOrder::EQUAL_CAPTURES);
                move._group = MoveOrder::WINNING_CAPTURES + (capture_gain == 0);
            }

            move._score = capture_gain;
        }
    }


    /*
     * Return false if the move is not legal, or pruned.
     */
    template<bool LateMovePrune>
    INLINE bool MoveMaker::make_move(Context& ctxt, Move& move, score_t futility)
    {
        ASSERT(move);
        ASSERT(move._group == MoveOrder::UNORDERED_MOVES);

        _need_sort = true;

        if (move._state == nullptr)
        {
            ASSERT(_state_index < Context::states(ctxt.tid(), ctxt._ply).size());

            move._state = &Context::states(ctxt.tid(), ctxt._ply)[_state_index++];
        }
        else
        {
            return (_have_move = true);
        }

        /* Capturing the king is an illegal move (Louis XV?) */
        if (ctxt.state().kings & chess::BB_SQUARES[move.to_square()])
        {
            mark_as_illegal(move);
            return false;
        }

        /* Late-move prune before making the move. */
        if constexpr(LateMovePrune)
        {
            if (can_late_move_prune(ctxt))
            {
                mark_as_pruned(ctxt, move);
                return false;
            }

            /* History-based pruning. */
            if (ctxt.depth() > 0
                && ctxt.history_count(move) >= HISTORY_PRUNE * pow2(ctxt.depth())
                && ctxt.history_score(move) < HISTORY_LOW
                && ctxt.can_prune_move(move))
            {
                mark_as_pruned(ctxt, move);
                return false;
            }
        }

        ctxt.state().clone_into(*move._state);
        ASSERT(move._state->capture_value == 0);

        move._state->apply_move(move);

        if (_group_quiet_moves && is_quiet(*move._state))
        {
            _have_quiet_moves = true;
            move._group = MoveOrder::QUIET_MOVES;
            return false;
        }

        incremental_update(move, ctxt);

        /* Futility pruning (1st pass, 2nd pass done in search) */
        /* Prune after making the move (state is needed for simple eval). */

        if (futility > 0)
        {
            /* The futility margin is calculated after at least one move has been searched. */
            ASSERT(_current > 0);

            const auto val = futility + move._state->simple_score * SIGN[!move._state->turn];

            if ((val < ctxt._alpha || val < ctxt._score) && ctxt.can_prune_move<true>(move))
            {
                if constexpr(EXTRA_STATS)
                    ++ctxt.get_tt()->_futility_prune_count;

                mark_as_pruned(ctxt, move);
                return false;
            }
        }

        if (move._state->is_check(ctxt.turn()))
        {
            mark_as_illegal(move); /* can't leave the king in check */
            return false;
        }

        /* consistency check */
        ASSERT((move._state->capture_value != 0) == ctxt.state().is_capture(move));

        if constexpr(COUNT_VALID_MOVES_AS_NODES)
            ++ctxt.get_tt()->_nodes;

        return (_have_move = true);
    }


    template<bool LateMovePrune>
    INLINE bool MoveMaker::make_move(Context& ctxt, Move& move, MoveOrder group, float score)
    {
        if (!make_move<LateMovePrune>(ctxt, move))
        {
            ASSERT(move._group >= MoveOrder::PRUNED_MOVES);
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


    INLINE void MoveMaker::mark_as_pruned(Context& ctxt, Move& move)
    {
        move._group = MoveOrder::PRUNED_MOVES;
        _have_pruned_moves = true;
        ++ctxt._pruned_count;
    }


    /*
     * Sort moves ascending by group, descending by score within each group.
     */
    INLINE bool compare_moves_gt(const Move& lhs, const Move& rhs)
    {
        return (lhs._group == rhs._group && lhs._score > rhs._score) || (lhs._group < rhs._group);
    }


    INLINE bool compare_moves_ge(const Move& lhs, const Move& rhs)
    {
        return !compare_moves_gt(rhs, lhs);
    }


    INLINE void MoveMaker::sort_moves(Context& ctxt, size_t start_at, size_t count)
    {
        ASSERT(start_at < ctxt.moves().size());
        ASSERT(count <= ctxt.moves().size());

        auto& moves_list = ctxt.moves();
        const auto first = moves_list.begin() + start_at;
        const auto last = moves_list.begin() + count;

        insertion_sort(first, last, compare_moves_gt);

        _need_sort = false;
    }

} /* namespace */
