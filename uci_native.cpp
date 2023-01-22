/** Native C++ UCI */
/** http://wbec-ridderkerk.nl/html/UCIProtocol.html */
#include <iostream>
#include <string_view>
#include "context.h"

/** Raise RuntimeError, and let Python handle it... */
static void raise_runtime_error(const char* err)
{
    PyGILState_STATE with_gil(PyGILState_Ensure());
    PyErr_SetString(PyExc_RuntimeError, err);
    PyGILState_Release(with_gil);
}

#if NATIVE_UCI /* experimental */
#include <cmath>
#include <format>
#include <map>
#include <memory>
#include <ranges>
#include <string>
#include <sstream>
#include <vector>
#include "nnue.h"
#include "thread_pool.hpp" /* pondering, go infinite */

#if 0
  /* enable additional logging */
  #define LOG_DEBUG(x) while (_debug) { log_debug((x)); break; }
#else
  #define LOG_DEBUG(x)
#endif
#define OUTPUT_POOL false

static constexpr auto INFINITE = -1;
static std::string g_out; /* global output buffer */

namespace std
{
    INLINE std::string to_string(std::string_view v)
    {
        return std::string(v);
    }
}

namespace
{
    static constexpr std::string_view START_POS{"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"};
    static bool _debug = false; /* enable verbose logging */

    template <typename T>
    static void log_error(T err)
    {
        try
        {
            search::Context::log_message(LogLevel::ERROR, std::to_string(err));
        }
        catch (...)
        {
        }
    }

    template <typename T> static void log_debug(T msg)
    {
        search::Context::log_message(LogLevel::DEBUG, std::to_string(msg));
    }

    template <typename T> static void log_warning(T warn)
    {
        search::Context::log_message(LogLevel::WARN, std::to_string(warn));
    }

    template <typename T> static INLINE T &lowercase(T &s)
    {
        std::transform(s.begin(), s.end(), s.begin(), [](auto c) { return std::tolower(c); });
        return s;
    }

    template <typename T> static INLINE std::string join(std::string_view sep, const T &v)
    {
        std::ostringstream s;
        for (const auto &elem : v)
            (s.tellp() ? s << sep : s) << elem;
        return s.str();
    }

    INLINE void output(std::ostream& out, const std::string& str)
    {
        out.write(str.data(), str.size());
    }

    template <bool flush=true>
    INLINE void output(const std::string& str)
    {
        output(std::cout, str);
        std::cout.write("\n", 1);
        if (_debug)
            log_debug(std::format("<<< {}", str));
        if constexpr(flush)
            std::cout.flush();
    }

    /** Raise ValueError exception, and exit with error (see dtor of GIL_State) */
    template <typename... Args>
#if _MSC_VER
    void raise_value_error(std::_Fmt_string<Args...> fmt, Args&&... args)
#else
    void raise_value_error(std::format_string<Args...> fmt, Args&&... args)
#endif
    {
        const auto err = std::format(fmt, std::forward<Args>(args)...);
        cython_wrapper::GIL_State with_gil;
        log_error(err);
        PyErr_SetString(PyExc_ValueError, err.c_str());
    }
} /* namespace */

namespace
{
    /*
    https://stackoverflow.com/questions/27866909/get-function-arity-from-template-parameter
    */
    template <typename T> struct arity {};

    template <typename R, typename C, typename... Args>
    struct arity<R (C::*)(Args...)> : std::integral_constant<unsigned, sizeof...(Args)> {};

    template <typename T> INLINE int to_int(T v)
    {
        try
        {
            return std::stoi(std::string(v));
        }
        catch(const std::exception&)
        {
            if (v == "false")
                return 0;
            if (v == "true")
                return 1;
            throw;
        }
    }


    struct Option
    {
        virtual ~Option() = default;
        virtual void print(std::ostream &) const = 0;
        virtual void set(std::string_view value) = 0;
    };

    struct OptionBase : public Option
    {
        const std::string _name;
        explicit OptionBase(const std::string &name) : _name(name) {}
        void print(std::ostream &out) const override { out << _name << " "; }
    };

    struct OptionAlgo : public OptionBase
    {
        search::Algorithm &_algo;

        explicit OptionAlgo(search::Algorithm& algo) : OptionBase("Algorithm"), _algo(algo) {}
        void print(std::ostream &out) const override
        {
            OptionBase::print(out);
            out << "type combo default " << name(_algo) << " var mtdf var negascout var negamax";
        }
        std::string_view name(search::Algorithm algo) const
        {
            switch (algo)
            {
            case search::Algorithm::MTDF: return "mtdf";
            case search::Algorithm::NEGAMAX: return "negamax";
            case search::Algorithm::NEGASCOUT: return "negascout";
            }
            return "";
        }
        void set(std::string_view value) override
        {
            if (value == "mtdf") _algo = search::Algorithm::MTDF;
            else if (value == "negascout") _algo = search::Algorithm::NEGASCOUT;
            else if (value == "negamax") _algo = search::Algorithm::NEGAMAX;
        }
    };

    struct OptionBool : public OptionBase
    {
        bool &_b;

        OptionBool(const std::string &name, bool &b) : OptionBase(name), _b(b)
        {
        }

        void print(std::ostream &out) const override
        {
            OptionBase::print(out);
            out << "type check default " << std::boolalpha << _b;
        }

        void set(std::string_view value) override
        {
            if (value == "true")
                _b = true;
            else if (value == "false")
                _b = false;
        }
    };

    struct OptionParam : public OptionBase
    {
        const Param _p;

        OptionParam(const std::string &name, const Param &param) : OptionBase(name), _p(param) {}

        void print(std::ostream &out) const override
        {
            OptionBase::print(out);
            if (_p.min_val == 0 && _p.max_val == 1)
                out << "type check default " << std::boolalpha << bool(_p.val);
            else
                out << "type spin default " << _p.val << " min " << _p.min_val << " max " << _p.max_val;
        }

        void set(std::string_view value) override { _set_param(_name, to_int(value), true); }
    };

    struct OptionEvalFile : public OptionBase
    {
        std::string &_eval_file;
        explicit OptionEvalFile(std::string& eval_file) : OptionBase("EvalFile"), _eval_file(eval_file) {}

        void print(std::ostream& out) const override
        {
            OptionBase::print(out);
            out << "type string default " << _eval_file;
        }

        void set(std::string_view value) override
        {
            if (nnue_init(std::string(value).c_str()))
                _eval_file = value;
            else
                raise_value_error("invalid NNUE file: {}", value);  /* refuse to run without valid NNUE */
        }
    };

    struct OptionSyzygy : public OptionBase
    {
        OptionSyzygy() : OptionBase("SyzygyPath") {}

        void print(std::ostream& out) const override
        {
            OptionBase::print(out);
            out << "type string";
            const auto &path = search::Context::syzygy_path();
            if (!path.empty())
                out << " default " << path;
        }

        void set(std::string_view value) override
        {
            search::Context::set_syzygy_path(std::string(value));
        }
    };
}

using ThreadPool = thread_pool<>;

class UCI
{
    using Arguments = std::vector<std::string_view>;
    using EngineOptions = std::map<std::string, std::shared_ptr<Option>>;

    static constexpr int max_depth = PLY_MAX;

public:
    UCI(const std::string &name, const std::string &version)
        : _name(name)
        , _version(version)
        , _use_opening_book(search::Context::_book_init(_book))
    {
        search::Context::_history = std::make_unique<search::History>();
        search::Context::log_message(LogLevel::INFO,
            std::format("TT_Entry size: {}", std::to_string(sizeof(search::TT_Entry))));

        set_start_position();

        search::Context::_on_iter = on_iteration;
        search::Context::_on_move = on_move;

        refresh_options();
        _options.emplace("algorithm", std::make_shared<OptionAlgo>(_algorithm));
        _options.emplace("best opening", std::make_shared<OptionBool>("Best Opening", _best_book_move));
        _options.emplace("debug", std::make_shared<OptionBool>("Debug", _debug));
        _options.emplace("ownbook", std::make_shared<OptionBool>("OwnBook", _use_opening_book));
        _options.emplace("ponder", std::make_shared<OptionBool>("Ponder", _ponder));
        _options.emplace("evalfile", std::make_shared<OptionEvalFile>(_eval_file));
        _options.emplace("syzygypath", std::make_shared<OptionSyzygy>());
    }

    static bool output_expected() { return _output_expected.load(std::memory_order_relaxed); }
    void run();

private:
    void dispatch(std::string &, const Arguments &args);

    /** UCI commands */
    void debug();
    void go(const Arguments &args);
    void isready();
    void ponderhit();
    void position(const Arguments &args);
    void setoption(const Arguments &args);
    void stop();
    void uci();
    void newgame();

    /** Context callbacks */
    static void on_iteration(PyObject *, search::Context *, const search::IterationInfo *);
    static void on_move(PyObject *, const std::string&, int);

private:
    /** position() helper */
    template <typename T> INLINE void apply_moves(const T &moves)
    {
        _last_move = chess::BaseMove();
        _ply_count = 0;

        search::Context::_history->emplace(_buf._state);

        for (const auto &m : moves)
            if (m.size() >= 4)
            {
                chess::Square from, to;

                if (chess::parse_square(m, from) && chess::parse_square(std::string_view(&m[2], 2), to))
                {
                    const auto promo = m.size() > 4 ? chess::piece_type(m[4]) : chess::PieceType::NONE;
                    const auto move = chess::BaseMove(from, to, promo);
                    const auto prev = _buf._state;
                    _buf._state.apply_move(move);
                    chess::zobrist_update(prev, move, _buf._state);
                    ASSERT(_buf._state._hash == chess::zobrist_hash(_buf._state));
                    /* keep track of played moves, to detect repetitions */
                    search::Context::_history->emplace(_buf._state);
                    /* update the halfmove clock */
                    if (_buf._state.capture_value || prev.piece_type_at(from) == chess::PieceType::PAWN)
                        search::Context::_history->_fifty = 0;
                    else
                        ++search::Context::_history->_fifty;
                    _last_move = move;
                    ++_ply_count;
                }
            }
        if (moves.empty())
            _buf._state.hash();
        ASSERT(_buf._state._hash);
    }

    INLINE search::Context &context() { return *_buf.as_context(); }

    template <bool synchronous=false>
    INLINE void output_best_move(bool request_ponder = false)
    {
        if (output_expected())
        {
            auto &ctxt = context();
            auto move = ctxt._best_move;
            if (!move)
                if (auto first = ctxt.first_valid_move())
                    move = *first;
            if constexpr(synchronous)
                output_best_move(move, request_ponder);
            else
                #if OUTPUT_POOL
                    _output_pool->push_task([this, move, request_ponder] {
                        output_best_move(move, request_ponder);
                    });
                #else
                    output_best_move(move, request_ponder);
                #endif /* OUTPUT_POOL */
        }
    }

    INLINE void output_best_move(const chess::BaseMove &move, bool request_ponder = false)
    {
        ASSERT(output_expected());
        _output_expected = false;

        if (!move)
        {
            output("bestmove 0000");
        }
        else
        {
            g_out.clear();
            if (request_ponder && _ponder)
            {
                const auto &pv = _tt.get_pv();
                if (pv.size() > 2 && pv[1] == move)
                {
                    std::format_to(std::back_inserter(g_out), "bestmove {} ponder {}", move.uci(), pv[2].uci());
                    output(g_out);
                    return;
                }
            }
            std::format_to(std::back_inserter(g_out), "bestmove {}", move.uci());
            output(g_out);
        }
    }

    INLINE void set_start_position()
    {
        _buf._state = chess::State();
        _buf._state.castling_rights = chess::BB_DEFAULT_CASTLING_RIGHTS;
        chess::epd::parse_pos(START_POS, _buf._state);
        _buf._state.rehash();
    }

    void refresh_options()
    {
        for (auto p : _get_param_info())
        {
            auto name = p.first;
            /* option names are case insensitive, and can contain _single_ spaces */
            _options[lowercase(name)] = std::make_shared<OptionParam>(p.first, p.second);
        }
    }

    /** think on opponent's time */
    void ponder();

    /** iterative deepening search */
    template<typename F = void(*)()> score_t search(F f = []{});

    search::Algorithm _algorithm = search::Algorithm::MTDF;
    search::ContextBuffer _buf;
    search::TranspositionTable _tt;
    std::string _book = "book.bin";
    std::string _eval_file = NNUE_EVAL_FILE;
    std::atomic_int _extended_time = 0; /* for pondering */
    int _book_depth = max_depth;
    int _depth = max_depth;
    int _ply_count = 0;
    score_t _score = 0;
    score_t _score_delta = 0;
    EngineOptions _options;
    const std::string _name;
    const std::string _version; /* engine version */
    static std::unique_ptr<ThreadPool> _compute_pool;
#if OUTPUT_POOL
    static std::unique_ptr<ThreadPool> _output_pool;
#endif /* OUTPUT_POOL */
    static std::atomic_bool _output_expected;
    bool _ponder = false;
    bool _use_opening_book = false;
    bool _best_book_move = false;
    chess::BaseMove _last_move;
};

#if OUTPUT_POOL
std::unique_ptr<ThreadPool> UCI::_output_pool(std::make_unique<ThreadPool>(1));
#endif /* OUTPUT_POOL */
std::unique_ptr<ThreadPool> UCI::_compute_pool(std::make_unique<ThreadPool>(1));

std::atomic_bool UCI::_output_expected(false);

/** Estimate number of moves (not plies!) until mate. */
static INLINE int mate_distance(score_t score, const search::PV &pv)
{
    return std::copysign((std::max<int>(CHECKMATE - std::abs(score), pv.size()) + 1) / 2, score);
}

/** Info sent to the GUI. */
struct Info : public search::IterationInfo
{
    bool brief = false;
    const int eval_depth;
    const int hashfull;
    const int iteration;
    search::PV* const pv;
    static std::array<search::PV, PLY_MAX> pvs;

    Info(const search::Context& ctxt, const IterationInfo& info)
        : IterationInfo(info)
        , eval_depth(ctxt.get_tt()->_eval_depth)
        , hashfull(search::TranspositionTable::usage() * 10)
        , iteration(ctxt.iteration())
        , pv(&pvs[std::min<size_t>(pvs.size() - 1, iteration)])
    {
        constexpr auto TIME_LOW = 25; /* millisec */
        const auto time_limit = search::Context::time_limit();
        brief = (time_limit >= 0 && time_limit <= milliseconds + TIME_LOW);
        if (!brief)
        {
            const auto& ctxt_pv = ctxt.get_pv();
            if (!ctxt_pv.empty())
                pv->assign(ctxt_pv.begin() + 1, ctxt_pv.end());
        }
    }
};

/* Hold PVs for pending output tasks */
std::array<search::PV, PLY_MAX> Info::pvs;

static void INLINE output_info(std::ostream& out, const Info& info)
{
    g_out.clear();
    if (info.brief)
    {
        std::format_to(std::back_inserter(g_out), "info score cp {} depth {}", info.score, info.iteration);
        output(out, g_out);
    }
    else
    {
        constexpr auto MATE_DIST_MAX = 10;

        auto score_unit = "cp";
        auto score = info.score;
        if (std::abs(info.score) > CHECKMATE - MATE_DIST_MAX)
        {
            score_unit = "mate";
            score = mate_distance(info.score, *info.pv);
        }
        std::format_to(
            std::back_inserter(g_out),
            "info score {} {} depth {} seldepth {} time {} nodes {} nps {} hashfull {} pv ",
            score_unit,
            score,
            info.iteration,
            info.eval_depth,
            info.milliseconds,
            info.nodes,
            int(info.knps * 1000),
            info.hashfull);
        output(out, g_out);

        /* output PV */
        for (const auto &m : *info.pv)
        {
            const auto uci = m.uci();
            out.write(uci.data(), uci.size());
            out.write(" ", 1);
        }
    }
}

static void INLINE output_info(const Info& info)
{
    output_info(std::cout, info);
    std::cout << std::endl;

    if (_debug)
    {
        std::ostringstream out;
        output_info(out << "<<< ", info);
        log_debug(out.str());
    }
}


/* static */
void UCI::on_iteration(PyObject *, search::Context *ctxt, const search::IterationInfo *iter_info)
{
    if (ctxt && iter_info)
    {
    #if OUTPUT_POOL
        const Info info(*ctxt, *iter_info);
        _output_pool->push_task([info] {
            output_info(info);
        });
    #else
        output_info(Info(*ctxt, *iter_info));
    #endif /* OUTPUT_POOL */
    }
}


/* static */
void UCI::on_move(PyObject *, const std::string& move, int move_num)
{
    static std::string move_info; /* thread-safe: callback is called at root, on thread 0 only */
    move_info.clear();
    std::format_to(std::back_inserter(move_info), "info currmove {} currmovenumber {}", move, move_num);
    output(move_info);
}


void UCI::run()
{
    Arguments args;
    std::string cmd;

    while (true)
    {
        std::getline(std::cin, cmd);
        if (std::cin.fail() || std::cin.eof())
        {
            stop();
            break;
        }
        const auto nl = cmd.find_last_not_of("\n\r");
        if (nl != std::string::npos)
            cmd.erase(nl + 1);
        if (cmd.empty())
            continue;
        if (_debug)
            log_debug(std::format(">>> {}", cmd));

        args.clear();
        /* tokenize command */
        std::ranges::for_each(
            std::views::lazy_split(cmd, std::string_view(" ")),
            [&](auto const &tok)
            {
                if (!tok.empty())
                    args.emplace_back(std::string_view(&*tok.begin(), std::ranges::distance(tok)));
            });

        if (args.empty())
            continue;

        if (args.front() == "quit")
        {
            _output_expected = false;
            stop();
            output("info string good bye");
            break;
        }
        dispatch(cmd, args);
    }
}

INLINE void UCI::dispatch(std::string &cmd, const Arguments &args)
{
    ASSERT(!args.empty());
    const auto& tok = args.front();
    switch (tok[0])
    {
    case 'd':
        if (tok == "debug")
        {
            debug();
            return;
        }
        break;
    case 'g':
        if (tok == "go")
        {
            go(args);
            return;
        }
        break;
    case 'i':
        if (tok == "isready")
        {
            isready();
            return;
        }
        break;
    case 'p':
        if (tok == "position")
        {
            position(args);
            return;
        }
        if (tok == "ponderhit")
        {
            ponderhit();
            return;
        }
        break;
    case 's':
        if (tok == "setoption")
        {
            setoption(args);
            return;
        }
        if (tok == "stop")
        {
            stop();
            return;
        }
        break;
    case 'u':
        if (tok == "uci")
        {
            uci();
            return;
        }
        if (tok == "ucinewgame")
        {
            newgame();
            return;
        }
        break;
    }
    log_error("unknown command: " + cmd);
}

template <typename T>
INLINE const auto &next(const T &v, size_t &i)
{
    static typename T::value_type empty;
    return ++i < v.size() ? v[i] : empty;
}

void UCI::debug()
{
#if _WIN32
    bool use_unicode = false; /* cmd does not support unicode */
#else
    bool use_unicode = true;
#endif
    cython_wrapper::call(search::Context::_print_state, _buf._state, use_unicode);
    output(std::format("fen: {}", search::Context::epd(_buf._state)));
    output(std::format("hash: {}", _buf._state._hash));
    size_t history_size = 0;
    history_size = search::Context::_history->_positions.size();
    output(std::format("history_size: {}", history_size));
    std::ostringstream checkers;
    chess::for_each_square(_buf._state.checkers_mask(_buf._state.turn),
        [&checkers](chess::Square sq) {
            checkers << sq << " ";
        });
    output(std::format("checkers: {}", checkers.str()));
}

void UCI::go(const Arguments &args)
{
    stop();

    bool explicit_movetime = false, do_analysis = false, do_ponder = false;
    int movestogo = 40, movetime = 0;
    double time_remaining[] = {0, 0};
    int time_increments[] = {0, 0};

    auto turn = _buf._state.turn;

    _depth = max_depth;

    for (size_t i = 1; i < args.size(); ++i)
    {
        const auto &a = args[i];
        if (a == "depth")
        {
            _depth = to_int(next(args, i));
            do_analysis = true;
        }
        else if (a == "movetime")
        {
            movetime = to_int(next(args, i));
            explicit_movetime = true;
        }
        else if (a == "movestogo")
        {
            movestogo = to_int(next(args, i));
        }
        else if (a == "wtime")
        {
            time_remaining[chess::WHITE] = to_int(next(args, i));
        }
        else if (a == "btime")
        {
            time_remaining[chess::BLACK] = to_int(next(args, i));
        }
        else if (a == "winc")
        {
            time_increments[chess::WHITE] = to_int(next(args, i));
        }
        else if (a == "binc")
        {
            time_increments[chess::BLACK] = to_int(next(args, i));
        }
        else if (a == "ponder")
        {
            do_ponder = true;
        }
        else if (a == "infinite")
        {
            movetime = -1;
            do_analysis = true;
        }
    }
    /* initialize search context */
    auto ctxt = new (_buf.as_context()) search::Context();
    ctxt->_state = &_buf._state;

    if (!movetime)
        movetime = std::max<int>(0, time_remaining[turn] / std::max(movestogo, 40));
    LOG_DEBUG(std::format("movetime {}, movestogo {}", movetime, movestogo));

    _extended_time = 0;
    _output_expected = true;

    if (do_ponder)
    {
        _extended_time = std::max(1, movetime);
        ctxt->set_time_limit_ms(INFINITE);
        _compute_pool->push_task([this]{ ponder(); });
    }
    else if (do_analysis && !explicit_movetime)
    {
        ctxt->set_time_limit_ms(INFINITE);
        _compute_pool->push_task([this]{ search(); output_best_move(); });
    }
    else
    {
        if (_use_opening_book && _ply_count < _book_depth && !do_analysis)
        {
            LOG_DEBUG(std::format("lookup book_depth={}, ply_count={}", _book_depth, _ply_count));
            if (auto move = search::Context::_book_lookup(_buf._state, _best_book_move))
            {
                output_best_move(move);
                return;
            }
            else
                _book_depth = std::min(_book_depth, _ply_count);
        }
        ASSERT(!do_analysis);
        ASSERT(!do_ponder);

        const auto set_time_limit = [&, ctxt] {
            if (explicit_movetime)
            {
                search::Context::set_time_limit_ms(movetime);
            }
            else
            {
                search::TimeControl ctrl;
                ctrl.millisec[chess::BLACK] = time_remaining[chess::BLACK];
                ctrl.millisec[chess::WHITE] = time_remaining[chess::WHITE];
                ctrl.increments[chess::BLACK] = time_increments[chess::BLACK];
                ctrl.increments[chess::WHITE] = time_increments[chess::WHITE];
                ctrl.moves = movestogo;

                search::Context::set_start_time();
                ctxt->set_time_ctrl(ctrl, _score_delta);
            }
        };
    #if 0
        /* search asynchronously on the background thread */
        _compute_pool->push_task([this, movetime] {
            _score = search(set_time_limt);
            /* Do not request to ponder below 100 ms per move. */
            output_best_move(movetime >= 100);
        });
    #else
        /* search synchronously */
        _score = search(set_time_limit);
        /* Do not request to ponder below 100 ms per move. */
        output_best_move(movetime >= 100);
    #endif
    }
}

/**
 * This command must always be answered with "readyok" and can be sent also
 * when the engine is calculating in which case the engine should also immediately
 * answer with "readyok" without stopping the search.
 */
INLINE void UCI::isready()
{
    output("readyok");
}

void UCI::newgame()
{
    stop();
    search::TranspositionTable::clear_shared_hashtable();
    set_start_position();
    _book_depth = max_depth;
}

/**
 * Runs on a background thread with infinite time, and expects that:
 * either STOP is received; or
 * PONDERHIT is received, which extends the search by _extended_time,
 * then sets _extended_time to 0, to indicate to this function to send out
 * the best move when the search finishes.
 *
 * Pondering may finish before PONDERHIT is received, in which case
 * it resets _extended_time and does not output a move;
 *
 * the ponderhit handler will send out the best move instead, when PONDERHIT
 * is received (thus avoiding "premature bestmove in ponder" errors).
 */
void UCI::ponder()
{
    LOG_DEBUG(std::format("pondering, extended_time={}", _extended_time.load()));
    search();
    if (_extended_time)
        _extended_time = 0;
    else
        output_best_move();
}

void UCI::ponderhit()
{
    if (int ext = _extended_time)
    {
        _extended_time = 0;
        context().set_time_limit_ms(ext);
    }
    else
    {
        stop();
    }
}

void UCI::position(const Arguments &args)
{
    stop();

    bool in_moves = false;
    static Arguments fen, moves;

    fen.clear();
    moves.clear();
    search::Context::_history->clear();

    for (const auto &a : std::ranges::subrange(args.begin() + 1, args.end()))
    {
        if (a == "fen")
        {
            in_moves = false;
        }
        else if (a == "moves")
        {
            in_moves = true;
        }
        else if (a == "startpos")
        {
            set_start_position();
            in_moves = false;
        }
        else if (in_moves)
        {
            moves.push_back(a);
        }
        else
        {
            fen.push_back(a);
        }
    }
    if (fen.size() >= 4)
    {
        _buf._state = chess::State();
        if (   !chess::epd::parse_pos(fen[0], _buf._state)
            || !chess::epd::parse_side_to_move(fen[1], _buf._state)
            || !chess::epd::parse_castling(fen[2], _buf._state)
            || !chess::epd::parse_en_passant_target(fen[3], _buf._state)
           )
            raise_value_error("fen={} {} {} {}", fen[0], fen[1], fen[2], fen[3]);
    }
    else if (!fen.empty())
    {
        raise_value_error("invalid token count {}, expected 4", fen.size());
    }
    apply_moves(moves);
    LOG_DEBUG(search::Context::epd(_buf._state));
}

template<typename F>
INLINE score_t UCI::search(F set_time_limit)
{
    if (!search::Context::_history)
        search::Context::_history = std::make_unique<search::History>();

    _tt.init();

    auto& ctxt = context();
    ctxt.set_tt(&_tt);

    ctxt._algorithm = _algorithm;
    ctxt._max_depth = 1;
    ctxt._move = _last_move;

    set_time_limit();
    const auto score = search::iterative(ctxt, _tt, _depth + 1);

    _score_delta = score - _score;
#if 0
    if (_score_delta < -50)
        search::Context::log_message(LogLevel::INFO, std::format("score drop from {} to {}", _score, score));
#endif
    return score;
}

void UCI::setoption(const Arguments &args)
{
    Arguments name, value, *acc = nullptr;

    for (const auto &a : std::ranges::subrange(args.begin() + 1, args.end()))
    {
        if (a == "name")
            acc = &name;
        else if (a == "value")
            acc = &value;
        else if (acc)
            acc->emplace_back(a);
    }

    auto opt_name = join(" ", name);
    auto iter = _options.find(lowercase(opt_name));
    if (iter != _options.end())
        iter->second->set(join(" ", value));
    else
        log_warning(__func__ + (": \"" + opt_name + "\": not found"));
}

void UCI::stop()
{
    search::Context::set_time_limit_ms(0);
    _compute_pool->wait_for_tasks([] { search::Context::cancel(); });
#if OUTPUT_POOL
    _output_pool->wait_for_tasks();
#endif /* OUTOUT_POOL */
    output_best_move<true>();
}

void UCI::uci()
{
    std::ios_base::sync_with_stdio(false);
    output<false>(std::format("id name {}-{}", _name, _version));
    output<false>("id author Cristian Vlasceanu");

    refresh_options();

    /* show available options */
    for (const auto &opt : _options)
    {
        std::ostringstream opts;
        opt.second->print(opts << "option name ");
        output<false>(opts.str());
    }
    output("uciok");
}

extern "C" void run_uci_loop(const char *name, const char *version, bool debug)
{
    _debug = debug;
    std::string err;
    try
    {
        UCI uci(name, version);
        uci.run();
    }
    catch (const std::exception &e)
    {
        err = e.what();
    }
    catch (...)
    {
        err = "unknown exception";
    }
    if (!err.empty())
        raise_runtime_error(err.c_str());
}
#else
extern "C" void run_uci_loop(const char *, const char *, bool)
{
    raise_runtime_error("Native UCI implementation is not enabled.");
}
#endif /* NATIVE_UCI */
