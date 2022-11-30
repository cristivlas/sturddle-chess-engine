/** Native C++ UCI */
/** http://wbec-ridderkerk.nl/html/UCIProtocol.html */
#include <iostream>
#include <string_view>
#include "context.h"

namespace std
{
    INLINE std::string to_string(std::string_view v)
    {
        return std::string(v);
    }
}
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
#if NATIVE_UCI /* experimental */
#include <cmath>
#include <format>
#include <memory>
#include <ranges>
#include <string>
#include <sstream>
#include <unordered_map>
#include <vector>
#include "nnue.h"
#include "thread_pool.hpp" /* pondering, go infinite */

static constexpr std::string_view START_POS{"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"};
static bool _debug = false;

template <typename T> static void log_debug(T msg)
{
    if (_debug)
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

enum class Command
{
    NONE,
    GO,
    ISREADY,
    PONDERHIT,
    POSITION,
    SETOPTION,
    STOP,
    UCI,
    UCINEWGAME,
};

static std::unordered_map<std::string_view, Command> commands{
    {"go", Command::GO},
    {"isready", Command::ISREADY},
    {"ponderhit", Command::PONDERHIT},
    {"position", Command::POSITION},
    {"setoption", Command::SETOPTION},
    {"stop", Command::STOP},
    {"uci", Command::UCI},
    {"ucinewgame", Command::UCINEWGAME},
};

namespace
{
    /*
    https://stackoverflow.com/questions/27866909/get-function-arity-from-template-parameter
    */
    template <typename T> struct arity {};

    template <typename R, typename C, typename... Args>
    struct arity<R (C::*)(Args...)> : std::integral_constant<unsigned, sizeof...(Args)> {};

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

    template <typename T> INLINE int to_int(T v)
    {
        return std::stoi(std::string(v));
    }

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

        OptionParam(const std::string &name, const Param &param) : OptionBase(name), _p(param)
        {
        }

        void print(std::ostream &out) const override
        {
            OptionBase::print(out);
            if (_p.min_val == 0 && _p.max_val == 1)
                out << "type check default " << std::boolalpha << bool(_p.val);
            else
                out << "type spin default " << _p.val << " min " << _p.min_val << " max " << _p.max_val;
        }

        void set(std::string_view value) override { _set_param(_name, to_int(value)); }
    };

    struct OptionEvalFile : public OptionBase
    {
        std::string &_eval_file;
        OptionEvalFile(std::string& eval_file) : OptionBase("EvalFile"), _eval_file(eval_file) {}

        void print(std::ostream& out) const override
        {
            OptionBase::print(out);
            out << "type string default " << _eval_file;
        }

        void set(std::string_view value) override
        {
            if (nnue_init(std::string(value).c_str()))
            {
                _eval_file = value;
            }
            else
            {
                log_error(std::format("invalid NNUE file: {}", value));
                _exit(-1); /* refuse to run without valid NNUE */
            }
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

class UCI
{
    using Arguments = std::vector<std::string_view>;
    using EngineOptions = std::unordered_map<std::string, std::shared_ptr<Option>>;
    using ThreadPool = thread_pool<int>;

    static constexpr int max_depth = 100;

public:
    UCI(const std::string &name, const std::string &version)
        : _name(name)
        , _version(version)
        , _use_opening_book(search::Context::_book_init(_book))
    {
        set_start_position();

        search::Context::_on_iter = on_iteration;

        _options.emplace("best opening", std::make_shared<OptionBool>("Best Opening", _best_book_move));
        _options.emplace("debug", std::make_shared<OptionBool>("Debug", _debug));
        _options.emplace("ownbook", std::make_shared<OptionBool>("OwnBook", _use_opening_book));
        _options.emplace("ponder", std::make_shared<OptionBool>("Ponder", _ponder));
        _options.emplace("evalfile", std::make_shared<OptionEvalFile>(_eval_file));
        _options.emplace("syzygypath", std::make_shared<OptionSyzygy>());

        /* Options TODO: Algorithm, Opening Book */
    }

    void run();

private:
    void dispatch(const std::string &, const Arguments &args);

    /** UCI commands */
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

private:
    /** position() helper */
    void apply_moves(const Arguments &moves)
    {
        _last_move = chess::BaseMove();
        _ply_count = 0;

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
                    search::Context::_history->insert(_buf._state);
                    _last_move = move;
                    ++_ply_count;
                }
            }
    }

    ThreadPool &background()
    {
        if (!_pool)
            _pool = std::make_unique<ThreadPool>(1);
        return *_pool;
    }

    INLINE search::Context &context() { return *_buf.as_context(); }

    template <bool flush=true> static void output(const std::string_view out)
    {
        std::cout << out << "\n";
        log_debug(std::format("<<< {}", out));
        if constexpr(flush)
            std::cout << std::flush;
    }

    void output_best(const chess::BaseMove &move, bool request_ponder)
    {
        if (_output_expected)
        {
            _output_expected = false;
            if (!move)
            {
                output("resign");
            }
            else
            {
                std::ostringstream out;
                out << "bestmove " << move.uci();
                if (_ponder && request_ponder)
                {
                    const auto &pv = _tt.get_pv();
                    if (pv.size() > 2 && pv[1] == move)
                        out << " ponder " << pv[2].uci();
                }
                output(out.str());
            }
        }
    }

    template <typename F>
    INLINE void invoke(const std::string &cmd, F f, const Arguments &args)
    {
        try
        {
            if constexpr (arity<F>{} == 0)
            {
                if (args.size() > 1)
                    log_warning("extraneous arguments: " + cmd);
                (this->*f)();
            }
            else
            {
                (this->*f)(args);
            }
        }
        catch (const std::exception &e)
        {
            log_error(e.what());
        }
    }

    void set_start_position()
    {
        _buf._state = chess::State();
        _buf._state.castling_rights = chess::BB_DEFAULT_CASTLING_RIGHTS;
        chess::epd::parse_pos(START_POS, _buf._state);
        _buf._state.rehash();
    }

    /** think on opponent's time */
    void ponder();

    /** iterative deepening search */
    score_t search();

    search::Algorithm _algorithm = search::Algorithm::MTDF;
    search::ContextBuffer _buf;
    search::TranspositionTable _tt;
    std::string _book = "book.bin";
    std::string _eval_file = NNUE_EVAL_FILE;
    std::atomic_int _extended_time = 0; /* for pondering */
    int _book_depth = max_depth;
    int _depth = max_depth;
    int _ply_count =0;
    score_t _score = 0;
    EngineOptions _options;
    const std::string _name;
    const std::string _version; /* engine version */
    std::unique_ptr<ThreadPool> _pool;
    bool _output_expected = false;
    bool _ponder = false;
    bool _use_opening_book = false;
    bool _best_book_move = false;
    chess::BaseMove _last_move;
};

/** Estimate number of moves (not plies!) until mate. */
static INLINE int mate_distance(score_t score, const search::PV &pv)
{
    return std::copysign((std::max<int>(CHECKMATE - std::abs(score), pv.size()) + 1) / 2, score);
}

/* static */
void UCI::on_iteration(PyObject *, search::Context *ctxt, const search::IterationInfo *info)
{
    std::ostringstream out;

    out << "info depth " << ctxt->iteration() << " seldepth " << ctxt->get_tt()->_eval_depth;
    out << " score cp " << info->score;

    if (std::abs(info->score) > MATE_HIGH)
        out << " mate " << mate_distance(info->score, ctxt->get_pv());

    out << " time " << info->milliseconds;
    out << " nodes " << info->nodes << " knps " << int(info->knps * 1000);
    out << " hashfull " << int(search::TranspositionTable::usage() * 10);

    out << " pv ";
    for (const auto &m : std::ranges::subrange(ctxt->get_pv().begin() + 1, ctxt->get_pv().end()))
        out << m.uci() << " ";

    output(out.str());
}

void UCI::run()
{
    std::string cmd;
    while (true)
    {
        std::getline(std::cin, cmd);
        const auto nl = cmd.find_last_not_of("\n\r");
        if (nl != std::string::npos)
            cmd.erase(nl + 1);
        if (cmd.empty())
            continue;
        log_debug(std::format(">>> {}", cmd));
        lowercase(cmd);
        if (cmd == "quit")
            break;

        Arguments args;
        /* tokenize command */
        std::ranges::for_each(
            std::views::lazy_split(cmd, std::string_view(" ")),
            [&](auto const &tok)
            {
                if (!tok.empty())
                    args.emplace_back(std::string_view(&*tok.begin(), std::ranges::distance(tok)));
            });

        if (!args.empty())
            dispatch(cmd, args);
    }
}

void UCI::dispatch(const std::string &cmd, const Arguments &args)
{
    ASSERT(!args.empty());
    const auto iter = commands.find(args.front());
    if (iter == commands.end())
    {
        log_error("unknown command: " + cmd);
    }
    else
    {
        switch (iter->second)
        {
        case Command::GO:
            invoke(cmd, &UCI::go, args);
            break;
        case Command::ISREADY:
            invoke(cmd, &UCI::isready, args);
            break;
        case Command::PONDERHIT:
            invoke(cmd, &UCI::ponderhit, args);
            break;
        case Command::POSITION:
            invoke(cmd, &UCI::position, args);
            break;
        case Command::SETOPTION:
            invoke(cmd, &UCI::setoption, args);
            break;
        case Command::STOP:
            invoke(cmd, &UCI::stop, args);
            break;
        case Command::UCI:
            invoke(cmd, &UCI::uci, args);
            break;
        case Command::UCINEWGAME:
            invoke(cmd, &UCI::newgame, args);
            break;
        default:
            break;
        }
    }
}

template <typename T>
INLINE const auto &next(const T &v, size_t &i)
{
    static typename T::value_type empty;
    return ++i < v.size() ? v[i] : empty;
}

void UCI::go(const Arguments &args)
{
    stop();

    bool explicit_movetime = false, do_analysis = false, do_ponder = false;
    int movestogo = 40, movetime = 0;
    int time_remaining[] = {0, 0};
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
        else if (a == "ponder")
        {
            do_ponder = _ponder;
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
        movetime = time_remaining[turn] / std::max(movestogo, 40);
    log_debug(std::format("movetime {}, movestogo {}", movetime, movestogo));

    _extended_time = 0;
    _output_expected = true;

    if (do_ponder)
    {
        _extended_time = std::max(1, movetime);
        background().push_task([this]{ ponder(); });
    }
    else if (do_analysis)
    {
        ctxt->set_time_limit_ms(-1);
        background().push_task([this]{ search(); });
    }
    else
    {
        if (_use_opening_book && _ply_count < _book_depth)
        {
            log_debug(std::format("lookup book_depth={}, ply_count={}", _book_depth, _ply_count));
            if (auto move = search::Context::_book_lookup(_buf._state, _best_book_move))
            {
                output_best(move, false);
                return;
            }
            else
                _book_depth = _ply_count;
        }
        ctxt->set_time_limit_ms(movetime);
        if (!explicit_movetime)
            ctxt->set_time_info(time_remaining[turn], movestogo, _score);

        /* search synchronously */
        _score = search();
        /* Do not ponder below 1s per move. */
        output_best(ctxt->_best_move, movetime >= 1000);
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
    log_debug(std::format("pondering, extended_time={}", _extended_time.load()));

    context().set_time_limit_ms(-1 /* infinite */);
    search();
    if (_extended_time)
        _extended_time = 0;
    else
        output_best(context()._best_move, false);
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
    Arguments fen, moves;

    for (const auto &a : args)
    {
        if (a == "fen")
        {
            in_moves = false;
        }
        else if (a == "moves")
        {
            in_moves = true;
            search::Context::_history = std::make_unique<search::History>();
        }
        else if (a == "startpos")
        {
            set_start_position();
            in_moves = false;
        }
        else if (in_moves)
        {
            moves.emplace_back(a);
        }
        else
        {
            fen.emplace_back(a);
        }
    }
    if (fen.size() >= 4)
    {
        _buf._state = chess::State();
        chess::epd::parse_pos(fen[0], _buf._state);
        chess::epd::parse_side_to_move(fen[1], _buf._state);
        chess::epd::parse_castling(fen[2], _buf._state);
        chess::epd::parse_en_passant_target(fen[3], _buf._state);
    }
    apply_moves(moves);
    log_debug(search::Context::epd(_buf._state));
}

score_t UCI::search()
{
    if (!search::Context::_history)
        search::Context::_history = std::make_unique<search::History>();

    _tt.init();

    auto& ctxt = context();
    ctxt.set_tt(&_tt);

    ctxt._algorithm = _algorithm;
    ctxt._max_depth = 1;
    ctxt._move = _last_move;
    ctxt._prev = chess::BaseMove();

    return search::iterative(ctxt, _tt, _depth + 1);
}

void UCI::setoption(const Arguments &args)
{
    stop();
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

    const auto opt_name = join(" ", name);
    auto iter = _options.find(opt_name);
    if (iter != _options.end())
        iter->second->set(join(" ", value));
    else
        log_warning(__func__ + (": " + opt_name + ": not found"));
}

void UCI::stop()
{
    if (_pool && _pool->tasks_pending())
    {
        search::Context::cancel();
        _pool->wait_for_tasks();
        ASSERT_ALWAYS(!_pool->tasks_pending());
    }
    output_best(context()._best_move, false);
}

void UCI::uci()
{
    std::ios_base::sync_with_stdio(false);
    output<false>(std::format("id name {}-{}", _name, _version));
    output<false>("id author Cristi Vlasceanu");

    /* refresh options */
    for (auto p : _get_param_info())
    {
        auto name = p.first;
        /* option names are case insensitive, and can contain _single_ spaces */
        _options[lowercase(name)] = std::make_shared<OptionParam>(p.first, p.second);
    }
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
        log_error(err);
}
#else
extern "C" void run_uci_loop(const char *name, const char *version)
{
    log_error("Native UCI implementation is not enabled.");
}
#endif /* NATIVE_UCI */
