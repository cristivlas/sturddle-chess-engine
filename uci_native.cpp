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
#if NATIVE_UCI
#include <format>
#include <memory>
#include <ranges>
#include <string>
#include <sstream>
#include <unordered_map>
#include <vector>
#include "thread_pool.hpp" /* pondering, go infinite */

static constexpr std::string_view START_POS{"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"};

template <typename T>
static void log_debug(T msg)
{
    search::Context::log_message(LogLevel::DEBUG, std::to_string(msg));
}

template <typename T>
static void log_warning(T warn)
{
    search::Context::log_message(LogLevel::WARN, std::to_string(warn));
}

template <typename T>
static INLINE T &lowercase(T &s)
{
    std::transform(s.begin(), s.end(), s.begin(), [](auto c)
                   { return std::tolower(c); });
    return s;
}

template <typename T>
static INLINE std::string join(std::string_view sep, const T &v)
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
    template <typename T>
    struct arity
    {
    };

    template <typename R, typename C, typename... Args>
    struct arity<R (C::*)(Args...)> : std::integral_constant<unsigned, sizeof...(Args)>
    {
    };

    template <typename R, typename C, typename... Args>
    struct arity<R (C::*)(Args...) const> : std::integral_constant<unsigned, sizeof...(Args)>
    {
    };

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

    template <typename T>
    INLINE int to_int(T v)
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

        void set(std::string_view value) override
        {
            _set_param(_name, to_int(value));
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
        : _name(name), _version(version)
    {
        set_start_position();

        /* callbacks */
        search::Context::_on_iter = on_iteration;

        _options.emplace("ownbook", std::make_shared<OptionBool>("OwnBook", _use_opening_book));
        _options.emplace("ponder", std::make_shared<OptionBool>("Ponder", _ponder));

        /** Options TODO: */
        /* Algorithm */
        /* Best Opening */
        /* EvalFile */
        /* SyzygyPath */
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
    static void on_iteration(PyObject*, search::Context*, const search::IterationInfo*);

private:
    /** impl details */
    template <bool debug = true>
    static void output(const std::string_view out)
    {
        std::cout << out << std::endl;
        if (debug && _debug)
            log_debug(out);
    }

    void output_best(const chess::BaseMove& move, bool request_ponder)
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
                    const auto& pv = _tt.get_pv();
                    if (pv.size() > 1 && pv[0] == move)
                        out << " ponder " << pv[1].uci();
                }
                output(out.str());
            }
        }
    }

    template <typename F>
    INLINE void Invoke(const std::string &cmd, F f, const Arguments &args)
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
    }

    /** iterative deepening search */
    score_t search();

    search::Algorithm _algorithm = search::Algorithm::MTDF;
    search::ContextBuffer _buf;
    search::TranspositionTable _tt;
    static bool _debug;
    bool _output_expected = false;
    bool _ponder = false;
    bool _use_opening_book = false;
    int _depth = max_depth;
    score_t _score = 0;
    EngineOptions _options;
    const std::string _name;
    const std::string _version; /* engine version */
    std::unique_ptr<ThreadPool> _pool;
};

bool UCI::_debug = false;


/* static */
void UCI::on_iteration(PyObject*, search::Context* ctxt, const search::IterationInfo* info)
{
    std::ostringstream out;

    /* TODO seldepth accuracy */
    out << "info depth " << ctxt->iteration() << " seldepth " << ctxt->get_tt()->_eval_depth;

    /* TODO: mate distance. */
    out << " score cp " << info->score;

    out << " time " << info->milliseconds;
    out << " nodes " << info->nodes << " knps " << int(info->knps * 1000);
    out << " hashfull " << int(search::TranspositionTable::usage() * 10);

    out << " pv ";
    for (const auto& m : std::ranges::subrange(ctxt->get_pv().begin() + 1, ctxt->get_pv().end()))
        out << m.uci() << " ";

    output(out.str());
}

void UCI::run()
{
    std::string cmd;
    while (true)
    {
        std::getline(std::cin, cmd);
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
            Invoke(cmd, &UCI::go, args);
            break;
        case Command::ISREADY:
            Invoke(cmd, &UCI::isready, args);
            break;
        case Command::PONDERHIT:
            Invoke(cmd, &UCI::ponderhit, args);
            break;
        case Command::POSITION:
            Invoke(cmd, &UCI::position, args);
            break;
        case Command::SETOPTION:
            Invoke(cmd, &UCI::setoption, args);
            break;
        case Command::STOP:
            Invoke(cmd, &UCI::stop, args);
            break;
        case Command::UCI:
            Invoke(cmd, &UCI::uci, args);
            break;
        case Command::UCINEWGAME:
            Invoke(cmd, &UCI::newgame, args);
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

    bool explicit_movetime = false, analysis = false, ponder = false;
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
            analysis = true;
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
            ASSERT_ALWAYS(_ponder);
            ponder = true;
        }
        else if (a == "infinite")
        {
            movetime = -1;
            analysis = true;
        }
    }
    /* initialize search context */
    auto ctxt = new (_buf.as_context()) search::Context();
    ctxt->_state = &_buf._state;

    if (!movetime)
        movetime = time_remaining[turn] / std::max(movestogo, 40);
    log_debug("movetime=" + std::to_string(movetime));

    _output_expected = true;

    if (ponder)
    {
        log_error("TODO: support pondering!");
    }
    else if (analysis)
    {
        ctxt->set_time_limit_ms(-1);
        if (!_pool)
            _pool = std::make_unique<ThreadPool>(1);
        _pool->push_task([this] { search(); });
    }
    else
    {
        ASSERT_ALWAYS(!_pool || !_pool->tasks_pending());
        if (_use_opening_book)
            log_error("TODO: support opening book!");

        ctxt->set_time_limit_ms(movetime);
        if (!explicit_movetime)
            ctxt->set_time_info(time_remaining[turn], movestogo, _score);

        /* search synchronously */
        _score = search();
        /* Do not ponder below 1s per move. */
        output_best(ctxt->_best_move, movetime >= 1000);
    }
}

INLINE void UCI::isready()
{
    stop();
    output("readyok");
}

void UCI::newgame()
{
    stop();
    search::TranspositionTable::clear_shared_hashtable();
    set_start_position();
}

void UCI::ponderhit()
{
    /* TODO */
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

    for (const auto &move : moves)
        if (move.size() >= 4)
        {
            chess::Square from, to;

            if (chess::parse_square(move, from) && chess::parse_square(std::string_view(&move[2], 2), to))
            {
                auto promo = move.size() > 4 ? chess::piece_type(move[4]) : chess::PieceType::NONE;
                _buf._state.apply_move(chess::BaseMove(from, to, promo));
                /* TODO set up history */
            }
        }
    if (_debug)
        log_debug(search::Context::epd(_buf._state));
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

score_t UCI::search()
{
    _tt.init();

    auto ctxt = _buf.as_context();
    ctxt->_algorithm = _algorithm;
    ctxt->_max_depth = 1;
    ctxt->set_tt(&_tt);

    return search::iterative(*ctxt, _tt, _depth + 1);
}

void UCI::stop()
{
    if (_pool && _pool->tasks_pending())
    {
        search::Context::cancel();
        _pool->wait_for_tasks();
        ASSERT_ALWAYS(!_pool->tasks_pending());
    }
    output_best(_buf.as_context()->_best_move, false /* ponder */);
}

void UCI::uci()
{
    output<false>(std::format("id name {}-{}", _name, _version));

    /* refresh options */
    for (auto p : _get_param_info())
    {
        auto name = p.first;
        /* option names are case insensitive, and can contain _single_ spaces */
        _options[lowercase(name)] = std::make_shared<OptionParam>(p.first, p.second);
    }
    /* show available options */
    std::ostringstream opts;
    for (const auto &opt : _options)
        opt.second->print(opts << "\noption ");
    output<false>(opts.str());
}

extern "C" void run_uci_loop(const char *name, const char *version)
{
    std::string err;
    try
    {
        UCI uci(name, version);
        uci.run();
    }
    catch (const std::exception &e)
    {
        try
        {
            err = e.what();
        }
        catch (...)
        {
        }
    }
    catch (...)
    {
        try
        {
            err = "unknown exception";
        }
        catch (...)
        {
        }
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
