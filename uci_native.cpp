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

    class OptionParam : public Option
    {
        const std::string _name;
        const Param _p;

    public:
        OptionParam(const std::string &name, const Param &param) : _name(name), _p(param) {}

        void print(std::ostream &out) const override
        {
            out << _name << " ";

            if (_p.min_val == 0 && _p.max_val == 1)
                out << "type check default " << std::boolalpha << bool(_p.val);
            else
                out << "type spin default " << _p.val << " min " << _p.min_val << " max " << _p.max_val;
        }

        void set(std::string_view value) override
        {
            _set_param(_name, std::stoi(std::string(value)));
        }
    };
}

class UCI
{
    using Arguments = std::vector<std::string_view>;
    using EngineOptions = std::unordered_map<std::string, std::shared_ptr<Option>>;

public:
    UCI(const std::string &name, const std::string &version)
        : _name(name), _version(version)
    {
        _buf.as_context()->_state = &_buf._state;
        _buf.as_context()->_algorithm = search::Algorithm::MTDF;
    }

    void run();

private:
    void dispatch(const std::string &, const Arguments &args);

    /** UCI commands */
    void go(const Arguments &args);
    void isready() const;
    void ponderhit();
    void position(const Arguments &args);
    void setoption(const Arguments &args);
    void stop();
    void uci();
    void newgame();

private:
    /** impl details */
    template <bool debug = true>
    void output(const std::string_view out) const
    {
        std::cout << out << std::endl;
        if (debug && _debug)
            log_debug(out);
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

    void search();

    search::ContextBuffer _buf;
    search::TranspositionTable _tt;
    bool _debug = true;
    EngineOptions _options;
    const std::string _name;
    const std::string _version;
};

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

void UCI::go(const Arguments &args)
{
    // TODO
}

INLINE void UCI::isready() const
{
    output("readyok");
}

void UCI::newgame()
{
    // TODO
}

void UCI::ponderhit()
{
    // TODO
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
            _buf._state = chess::State();
            _buf._state.castling_rights = chess::BB_DEFAULT_CASTLING_RIGHTS;
            chess::epd::parse_pos(START_POS, _buf._state);
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
            }
        }
    if (_debug)
        log_debug(search::Context::epd(_buf._state));
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

    auto iter = _options.find(join(" ", name));
    if (iter != _options.end())
        iter->second->set(join(" ", value));
}

void UCI::search()
{
    // TODO
}

void UCI::stop()
{
    // TODO
}

void UCI::uci()
{
    output<false>(std::format("id name {}-{}", _name, _version));

    /* refresh options */
    _options.clear();
    /* option names are case insensitive, and can contain _single_ spaces */
    for (auto p : _get_param_info())
    {
        auto name = p.first;
        _options.emplace(lowercase(name), new OptionParam(p.first, p.second));
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
