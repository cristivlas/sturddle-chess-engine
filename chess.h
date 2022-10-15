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
/*
 * Core Chess support data structures and routines.
 *
 * Parts inspired and ported from:
 * python-chess (C) Niklas Fiekas (https://python-chess.readthedocs.io/en/latest/)
*/
#include <array>
#include <cmath>
#include <cstdint>
#include <functional>
#include <utility>
#include <string>
#include <type_traits>
#include <vector>
#if _MSC_VER
#include <intrin.h>
#pragma intrinsic(_BitScanForward64)
#pragma intrinsic(_BitScanReverse64)
#pragma warning(disable: 4146)
#endif
#include "common.h"

#include "attack_tables.h"

#if USE_LIBPOPCOUNT
#include "libpopcnt.h"
#endif

#if USE_MAGIC_BITS
#include "magic_bits.hpp"
extern const magic_bits::Attacks magic_bits_attacks;
#endif /* USE_MAGIC_BITS */

#include "tables.h"

#if !defined (M_E)
static constexpr auto M_E = 2.718281828459045;
#endif

constexpr int SIGN[] = { -1, 1 };


template<typename T> INLINE T constexpr pow2(T x) { return x * x; }

namespace chess
{
    using Bitboard = uint64_t;

    using AttackMasks = std::array<Bitboard, 64>;
    using RaysRow = std::array<chess::Bitboard, 64>;
    using Rays = std::array<RaysRow, 64>;

    constexpr Bitboard BB_ALL = ~0ULL;
    constexpr Bitboard BB_EMPTY = 0ULL;
    constexpr Bitboard BB_CENTER = (0x3ULL << 35) | (0x3ULL << 27);

    extern AttackMasks BB_DIAG_MASKS, BB_FILE_MASKS, BB_RANK_MASKS;

    template<std::size_t... I>
    static constexpr std::array<uint64_t, sizeof ... (I)> bb_squares(std::index_sequence<I...>)
    {
        return { 1ULL << I ... };
    }

    static constexpr auto BB_SQUARES = bb_squares(std::make_index_sequence<64>{});

    static constexpr Bitboard BB_LIGHT_SQUARES = 0x55aa55aa55aa55aaULL;
    static constexpr Bitboard BB_DARK_SQUARES  = 0xaa55aa55aa55aa55ULL;

    extern Bitboard BB_KING_ATTACKS[64];
    extern Bitboard BB_KNIGHT_ATTACKS[64];
    extern Bitboard BB_PAWN_ATTACKS[2][64];

    template<int N> struct Rank
    {
        static constexpr uint64_t mask = 0xffULL << (8 * N);
    };

    constexpr Bitboard BB_RANK_1 = Rank<0>::mask;
    constexpr Bitboard BB_RANK_2 = Rank<1>::mask;
    constexpr Bitboard BB_RANK_3 = Rank<2>::mask;
    constexpr Bitboard BB_RANK_4 = Rank<3>::mask;
    constexpr Bitboard BB_RANK_5 = Rank<4>::mask;
    constexpr Bitboard BB_RANK_6 = Rank<5>::mask;
    constexpr Bitboard BB_RANK_7 = Rank<6>::mask;
    constexpr Bitboard BB_RANK_8 = Rank<7>::mask;


    constexpr Bitboard BB_RANKS[8] = {
        BB_RANK_1,
        BB_RANK_2,
        BB_RANK_3,
        BB_RANK_4,
        BB_RANK_5,
        BB_RANK_6,
        BB_RANK_7,
        BB_RANK_8,
    };

    constexpr Bitboard BB_BACKRANKS[2] = { BB_RANK_8, BB_RANK_1 };


    template<int N> struct File
    {
        static constexpr uint64_t mask = 0x0101010101010101ULL << N;
    };


    constexpr Bitboard BB_FILE_A = File<0>::mask;
    constexpr Bitboard BB_FILE_B = File<1>::mask;
    constexpr Bitboard BB_FILE_C = File<2>::mask;
    constexpr Bitboard BB_FILE_D = File<3>::mask;
    constexpr Bitboard BB_FILE_E = File<4>::mask;
    constexpr Bitboard BB_FILE_F = File<5>::mask;
    constexpr Bitboard BB_FILE_G = File<6>::mask;
    constexpr Bitboard BB_FILE_H = File<7>::mask;


    constexpr Bitboard BB_FILES[8] = {
        BB_FILE_A,
        BB_FILE_B,
        BB_FILE_C,
        BB_FILE_D,
        BB_FILE_E,
        BB_FILE_F,
        BB_FILE_G,
        BB_FILE_H,
    };

    constexpr Bitboard bb_neighbour_files_mask[8] = {
        BB_FILE_B,
        BB_FILE_A | BB_FILE_C,
        BB_FILE_B | BB_FILE_D,
        BB_FILE_C | BB_FILE_E,
        BB_FILE_D | BB_FILE_F,
        BB_FILE_E | BB_FILE_G,
        BB_FILE_F | BB_FILE_H,
        BB_FILE_G
    };


    /* Quadrants */
    constexpr Bitboard BB_NW = 0x0F0F0F0F00000000ULL;
    constexpr Bitboard BB_NE = 0xF0F0F0F000000000ULL;
    constexpr Bitboard BB_SW = 0x0F0F0F0FULL;
    constexpr Bitboard BB_SE = 0xF0F0F0F0ULL;

    constexpr Bitboard BB_QUANDRANTS[4] = { BB_NW, BB_NE, BB_SW, BB_SE, };

    constexpr Bitboard BB_PASSED[2] = {
        /* BB_RANK_4 | */ BB_RANK_3 | BB_RANK_2,
        /* BB_RANK_5 | */ BB_RANK_6 | BB_RANK_7
    };

    extern Rays BB_RAYS;

    enum PieceType : int8_t
    {
        NONE = 0,
        PAWN = 1,
        KNIGHT = 2,
        BISHOP = 3,
        ROOK = 4,
        QUEEN = 5,
        KING = 6,
    };

    INLINE bool operator !(PieceType type) { return type == PieceType::NONE; }


    constexpr PieceType PIECES[6] =
    {
        PieceType::PAWN,
        PieceType::KNIGHT,
        PieceType::BISHOP,
        PieceType::ROOK,
        PieceType::QUEEN,
        PieceType::KING,
    };

    constexpr const char* PIECE_SYMBOL[] = { "", "p", "n", "b", "r", "q", "k" };

    enum Color : int8_t
    {
        BLACK = 0,
        WHITE = 1,
    };

    INLINE constexpr Color operator !(Color color)
    {
        return static_cast<Color>(!static_cast<bool>(color));
    }

    INLINE void flip(Color& color)
    {
        color = !color;
    }


    enum Square : int8_t
    {
        A1, B1, C1, D1, E1, F1, G1, H1,
        A2, B2, C2, D2, E2, F2, G2, H2,
        A3, B3, C3, D3, E3, F3, G3, H3,
        A4, B4, C4, D4, E4, F4, G4, H4,
        A5, B5, C5, D5, E5, F5, G5, H5,
        A6, B6, C6, D6, E6, F6, G6, H6,
        A7, B7, C7, D7, E7, F7, G7, H7,
        A8, B8, C8, D8, E8, F8, G8, H8,

        UNDEFINED = -1,
    };


//#define DEFAULT_MOBILITY_WEIGHTS { 0, 3, 2, 2, 1, 3, 2 }
//#define DEFAULT_MOBILITY_WEIGHTS { 0, 5, 1, 8, 7, 4, 1 }
#define DEFAULT_MOBILITY_WEIGHTS { 0, 0, 0, 7, 6, 5, 0 }


#if MOBILITY_TUNING_ENABLED
    extern int MOBILITY[7];
#else
    static constexpr int MOBILITY[7] = DEFAULT_MOBILITY_WEIGHTS;
#endif

    /* Piece values */
    constexpr int WEIGHT[] = { 0, 100, 325, 325, 500, 975, 20000 };


    /*
     * BitBoard and Square utilities
     */

    INLINE int popcount(uint64_t u)
    {
    #if USE_LIBPOPCOUNT
        /* Evals break if results are converted to unsigned! */
        return int(popcnt64(u));
    #elif _MSC_VER
        return int(__popcnt64(u));
    #else
        static_assert(std::is_same<decltype(__builtin_popcount(0)), int>::value);
        return __builtin_popcount(u);
    #endif
    }


    INLINE int msb(uint64_t v)
    {
        ASSERT(v);
#if _MSC_VER
        unsigned long index = 0;

        _BitScanReverse64(&index, v);
        return int(index);

#elif (__GNUC__)
        return (63 - __builtin_clzll(v));
#else
    #error "unsupported"
#endif /* !__GNUC__ */
    }


    INLINE int lsb(uint64_t u)
    {
        ASSERT(u);
#if (__GNUC__)
        return __builtin_ctzll(u);

#elif (_MSC_VER)
        unsigned long index = 0;

        _BitScanForward64(&index, u);
        return int(index);
#else
    #error "unsupported"
#endif /* _MSC_VER */
    }


    std::vector<int> scan_forward(Bitboard);


    INLINE constexpr int square_file(int square)
    {
        return square & 7;
    }

    INLINE constexpr int square_rank(int square)
    {
        return square >> 3;
    }

    INLINE int square_distance(int a, int b)
    {
        return std::max(abs(square_file(a) - square_file(b)), abs(square_rank(a) - square_rank(b)));
    }

    /*
     * https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating
     */
    INLINE constexpr int square_mirror(int square)
    {
        return square ^ 0x38;
    }

    std::string square_name(Square);


    template<typename F> constexpr void for_each_square(Bitboard bb, F f)
    {
        while (bb)
        {
            const auto i = msb(bb);
            bb &= ~(1ULL << i);

            f(static_cast<Square>(i));
        }
    }


    template<typename T, typename F> constexpr T for_each_square_r(Bitboard bb, F f)
    {
        while (bb)
        {
            const auto temp = bb & -bb;

            if (auto r = f(static_cast<Square>(lsb(bb))))
                return r;

            bb ^= temp;
        }
        return T();
    }


    /*
     * https://www.chessprogramming.org/Traversing_Subsets_of_a_Set
     */
    template<typename F> void for_each_subset(Bitboard mask, F f)
    {
        auto subset = BB_EMPTY;
        while (true)
        {
            f(subset);
            subset = (subset - mask) & mask;
            if (subset == 0)
                break;
        }
    }


    struct State;

    uint64_t zobrist_hash(const State& state);


    /* Move representation */
    class BaseMove
    {
        Square _from_square = Square::UNDEFINED;
        Square _to_square = Square::UNDEFINED;
        PieceType _promotion = PieceType::NONE;

    public:
        BaseMove() = default;
        BaseMove(Square from, Square to, PieceType promo) noexcept
            : _from_square(from)
            , _to_square(to)
            , _promotion(promo)
        {}

        BaseMove(Square from, Square to) noexcept
            : _from_square(from)
            , _to_square(to)
            , _promotion(PieceType::NONE)
        {}

        INLINE constexpr Square to_square() const
        {
            return _to_square;
        }

        INLINE constexpr Square from_square() const
        {
            return _from_square;
        }

        constexpr PieceType promotion() const
        {
            return _promotion;
        }

        constexpr bool is_equal(const BaseMove& other) const
        {
            return _from_square == other._from_square
                && _to_square == other._to_square
                && _promotion == other._promotion;
        }

        std::string uci() const;

        explicit operator bool() const
        {
            return !is_none();
        }

        bool is_none() const
        {
            bool none = (_from_square == _to_square);

            ASSERT(none || (_from_square != UNDEFINED && _to_square != UNDEFINED));
            return none;
        }
    };


    INLINE std::ostream& operator <<(std::ostream& out, const BaseMove& move)
    {
        return out << move.uci();
    }


    struct Move : public BaseMove
    {
    public:
        Move() = default;
        Move(const Move&) = default;
        Move& operator=(const Move&) = default;

        Move(Square from, Square to, PieceType promo = PieceType::NONE) noexcept
            : BaseMove(from, to, promo)
        {
        }

        Move(const BaseMove& base) noexcept
            : BaseMove(base)
        {
        }

        /* group id, for move ordering */
        int8_t  _group = ::search::MoveOrder::UNORDERED_MOVES;
        float   _score = 0;         /* sort score, for move ordering */
        State*  _state = nullptr;   /* state of the board after the move */
    };


    INLINE bool operator==(const BaseMove& lhs, const BaseMove& rhs)
    {
        return lhs.is_equal(rhs);
    }

    INLINE bool operator!=(const BaseMove& lhs, const BaseMove& rhs)
    {
        return !lhs.is_equal(rhs);
    }


#if HAVE_INT128

    INLINE void swap(Move& lhs, Move& rhs)
    {
        using int128_t = __int128;
        static_assert(sizeof(Move) == sizeof(int128_t), "expect sizeof(Move)==16");

        int128_t* x = reinterpret_cast<int128_t*>(&lhs);
        int128_t* y = reinterpret_cast<int128_t*>(&rhs);

        *x = *x ^ *y;
        *y = *x ^ *y;
        *x = *x ^ *y;
    }

#endif /* HAVE_INT128 */


    using MovesList = std::vector<Move>;


#if USE_PIECE_SQUARE_TABLES
    INLINE constexpr int square_index(int i, chess::Color color)
    {
        return square_indices[color][i];
    }


    INLINE const int (&select_piece_square_table(bool endgame, PieceType pt))[64]
    {
        if (endgame && pt == PieceType::KING)
            return ENDGAME_KING_SQUARE_TABLE;

        return SQUARE_TABLE[pt];
    }
#endif /* USE_PIECE_SQUARE_TABLES */


    /* A position on the chessboard represented as a collection of bitboards. */
    struct Position
    {
        union
        {
            struct
            {
                Bitboard black;
                Bitboard white;
            };
            Bitboard _occupied_co[2] = {0, 0};
        };

        union
        {
            struct
            {
                Bitboard pawns;
                Bitboard knights;
                Bitboard bishops;
                Bitboard rooks;
                Bitboard queens;
                Bitboard kings;
            };
            Bitboard _pieces[6] = { 0 };
        };

        PieceType _piece_types[64] = { PieceType::NONE };

        /* Get the bitboard of squares attacked from a given square */
        Bitboard attacks_mask(Square, Bitboard occupied) const;

        INLINE Bitboard attackers_mask(Color color, Square square, Bitboard occupied) const
        {
            const auto attackers = attacker_pieces_mask(color, square, occupied)
                | (kings & BB_KING_ATTACKS[square])
                | (pawns & BB_PAWN_ATTACKS[!color][square]);

            return attackers & occupied_co(color);
        }

        /*
         * Get the bitboard of squares attacked from a given square
         * by pieces other than pawns and kings.
         */
        INLINE Bitboard attacker_pieces_mask(Color color, Square square, Bitboard occupied_mask) const
        {
            auto attackers = knights & BB_KNIGHT_ATTACKS[square];

    #if USE_MAGIC_BITS
            if (const auto queens_and_rooks = queens | rooks)
                attackers |= queens_and_rooks & magic_bits_attacks.Rook(occupied_mask, square);

            if (const auto queens_and_bishops = queens | bishops)
                attackers |= queens_and_bishops & magic_bits_attacks.Bishop(occupied_mask, square);
    #else
            if (const auto queens_and_rooks = queens | rooks)
                attackers |= queens_and_rooks & BB_ROOK_ATTACKS.get(square, occupied_mask);

            if (const auto queens_and_bishops = queens | bishops)
                attackers |= queens_and_bishops & BB_DIAG_ATTACKS.get(square, occupied_mask);

    #endif /* !USE_MAGIC_BITS */

            return attackers & occupied_co(color);
        }

        INLINE Bitboard checkers_mask(Color turn) const
        {
            const auto king_square = king(turn);
            return attackers_mask(!turn, king_square, occupied());
        }

        INLINE Square king(Color color) const
        {
            const auto king_mask = occupied_co(color) & kings;
            /* there should always be a king */
            ASSERT(king_mask);

            return Square(msb(king_mask));
        }

        INLINE Bitboard kings_quarter(Color color) const
        {
            const auto king_mask = kings & occupied_co(color);
            for (auto quadrant : BB_QUANDRANTS)
                if (king_mask & quadrant)
                    return quadrant;

            return BB_EMPTY;
        }

        score_t eval_mobility() const;

        INLINE constexpr Bitboard occupied() const
        {
            return white | black;
        }

        INLINE constexpr Bitboard occupied_co(Color color) const
        {
            return _occupied_co[color];
        }

        INLINE PieceType _piece_type_at(Square square) const
        {
            const auto mask = BB_SQUARES[square];
            if ((occupied() & mask) == 0)
                return PieceType::NONE;
            if (pawns & mask)
                return PieceType::PAWN;
            if (knights & mask)
                return PieceType::KNIGHT;
            if (bishops & mask)
                return PieceType::BISHOP;
            if (rooks & mask)
                return PieceType::ROOK;
            if (queens & mask)
                return PieceType::QUEEN;
            return PieceType::KING;
        }

        INLINE Bitboard& pieces(PieceType piece_type)
        {
            return _pieces[piece_type - 1];
        }

        INLINE Bitboard pieces(PieceType piece_type) const
        {
            return _pieces[piece_type - 1];
        }

        INLINE Bitboard pieces_mask(PieceType piece_type, Color color) const
        {
            return const_cast<Position*>(this)->pieces(piece_type) & occupied_co(color);
        }

        INLINE PieceType piece_type_at(Square square) const
        {
            ASSERT(_piece_types[square] == _piece_type_at(square));
            return _piece_types[square];
        }

        INLINE Color piece_color_at(Square square) const
        {
            constexpr Color colors[] = { BLACK, WHITE };

            const auto mask = BB_SQUARES[square];
            ASSERT(mask & occupied());
            return colors[(mask & white) != 0];
        }

        Bitboard pin_mask(Color, Square) const;
    };


    INLINE Bitboard Position::attacks_mask(Square square, Bitboard occupied) const
    {
        const auto bb_square = BB_SQUARES[square];

        if (bb_square & pawns)
        {
            const bool color = (bb_square & white) == bb_square;
            return BB_PAWN_ATTACKS[color][square];
        }
        else if (bb_square & knights)
        {
            return BB_KNIGHT_ATTACKS[square];
        }
        else if (bb_square & kings)
        {
            return BB_KING_ATTACKS[square];
        }
        else
        {
            Bitboard mask = 0;

    #if USE_MAGIC_BITS
            if ((bb_square & bishops) || (bb_square & queens))
                mask = magic_bits_attacks.Bishop(occupied, square);

            if ((bb_square & rooks) || (bb_square & queens))
                mask |= magic_bits_attacks.Rook(occupied, square);
    #else
            if ((bb_square & bishops) || (bb_square & queens))
                mask = BB_DIAG_ATTACKS.get(square, occupied);

            if ((bb_square & rooks) || (bb_square & queens))
                mask |= BB_ROOK_ATTACKS.get(square, occupied);

    #endif /* !USE_MAGIC_BITS */
            return mask;
        }
    }


    INLINE Bitboard diagonal_attacks(Bitboard mask, Square square)
    {
#if USE_MAGIC_BITS
        return magic_bits_attacks.Bishop(mask, square);
#else
        return BB_DIAG_ATTACKS.get(square, mask);
#endif /* !USE_MAGIC_BITS */
    }


    INLINE Bitboard rank_and_file_attacks(Bitboard mask, Square square)
    {
#if USE_MAGIC_BITS
        return magic_bits_attacks.Rook(mask, square);
#else
        return BB_RANK_ATTACKS.get(square, mask) | BB_FILE_ATTACKS.get(square, mask);
#endif /* !USE_MAGIC_BITS */
    }


    INLINE Bitboard between(Square a, Square b)
    {
        const auto bb = BB_RAYS[a][b] & ((BB_ALL << a) ^ (BB_ALL << b));
        return bb & (bb - 1);
    }


    /*
     * If the square is pinned for the side of the given color, return the pin mask.
     * Otherwise return BB_ALL (so that the pinned piece's attacks can be restricted
     * to the direction of the pin).
     */
    INLINE Bitboard Position::pin_mask(Color color, Square square) const
    {
        const auto king_square = king(color);
        ASSERT(king_square >= 0);

        const auto square_mask = BB_SQUARES[square];
        const auto occupied = this->occupied();
        const auto theirs = this->occupied_co(!color);

        Bitboard result = BB_EMPTY;

        for (auto attacks:
            {
                std::make_pair(diagonal_attacks, bishops | queens),
                std::make_pair(rank_and_file_attacks, rooks | queens),
            })
        {
            auto rays = attacks.first(0, king_square);

            if (rays & square_mask)
            {
                const auto snipers = rays & attacks.second & theirs;

                result = for_each_square_r<Bitboard>(snipers, [&](Square sniper)
                {
                    if ((between(sniper, king_square) & (occupied | square_mask)) == square_mask)
                        return BB_RAYS[king_square][sniper];

                    return BB_EMPTY; /* lambda */
                });
                break;
            }
        }

        return result ? result : BB_ALL;
    }


    struct BoardPosition : public Position
    {
        Bitboard castling_rights = 0;

        Square en_passant_square = UNDEFINED;
        Color turn = WHITE;

        bool equals(const BoardPosition& other) const
        {
            return this->black == other.black
                && this->white == other.white
                && this->pawns == other.pawns
                && this->knights == other.knights
                && this->bishops == other.bishops
                && this->rooks == other.rooks
                && this->queens == other.queens
                && this->kings == other.kings
                && this->castling_rights == other.castling_rights
                && this->en_passant_square == other.en_passant_square
                && this->turn == other.turn;
        }
    };


    INLINE bool operator==(const BoardPosition& lhs, const BoardPosition& rhs)
    {
        return lhs.equals(rhs);
    }


    INLINE bool operator!=(const BoardPosition& lhs, const BoardPosition& rhs)
    {
        return !lhs.equals(rhs);
    }


    namespace
    {
        /* Recursive template for computing exponential at compile time */
        template<int N> struct _exp {
            static constexpr double value = _exp<N-1>::value * M_E;
        };

        template<> struct _exp<0> {
            static constexpr double value = 1;
        };

        template<std::size_t... I>
        static constexpr std::array<double, sizeof ... (I)> _exp_table(std::index_sequence<I...>)
        {
            return { _exp<I>::value ... };
        }

        /* Compile-time sigmoid in range [0,32] */
        INLINE constexpr double _e(int x)
        {
            auto constexpr e = _exp_table(std::make_index_sequence<33>{});
            return x < 0 ? (1.0 / e[-x]) : e[x];
        }
    }

    INLINE constexpr double logistic(int x)
    {
        return 1 / (1.0 + _e(-x));
    }

    /*
     * https://www.chessprogramming.org/Tapered_Eval
     */
    INLINE constexpr double _interpolate(double pc, int midgame, int endgame)
    {
        ASSERT(pc >= 2);
        ASSERT(pc <= 32);

    #if 0
        /* linear, hockey stick */
        return pc <= ENDGAME_PIECE_COUNT
            ? endgame
            : midgame + (endgame - midgame) * double(32 - pc) / (32 - ENDGAME_PIECE_COUNT);
    #else
        /* sigmoid */
        return (endgame - midgame) * (1 - logistic((pc - 19) / 2)) + midgame;
    #endif
    }


#if TUNING_ENABLED || defined(TUNING_PARTIAL)
    INLINE constexpr double interpolate(int pc, int mg, int eg)
    {
        return _interpolate(pc, mg, eg);
    }
#else
    template<int MG, int EG> struct Interpolate
    {
        template<typename T, T... is>  static constexpr std::array<double, sizeof ...(is)>
        make_values(std::integer_sequence<T, is...> int_seq)
        {
            return { _interpolate(is, MG, EG)... };
        }

        static constexpr auto _value = make_values(std::make_index_sequence<33>{});

        static double value(int i) { return _value[i]; }
    };

    #define interpolate(pc, mg, eg) Interpolate<mg, eg>::value(pc)

#endif /* !TUNING_ENABLED */


    struct State : public BoardPosition
    {
        int capture_value = 0;
        int pushed_pawns_score = 0; /* ranks past the middle of the board */
        bool is_castle = false;

        PieceType promotion = PieceType::NONE;

        /* material and PST from white's POV */
        static constexpr auto UNKNOWN_SCORE = std::numeric_limits<score_t>::max();
        mutable score_t simple_score = UNKNOWN_SCORE;

        void apply_move(const BaseMove&);

        State clone() const
        {
            State state;
            clone_into(state);
            return state;
        }

        void clone_into(State&) const;

        int count_connected_pawns(Color, Bitboard mask = BB_ALL) const;
        int count_isolated_pawns(Color, Bitboard mask = BB_ALL) const;

        int diff_connected_rooks() const
        {
            return has_connected_rooks(WHITE) - has_connected_rooks(BLACK);
        }

        int diff_bishop_pairs() const;

        /* evaluate doubled pawns, from the white player's perspective */
        int diff_doubled_pawns() const;

        int diff_isolated_pawns(Bitboard mask = BB_ALL) const
        {
            return count_isolated_pawns(WHITE, mask) - count_isolated_pawns(BLACK, mask);
        }

        /* evaluate base score from the perspective of the side to play */
        template<bool EVAL_MOBILITY = true> score_t eval() const;

        INLINE score_t eval_lazy() const
        {
            if (simple_score == UNKNOWN_SCORE)
                simple_score = eval_simple();

            return simple_score;
        }

        score_t eval_material() const;

        /*
         * Evaluate material and piece-squares (i.e. excluding mobility)
         * from the white side's perspective
         */
        score_t eval_simple() const;

        uint64_t hash() const
        {
            if (_hash == 0)
                _hash = zobrist_hash(*this);

            return _hash;
        }

        void rehash() { _hash = 0; hash(); }

        INLINE bool has_connected_rooks(Color color) const
        {
            const auto rook_mask = rooks & occupied_co(color);
            const auto occupied = this->occupied();

            return for_each_square_r<bool>(rook_mask, [&](Square rook) {
        #if USE_MAGIC_BITS
                return magic_bits_attacks.Rook(occupied, rook) & rook_mask;
        #else
                return (BB_RANK_ATTACKS.get(rook, occupied) & rook_mask)
                    || (BB_FILE_ATTACKS.get(rook, occupied) & rook_mask);
        #endif
            });
        }

        INLINE bool has_fork(Color color) const
        {
            const auto attacked = occupied_co(!color) & ~pawns;
            const auto occupied = black | white;

            return for_each_square_r<bool>(occupied_co(color) & (pawns | knights | bishops),
                [&](Square attacking_square) {
                    return popcount(attacks_mask(attacking_square, occupied) & attacked) > 1;
                });
        }

        bool has_insufficient_material(Color) const;

        bool is_capture(const BaseMove& move) const;
        bool is_castling(const BaseMove&) const;

        INLINE bool is_check() const { return is_check(this->turn); }

        INLINE bool is_check(Color color) const
        {
            if (_check[color] < 0)
                _check[color] = checkers_mask(color) != BB_EMPTY;

            return _check[color] > 0;
        }

        bool is_checkmate() const;

        bool is_endgame() const;
        bool is_en_passant(const BaseMove&) const;
        bool is_pinned(Color color) const;

        bool just_king_and_pawns(Color color) const
        {
            return (~(kings | pawns) & occupied_co(color)) == 0;
        }
        /* Side the move is down to just king and (maybe) pawns? */
        bool just_king_and_pawns() const
        {
            return just_king_and_pawns(turn);
        }

        int longest_pawn_sequence(Bitboard mask) const;
        Bitboard passed_pawns(Color, Bitboard mask = ~(BB_RANK_4 | BB_RANK_5)) const;

        const MovesList& generate_pseudo_legal_moves(
            MovesList&,
            Bitboard to_mask = BB_ALL,
            Bitboard from_mask = BB_ALL) const;

        /* perft */
        size_t make_pseudo_legal_moves(MovesList&) const;

        void generate_moves(MovesList& out, MovesList& buffer) const;

        void generate_castling_moves(MovesList& moves, Bitboard to_mask = BB_ALL) const;

        /*
         * Apply incremental material and piece squares evaluation.
         */
        INLINE void eval_apply_delta(const BaseMove& move, const State& prev)
        {
            simple_score = prev.eval_incremental(move);
            eval_lazy();
        }

        score_t eval_incremental(const BaseMove&) const;

        /*
         * Indirection point for future ideas (dynamic piece weights).
         * Ideas:
         * - use different weights for midgame / endgame and interpolate;
         * - use tables for game phases, use number of pawns to determine phase;
         * - leave weights alone and alter material eval (see eval_piece_grading).
         */
        static INLINE constexpr int weight(PieceType piece_type)
        {
            return WEIGHT[piece_type];
        }

        INLINE int piece_weight_at(Square square) const
        {
            return weight(piece_type_at(square));
        }

        mutable std::array<int8_t, 2> _check = {-1, -1};
        mutable uint64_t _hash = 0;

    private:
        void ep_moves(MovesList& moves, Bitboard to_mask) const;

        /*
         * Evaluate the incremental change of a move.
         */
        template<bool WithEnPassant> score_t eval_delta(const BaseMove&) const;

        PieceType remove_piece_at(Square);

        void set_piece_at(Square, PieceType, Color, PieceType promotion_type = PieceType::NONE);

        static bool is_endgame(const State&);

        enum : int8_t
        {
            ENDGAME_UNKNOWN = -1,
            ENDGAME_FALSE = 0,
            ENDGAME_TRUE = 1
        }
        mutable _endgame = ENDGAME_UNKNOWN;
    }; /* State */

    INLINE int score_pushed_pawns(const State& state, const BaseMove& move)
    {
        static constexpr int scores[2][8] = {
            { 0, 3, 2, 1, 0, 0, 0, 0 },
            { 0, 0, 0, 0, 1, 2, 3, 0 },
        };

        if (state.pawns & BB_SQUARES[move.to_square()])
        {
            return scores[!state.turn][square_rank(move.to_square())];
        }
        return 0;
    }


    static constexpr Square rook_castle_squares[2][2][2] = {
        { { H8, H1 }, { F8, F1 } },
        { { A8, A1 }, { D8, D1 } },
    };


    INLINE void State::apply_move(const BaseMove& move)
    {
        ASSERT(move);
        ASSERT(piece_type_at(move.to_square()) != PieceType::KING);

        _check = { -1, -1 };

        this->capture_value = piece_weight_at(move.to_square());
        this->promotion = move.promotion();

        const auto color = piece_color_at(move.from_square());

        if ((this->is_castle = is_castling(move)) == true)
        {
            const auto king_to_file = square_file(move.to_square());
            ASSERT(king_to_file == 2 || king_to_file == 6);

        #if 0
            Square rook_from_square = UNDEFINED, rook_to_square = UNDEFINED;
            if (king_to_file == 2)
            {
                rook_from_square = (color == WHITE ? A1 : A8);
                rook_to_square = (color == WHITE ? D1 : D8);
            }
            else
            {
                rook_from_square = (color == WHITE ? H1 : H8);
                rook_to_square = (color == WHITE ? F1 : F8);
            }
        #else
            const auto rook_from_square = rook_castle_squares[king_to_file == 2][0][color];
            const auto rook_to_square = rook_castle_squares[king_to_file == 2][1][color];
        #endif
            const auto piece_type = remove_piece_at(rook_from_square);
            ASSERT(piece_type == ROOK);

            ASSERT(piece_type_at(rook_to_square) == NONE);
            set_piece_at(rook_to_square, piece_type, color);
        }

        const auto piece_type = remove_piece_at(move.from_square());

        /* update castling rights */
        if (piece_type == PieceType::KING)
        {
            castling_rights &= ~BB_BACKRANKS[color];
        }
        castling_rights &= ~BB_SQUARES[move.from_square()] & ~BB_SQUARES[move.to_square()];

        /* save current en-passant square */
        const auto ep_square = en_passant_square;
        this->en_passant_square = Square::UNDEFINED; /* reset */

        if (piece_type == PieceType::PAWN)
        {
            const auto diff = move.to_square() - move.from_square();

            if (diff == 16 && square_rank(move.from_square()) == 1)
            {
                en_passant_square = Square(move.from_square() + 8);
            }
            else if (diff == -16 && square_rank(move.from_square()) == 6)
            {
                en_passant_square = Square(move.from_square() - 8);
            }
            else if (move.to_square() == ep_square && (abs(diff) == 7 || abs(diff) == 9) && !capture_value)
            {
                /* Remove pawns captured en passant. */
                remove_piece_at(Square(ep_square - 8 * SIGN[turn]));
                capture_value = weight(PieceType::PAWN);
            }
        }

        set_piece_at(move.to_square(), piece_type, color, move.promotion());
        flip(turn);

        pushed_pawns_score = score_pushed_pawns(*this, move);

        _endgame = ENDGAME_UNKNOWN; /* recalculate lazily */
        _hash = 0; /* invalidate */
    }


    INLINE void State::clone_into(State& state) const
    {
        state = *this;

        state.capture_value = 0;
        state.promotion = PieceType::NONE;
        state.simple_score = UNKNOWN_SCORE;
        state._check = {-1, -1};
        state._hash = 0;
        state._endgame = ENDGAME_UNKNOWN;
    }


    /*
     * Evaluate the incremental score change for a move.
     */
    template<bool WithEnPassant>
    INLINE score_t State::eval_delta(const BaseMove& move) const
    {
        score_t delta;

        const auto eg = is_endgame();
        const auto color = turn;
        const auto moved_piece_type = piece_type_at(move.from_square());

        ASSERT(color == piece_color_at(move.from_square()));

    #if USE_PIECE_SQUARE_TABLES
        const auto i = square_index(move.from_square(), color);
        const auto j = square_index(move.to_square(), color);

        /* piece-square table for the initial position of the moved piece */
        const auto& table_i = select_piece_square_table(eg, moved_piece_type);

        if (move.promotion())
        {
            const auto& table_j = select_piece_square_table(eg, move.promotion());

            delta = table_j[j] - table_i[i] + weight(move.promotion()) - weight(PieceType::PAWN);
        }
        else
        {
            delta = table_i[j] - table_i[i];
        }
    #else
        if (move.promotion())
        {
            delta = weight(move.promotion()) - weight(PieceType::PAWN);
        }
        else
        {
            delta = 0;
        }
    #endif /* USE_PIECE_SQUARE_TABLES */

        /*
         * Capture?
         */
        Square capt_sq = move.to_square();

        if constexpr(WithEnPassant)
            if (is_en_passant(move))
            {
                ASSERT(move.to_square() == en_passant_square);
                capt_sq = Square(move.to_square() - 8 * SIGN[color]);
            }

        if (const auto type = piece_type_at(capt_sq))
        {
            ASSERT(type != PieceType::KING);
            ASSERT(piece_color_at(capt_sq) != color);

            delta += weight(type);

        #if USE_PIECE_SQUARE_TABLES
            /*
             * Can't capture the king, no need to check for king's endgame table;
             * the only alternate endgame table for now is ENDGAME_KING_SQUARE_TABLE
             */
            /* const auto& table = select_piece_square_table(eg, type); */

            const auto& table = SQUARE_TABLE[type];

            delta += table[square_index(capt_sq, !color)];

            /*
             * Does the capture cause a transition to endgame phase?
             */
            if (!eg && popcount(occupied()) == ENDGAME_PIECE_COUNT + 1)
            {
                /* Update piece-square values for KING */
                for (const auto c : { BLACK, WHITE })
                {
                    const auto k1 = square_index(king(c), c);
                    auto k2 = k1;

                    if (moved_piece_type == PieceType::KING && c == color) /* king moved? */
                    {
                        k2 = square_index(move.to_square(), c);

                        /* undo same table assumption */
                        delta -= SQUARE_TABLE[PieceType::KING][k2] - SQUARE_TABLE[PieceType::KING][k1];
                    }

                    const auto d = ENDGAME_KING_SQUARE_TABLE[k2] - SQUARE_TABLE[PieceType::KING][k1];

                    delta += SIGN[c == color] * d;
                }
            }
        #endif /* USE_PIECE_SQUARE_TABLES */
        }

        return delta * SIGN[color];
    }


    INLINE score_t State::eval_incremental(const BaseMove& move) const
    {
        if (simple_score == UNKNOWN_SCORE)
        {
            return UNKNOWN_SCORE; /* full eval needed */
        }
        else
        {
            auto eval = simple_score + eval_delta<true>(move);

            if (is_castling(move))
            {
                const auto king_file = square_file(move.to_square());
                const auto rook_move = BaseMove(
                    chess::rook_castle_squares[king_file == 2][0][turn],
                    chess::rook_castle_squares[king_file == 2][1][turn]);

                eval += eval_delta<false>(rook_move);
            }
            return eval;
        }
    }


    INLINE score_t State::eval_simple() const
    {
        int score = 0;
        const auto endgame = is_endgame();

        for (auto color: {BLACK, WHITE})
        {
            const auto sign = SIGN[color];

            for (auto piece_type : PIECES)
            {
                const auto mask = pieces_mask(piece_type, color);
                score += sign * weight(piece_type) * popcount(mask);

            #if USE_PIECE_SQUARE_TABLES
                const auto& table = select_piece_square_table(endgame, piece_type);

                for_each_square(mask, [&](Square square) {
                    score += sign * table[square_index(square, color)];
                });
            #endif /* USE_PIECE_SQUARE_TABLES */
            }
        }

        return score;
    }


    /*
     * FIXME: doubled-up pawns are double counted as "connected" if there are pawns on
     * the adjacent files. OTOH doubled-up pawns with no paws on neighboring files will
     * EACH be considered "isolated" (which may violate the principle of orthogonality,
     * as doubled and tripled pawns are penalized via diff_doubled_pawns). An alternative
     * could be to count files with connected/isolated pawns instead.
     */
    INLINE int count_pawns(Bitboard pawns, Bitboard color_mask, Bitboard mask, bool connected)
    {
        int count = 0;

        for_each_square(pawns & color_mask & mask, [&](Square p) {
            const auto file_mask = bb_neighbour_files_mask[square_file(p)];

            count += bool(pawns & color_mask & file_mask) == connected;
        });
        return count;
    }


    INLINE int State::count_connected_pawns(Color color, Bitboard mask) const
    {
        return count_pawns(pawns, _occupied_co[color], mask, true);
    }


    INLINE int State::count_isolated_pawns(Color color, Bitboard mask) const
    {
        return count_pawns(pawns, _occupied_co[color], mask, false);
    }


    INLINE int State::diff_bishop_pairs() const
    {
        int count[] = { 0, 0 };

        if (popcount(bishops) == 3)
        {
            for (auto color : { BLACK, WHITE })
            {
                if (popcount(bishops & occupied_co(color)) == 2)
                {
                    count[color] = 1;
                    break;
                }
            }
        }
        return count[WHITE] - count[BLACK];
    }


    INLINE int State::diff_doubled_pawns() const
    {
        int count = 0;

        for (const auto& bb_file : BB_FILES)
        {
            for (auto color : { BLACK, WHITE })
            {
                auto n = popcount(pawns & bb_file & occupied_co(color));
                if (n > 1)
                {
                    count += SIGN[color] * (n - 1);
                }
            }
        }
        return count;
    }


    template<bool EVAL_MOBILITY> INLINE score_t State::eval() const
    {
        auto value = eval_lazy();

        if constexpr(EVAL_MOBILITY)
        {
            if (!is_endgame())
                value += eval_mobility();
        }
        return value * SIGN[turn];
    }


    INLINE score_t State::eval_material() const
    {
        int score = 0;

        for (auto color: {BLACK, WHITE})
        {
            const auto sign = SIGN[color];

            for (auto piece_type : PIECES)
            {
                const auto mask = pieces_mask(piece_type, color);
                score += sign * weight(piece_type) * popcount(mask);
            }
        }
        return score;
    }


    INLINE bool State::is_castling(const BaseMove& move) const
    {
        if ((castling_rights != BB_EMPTY) && (kings & BB_SQUARES[move.from_square()]))
        {
            const auto diff = square_file(move.from_square()) - square_file(move.to_square());
            return abs(diff) > 1;
        }
        return false;
    }


    INLINE bool State::is_en_passant(const BaseMove& move) const
    {
        return en_passant_square == move.to_square()
            && (pawns & BB_SQUARES[move.from_square()])
            && (abs(move.to_square() - move.from_square()) == 7
             || abs(move.to_square() - move.from_square()) == 9)
            && (occupied() & BB_SQUARES[move.to_square()]) == 0;
    }


    INLINE bool State::is_pinned(Color color) const
    {
        return for_each_square_r<bool>(occupied_co(color), [&] (Square square) {

            auto piece_type = piece_type_at(square);

            return (piece_type != PieceType::PAWN) && (piece_type != PieceType::KING)
                && (pin_mask(color, square) != BB_ALL);
        });
    }


    INLINE bool State::is_capture(const BaseMove& move) const
    {
        ASSERT (move);
        return (BB_SQUARES[move.to_square()] & occupied_co(!turn)) || is_en_passant(move);
    }


    /* static */ INLINE bool State::is_endgame(const State& state)
    {
        return (popcount(state.occupied()) <= ENDGAME_PIECE_COUNT);
        /*
            || state.just_king_and_pawns(BLACK)
            || state.just_king_and_pawns(WHITE)

           Bad idea. After a promotion an endgame node
           context can end up with a non-endgame child
         */
    }


    INLINE bool State::is_endgame() const
    {
        if (_endgame == ENDGAME_UNKNOWN)
        {
            _endgame = is_endgame(*this) ? ENDGAME_TRUE : ENDGAME_FALSE;
        }
        return (_endgame == ENDGAME_TRUE);
    }


    INLINE int State::longest_pawn_sequence(Bitboard mask) const
    {
        int result = 0, count = 0;
        auto pawn_mask = (pawns & mask);

        for (int i = 0; pawn_mask != BB_EMPTY && i < 8; pawn_mask &= ~BB_FILES[i++])
        {
            if (BB_FILES[i] & pawn_mask)
                result = std::max(result, ++count);
            else
                count = 0;
        }

        return result == 1 ? 0 : result;
    }


    INLINE Bitboard State::passed_pawns(Color color, Bitboard mask) const
    {
        return pawns & occupied_co(color) & BB_PASSED[color] & mask;
    }


    INLINE PieceType State::remove_piece_at(Square square)
    {
        ASSERT(square != Square::UNDEFINED);

        const auto piece_type = piece_type_at(square);

        if (piece_type)
        {
            const auto mask = BB_SQUARES[square];

            pieces(piece_type) &= ~mask;
            white &= ~mask;
            black &= ~mask;
        }

        _piece_types[square] = PieceType::NONE;

        return piece_type;
    }


    INLINE void State::set_piece_at(Square square, PieceType type, Color color, PieceType promo)
    {
        ASSERT(square != Square::UNDEFINED);
        ASSERT(type);

        remove_piece_at(square);

        const auto mask = BB_SQUARES[square];

        if (promo != PieceType::NONE)
        {
            type = promo;
        }

        pieces(type) |= mask;
        _occupied_co[color] |= mask;

        ASSERT(_piece_types[square] == PieceType::NONE);
        _piece_types[square] = type;
    }


    /*
     * Utilities
     */
    const char* piece_name(PieceType);

    INLINE constexpr const char* color_name(Color color)
    {
        return color == WHITE ? "white" : "black";
    }

    INLINE std::ostream& operator <<(std::ostream& os, Color color)
    {
        return os << color_name(color);
    }

    INLINE std::ostream& operator <<(std::ostream& os, PieceType piece_type)
    {
        return os << piece_name(piece_type);
    }

    INLINE std::ostream& operator <<(std::ostream& os, Square square)
    {
        return os << square_name(square);
    }

    INLINE constexpr Bitboard shift_down(Bitboard b)
    {
        return b >> 8;
    }

    INLINE constexpr Bitboard shift_up(Bitboard b)
    {
        return (b << 8) & BB_ALL;
    }

    INLINE constexpr Bitboard shift_right(Bitboard b)
    {
        return (b << 1) & ~BB_FILE_A & BB_ALL;
    }

    INLINE constexpr Bitboard shift_left(Bitboard b)
    {
        return (b >> 1) & ~BB_FILE_H;
    }


    /* debug */
    void print_bb(Bitboard);
    void print_bb(Bitboard, std::ostream&);


    /* initialize global data structures */
    void _init();


    /* SEE captures.cpp */
    score_t estimate_static_exchanges(const State&, Color, Square, PieceType = PieceType::NONE);
    score_t estimate_captures(const State&);


    /*
     * required by codegen
     */
    INLINE Bitboard sliding_attacks(int square, Bitboard occupied, const std::vector<int>& deltas)
    {
        auto attacks = BB_EMPTY;
        for (const auto delta : deltas)
        {
            int sq = square;

            while (true)
            {
                sq += delta;
                if (sq < 0 || sq >= 64 || square_distance(sq, sq - delta) > 2)
                    break;

                attacks |= BB_SQUARES[sq];

                if (occupied & BB_SQUARES[sq])
                    break;
            }
        }
        return attacks;
    }

    INLINE Bitboard edges(int square)
    {
        return (((BB_RANK_1 | BB_RANK_8) & ~BB_RANKS[square_rank(square)]) |
                ((BB_FILE_A | BB_FILE_H) & ~BB_FILES[square_file(square)]));
    }

} /* namespace chess */

#include "zobrist.h"

