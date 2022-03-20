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
 * Parts inspired and ported from python-chess (C) Niklas Fiekas
 * https://python-chess.readthedocs.io/en/latest/
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
#include "libpopcnt.h"

namespace chess
{
    using Bitboard = uint64_t;

    using AttackMasks = std::array<Bitboard, 64>;
    using RaysRow = std::array<chess::Bitboard, 64>;
    using Rays = std::array<RaysRow, 64>;

    constexpr int SIGN[] = { -1, 1 };

    constexpr Bitboard BB_ALL = ~0ULL;
    constexpr Bitboard BB_EMPTY = 0ULL;
    constexpr Bitboard BB_CENTER = (0x3ULL << 35) | (0x3ULL << 27);

    extern Bitboard BB_SQUARES[64];

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

    constexpr Bitboard BB_BACKRANKS = (BB_RANK_1 | BB_RANK_8);


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
        BB_RANK_4 | BB_RANK_3 | BB_RANK_2,
        BB_RANK_5 | BB_RANK_6 | BB_RANK_7
    };


    extern Rays BB_RAYS;
    // extern Bitboard BB_KING_ATTACKS[64];
    // extern Bitboard BB_KNIGHT_ATTACKS[64];
    // extern Bitboard BB_PAWN_ATTACKS[2][64];
    // extern AttackMasks BB_DIAG_MASKS, BB_FILE_MASKS, BB_RANK_MASKS;


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

    inline bool operator !(PieceType type) { return type == PieceType::NONE; }


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

    inline constexpr Color operator !(Color color)
    {
        return color == WHITE ? BLACK : WHITE;
    }

    inline void flip(Color& color)
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

#if TUNING_ENABLED
    /* Mobility coefficients */
    extern int MOBILITY[7];
#endif

    /* Piece values */
    constexpr int WEIGHT[] = { 0, 100, 350, 350, 525, 1000, 20000 };


    /*
     * BitBoard and Square utilities
     */

    inline int popcount(uint64_t u)
    {
        /* Evals break if results are converted to unsigned! */
        return int(popcnt64(u));
    }


    inline int msb(uint64_t v)
    {
        ASSERT(v);
#if _MSC_VER
        unsigned long index = 0;

        _BitScanReverse64(&index, v);
        return int(index);

#elif (__GNUC__)
        return (63 - __builtin_clzll(v));
#else
        int r = 0;
        while (v >>= 1)
            ++r;
        return r;
#endif /* !__GNUC__ */
    }


    inline int lsb(uint64_t u)
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

    Bitboard square_file_mask(Square);
    Bitboard square_rank_mask(Square);

    inline constexpr int square_file(int square)
    {
        return square & 7;
    }

    inline constexpr int square_rank(int square)
    {
        return square >> 3;
    }

    inline int square_distance(int a, int b)
    {
        return std::max(abs(square_file(a) - square_file(b)), abs(square_rank(a) - square_rank(b)));
    }

    // https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating
    inline constexpr int square_mirror(int square)
    {
        return square ^ 0x38;
    }

    std::string square_name(Square);

    Bitboard between(Square a, Square b);


    template<typename F> constexpr void for_each_square(Bitboard bb, F f)
    {
        while (bb)
        {
            auto i = msb(bb);
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

    /* Move representation */
    class BaseMove
    {
        Square _from_square = Square::UNDEFINED;
        Square _to_square = Square::UNDEFINED;
        PieceType _promotion = PieceType::NONE;

    public:
        BaseMove() = default;
        BaseMove(Square from, Square to, PieceType promo)
            : _from_square(from)
            , _to_square(to)
            , _promotion(promo)
        {}

        inline constexpr Square to_square() const
        {
            return _to_square;
        }

        inline constexpr Square from_square() const
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


    struct Move : public BaseMove
    {
    public:
        Move() = default;
        Move(const Move&) = default;

        Move(Square from, Square to, PieceType promo = PieceType::NONE)
            : BaseMove(from, to, promo)
        {
        }

        Move(const BaseMove& base) : BaseMove(base)
        {
        }

        /* group id, for move ordering */
        int8_t  _group = ::search::MoveOrder::UNORDERED_MOVES;
        float   _score = 0;         /* sort score, for move ordering */
        State*  _state = nullptr;   /* state of the board after the move */
    };


    inline bool operator==(const BaseMove& lhs, const BaseMove& rhs)
    {
        return lhs.is_equal(rhs);
    }

    inline bool operator!=(const BaseMove& lhs, const BaseMove& rhs)
    {
        return !lhs.is_equal(rhs);
    }


#if HAVE_INT128

    inline void swap(Move& lhs, Move& rhs)
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

        Bitboard pawns = 0;
        Bitboard knights = 0;
        Bitboard bishops = 0;
        Bitboard rooks = 0;
        Bitboard queens = 0;
        Bitboard kings = 0;
        PieceType _piece_types[64] = { PieceType::NONE };

        /* Get the bitboard of squares attacked from a given square */
        Bitboard attacks_mask(Square) const;

        Bitboard attackers_mask(Color, Square, Bitboard occupied) const;

        Bitboard attackers_mask(Color color, Square square) const
        {
            return attackers_mask(color, square, occupied());
        }

        Bitboard checkers_mask(Color turn) const
        {
            const auto king_square = king(turn);
            return attackers_mask(!turn, king_square);
        }

        Square king(Color) const;
        Bitboard kings_quarter(Color) const;

        score_t eval_mobility() const;

        constexpr Bitboard occupied() const
        {
            return white | black;
        }

        constexpr Bitboard occupied_co(Color color) const
        {
            return _occupied_co[color];
        }

        std::vector<int> piece_types(Bitboard) const;

        Bitboard& pieces(PieceType);

        Bitboard pieces_mask(PieceType piece_type, Color color) const
        {
            return const_cast<Position*>(this)->pieces(piece_type) & occupied_co(color);
        }

        inline PieceType _piece_type_at(Square square) const
        {
            const auto mask = BB_SQUARES[square];
            if ((occupied() & mask) == 0)
                return NONE;
            if (pawns & mask)
                return PAWN;
            if (knights & mask)
                return KNIGHT;
            if (bishops & mask)
                return BISHOP;
            if (rooks & mask)
                return ROOK;
            if (queens & mask)
                return QUEEN;
            return KING;
        }

        inline PieceType piece_type_at(Square square) const
        {
            ASSERT(_piece_types[square] == _piece_type_at(square));
            return _piece_types[square];
        }

        inline Color piece_color_at(Square square) const
        {
            constexpr Color colors[] = { BLACK, WHITE };

            const auto mask = BB_SQUARES[square];
            ASSERT(mask & occupied());
            return colors[(mask & white) != 0];
        }

        inline std::pair<PieceType, Color> piece_at(Square square) const
        {
            if (auto piece_type = piece_type_at(square))
                return std::make_pair(piece_type, piece_color_at(square));

            return std::make_pair(PieceType::NONE, BLACK);
        }

        Bitboard pin_mask(Color, Square, bool threat_only = false) const;
    };


    struct BoardPosition : public Position
    {
        Bitboard castling_rights = 0;
        uint8_t _padding[6] = { 0 };
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

    static_assert(sizeof(BoardPosition) % 8 == 0, "8-byte alignment expected");

    inline bool operator==(const BoardPosition& lhs, const BoardPosition& rhs)
    {
        return lhs.equals(rhs);
    }

    inline bool operator!=(const BoardPosition& lhs, const BoardPosition& rhs)
    {
        return !lhs.equals(rhs);
    }


    template<typename T> inline T constexpr pow2(T x) { return x * x; }

    /*
     * https://www.chessprogramming.org/Tapered_Eval
     */
    inline constexpr double interpolate(double pc, score_t midgame, score_t endgame)
    {
    #if 0
        /* linear, hockey stick */
        return pc <= ENDGAME_PIECE_COUNT
            ? endgame
            : midgame + (endgame - midgame) * double(32 - pc) / (32 - ENDGAME_PIECE_COUNT);
    #else
        /* quadratic with capping */
        return pc <= ENDGAME_PIECE_COUNT
            ? endgame
            : midgame + (endgame - midgame) * pow2(32 - pc) / pow2(32 - ENDGAME_PIECE_COUNT);
    #endif
    }


    struct State : public BoardPosition
    {
        int capture_value = 0;
        int pushed_pawns_score = 0; /* ranks past the middle of the board */
        bool is_castle = false;

        PieceType promotion = PieceType::NONE;
        score_t simple_score = 0;

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

        /* diff relative bishop strenghts */
        int diff_bishops_strength() const;

        int diff_bishop_pairs() const;

        /* evaluate doubled pawns, from the white player's perspective */
        int diff_doubled_pawns() const;

        int diff_isolated_pawns(Bitboard mask = BB_ALL) const
        {
            return count_isolated_pawns(WHITE, mask) - count_isolated_pawns(BLACK, mask);
        }

        /* evaluate base score from the perspective of the side to play */
        score_t eval() const;

        score_t eval_material() const;

        /*
         * Evaluate material and piece-squares (i.e. excluding mobility)
         * from the white side's perspective
         */
        score_t eval_simple() const;

        uint64_t hash() const;

        void rehash() { _hash = 0; hash(); }

        bool has_connected_rooks(Color) const;
        bool has_fork(Color) const;
        bool has_insufficient_material(Color) const;

        bool is_capture(const BaseMove& move) const;
        bool is_castling(const BaseMove&) const;

        bool is_check() const
        {
            if (_check < 0)
            {
                _check = is_check(turn);
                ASSERT(bool(_check) == (checkers_mask(turn) != 0));
            }
            return _check > 0;
        }

        bool is_check(Color turn) const
        {
            return checkers_mask(turn) != BB_EMPTY;
        }

        bool is_checkmate() const;

        bool is_endgame() const;
        bool is_en_passant(const BaseMove&) const;
        bool is_pinned(Color color, bool threat = false) const;

        /* Side the move is down to just king and (maybe) pawns? */
        bool just_king_and_pawns() const
        {
            return (occupied_co(turn) & ~(kings | pawns)) == 0;
        }

        int longest_pawn_sequence(Bitboard mask) const;
        Bitboard passed_pawns(Color, Bitboard mask = ~(BB_RANK_4 | BB_RANK_5)) const;

        const MovesList& generate_pseudo_legal_moves(
            MovesList&,
            Bitboard to_mask = BB_ALL,
            Bitboard from_mask = BB_ALL) const;

        void generate_castling_moves(MovesList& moves, Bitboard to_mask = BB_ALL) const;

        void eval_apply_delta(const BaseMove&, const State& previous);

    #if ENDGAME_INCREASED_PAWN_VALUE
        inline int weight(PieceType piece_type) const
        {
            static_assert(ENDGAME_INCREASED_PAWN_VALUE > WEIGHT[PAWN]);

            if (piece_type == PieceType::PAWN)
                return interpolate(popcount(occupied()), WEIGHT[PAWN], ENDGAME_INCREASED_PAWN_VALUE);

            return WEIGHT[piece_type];
        }
    #else
        static inline constexpr int weight(PieceType piece_type)
        {
            return WEIGHT[piece_type];
        }
    #endif /* ENDGAME_INCREASED_PAWN_VALUE */

        mutable uint64_t _hash = 0;

    private:
        void ep_moves(MovesList& moves, Bitboard to_mask) const;

        /* evaluate the incremental change of a move */
        score_t eval_delta(Square from_square, Square to_square) const;

        PieceType remove_piece_at(Square);

        void set_piece_at(Square, PieceType, Color, PieceType promotion_type = PieceType::NONE);

        static bool is_endgame(const State&);

        mutable int8_t _check = -1;

        enum : int8_t
        {
            ENDGAME_UNKNOWN = -1,
            ENDGAME_FALSE = 0,
            ENDGAME_TRUE = 1
        }
        mutable _endgame = ENDGAME_UNKNOWN;
    }; /* State */

    /* utilities */
    const char* piece_name(PieceType);

    inline constexpr const char* color_name(Color color)
    {
        return color == WHITE ? "white" : "black";
    }


    inline std::ostream& operator <<(std::ostream& os, Color color)
    {
        return os << color_name(color);
    }

    inline std::ostream& operator <<(std::ostream& os, PieceType piece_type)
    {
        return os << piece_name(piece_type);
    }

    inline std::ostream& operator <<(std::ostream& os, Square square)
    {
        return os << square_name(square);
    }


    inline constexpr Bitboard shift_down(Bitboard b)
    {
        return b >> 8;
    }

    inline constexpr Bitboard shift_up(Bitboard b)
    {
        return (b << 8) & BB_ALL;
    }

    inline constexpr Bitboard shift_right(Bitboard b)
    {
        return (b << 1) & ~BB_FILE_A & BB_ALL;
    }

    inline constexpr Bitboard shift_left(Bitboard b)
    {
        return (b >> 1) & ~BB_FILE_H;
    }

    Bitboard edges(int square);
    Bitboard sliding_attacks(int, Bitboard occupied, const std::vector<int>& deltas);

    /*
     * FIXME: doubled-up pawns are double counted as "connected" if they are pawns on
     * the adjacent files. OTOH doubled-up pawns with no paws on neighboring files will
     * EACH be considered "isolated" (which may violate the principle of orthogonality,
     * as doubled and tripled pawns are penalized via diff_doubled_pawns). An alternative
     * could be to count files with connected/isolated pawns instead.
     */
    static inline int count_pawns(Bitboard pawns, Bitboard color_mask, Bitboard mask, bool connected)
    {
        int count = 0;

        for_each_square(pawns & color_mask & mask, [&](Square p) {
            const auto file_mask = bb_neighbour_files_mask[square_file(p)];

            count += bool(pawns & color_mask & file_mask) == connected;
        });
        return count;
    }

    inline int State::count_connected_pawns(Color color, Bitboard mask) const
    {
        return count_pawns(pawns, _occupied_co[color], mask, true);
    }

    inline int State::count_isolated_pawns(Color color, Bitboard mask) const
    {
        return count_pawns(pawns, _occupied_co[color], mask, false);
    }


    /* debug */
    void print_bb(Bitboard);
    void print_bb(Bitboard, std::ostream&);


    /* initialize global data structures */
    void _init();


    /* SEE captures.cpp */
    score_t estimate_static_exchanges(const State&, Color, Square, PieceType = PieceType::NONE);
    score_t estimate_captures(const State&);

} // namespace chess
