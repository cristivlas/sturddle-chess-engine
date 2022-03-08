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
/*
 * Core chess routines (bitboards, move generation, simple scoring)
 * Parts inspired and ported from python-chess (C) Niklas Fiekas
 *
 * _init() must be called to initialize global data structures.
 */
#include <algorithm>
#include <iomanip>
#include "attacks.h"
#include "chess.h"
#include "tables.h"
#include "zobrist.h"

static constexpr chess::Bitboard BB_LIGHT_SQUARES = 0x55aa55aa55aa55aaULL;
static constexpr chess::Bitboard BB_DARK_SQUARES  = 0xaa55aa55aa55aa55ULL;


static int square_indices[2][64] = { {}, {} };


static inline int square_index(int i, chess::Color color)
{
    return square_indices[color][i];
}


static inline constexpr int _square_index(int i, chess::Color color)
{
    if (color == chess::BLACK)
        i = chess::square_mirror(i);
    return (7 - i / 8) * 8 + i % 8;
}


static void init_square_indices()
{
    for (auto color : { chess::BLACK, chess::WHITE })
        for (int i = 0; i != 64; ++i)
            square_indices[color][i] = _square_index(i, color);
}


namespace chess
{
#if !TUNING_ENABLED
    static constexpr
#endif /* TUNING_ENABLED */
        int MOBILITY[] = { 0, 3, 2, 2, 1, 3, 2 };

    AttackMasks BB_DIAG_MASKS, BB_FILE_MASKS, BB_RANK_MASKS;
    Bitboard BB_SQUARES[64];

    Bitboard BB_KING_ATTACKS[64];
    Bitboard BB_KNIGHT_ATTACKS[64];
    Bitboard BB_PAWN_ATTACKS[2][64];

    Rays BB_RAYS;


    const char* piece_name(PieceType piece_type)
    {
        static const char* const names[] = {
            "<invalid piece type>",
            "pawn",
            "knight",
            "bishop",
            "rook",
            "queen",
            "king",
        };

        if (piece_type > NONE && piece_type <= KING)
            return names[piece_type];

        return names[NONE];
    }


    Bitboard square_file_mask(Square square)
    {
        return BB_FILE_MASKS[square];
    }


    Bitboard square_rank_mask(Square square)
    {
        return BB_RANK_MASKS[square];
    }


    std::string square_name(Square s)
    {
        if (s == UNDEFINED)
            return "n/a";
        const char name[] = {char('a' + square_file(s)), char('1' + square_rank(s)), 0};
        return name;
    }


    Bitboard between(Square a, Square b)
    {
        const auto bb = BB_RAYS[a][b] & ((BB_ALL << a) ^ (BB_ALL << b));
        return bb & (bb - 1);
    }


    /* debug */
    void print_bb(Bitboard bb, std::ostream& out)
    {
        for (int i = 0; i < 64; ++i)
        {
            auto mask = BB_SQUARES[square_mirror(i)];
            if (i && i % 8 == 0)
                out << std::endl;
            out << (mask & bb ? '1' : '.') << ' ';
        }
        out << "\n\n";
    }


    void print_bb(Bitboard bb)
    {
        print_bb(bb, std::cout);
    }


    std::string BaseMove::uci() const
    {
        if (is_none())
            return "none";

        return square_name(_from_square) + square_name(_to_square) + PIECE_SYMBOL[_promotion];
    }


    Bitboard sliding_attacks(int square, Bitboard occupied, const std::vector<int>& deltas)
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


    Bitboard step_attacks(int square, const std::vector<int>& deltas)
    {
        return sliding_attacks(square, BB_ALL, deltas);
    }


    Bitboard edges(int square)
    {
        return (((BB_RANK_1 | BB_RANK_8) & ~BB_RANKS[square_rank(square)]) |
                ((BB_FILE_A | BB_FILE_H) & ~BB_FILES[square_file(square)]));
    }


    Rays _rays()
    {
        Rays rays;

        for (int a = 0; a < 64; ++a)
        {
            const auto bb_a = BB_SQUARES[a];
            RaysRow rays_row;

            for (int b = 0; b < 64; ++b)
            {
                const auto bb_b = BB_SQUARES[b];
                if (BB_DIAG_ATTACKS[a][0] & bb_b)
                    rays_row[b] = ((BB_DIAG_ATTACKS[a][0] & BB_DIAG_ATTACKS[b][0]) | bb_a | bb_b);

                else if (BB_RANK_ATTACKS[a][0] & bb_b)
                    rays_row[b] = (BB_RANK_ATTACKS[a][0] | bb_a);

                else if (BB_FILE_ATTACKS[a][0] & bb_b)
                    rays_row[b] = (BB_FILE_ATTACKS[a][0] | bb_a);

                else
                    rays_row[b] = BB_EMPTY;
            }
            rays[a] = rays_row;
        }
        return rays;
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


    static void
    init_attack_masks(AttackMasks& mask_table, const AttackTable (&tables)[64], const std::vector<int>& deltas)
    {
        for (int square = 0; square < 64; ++square)
        {
            auto mask = sliding_attacks(square, 0, deltas) & ~edges(square);
            mask_table[square] = mask;

            /* validate the generated attack tables */
            for_each_subset(mask, [&](Bitboard key)
            {
                const auto value = sliding_attacks(square, key, deltas);
                ASSERT_ALWAYS(tables[square][key] == value);
            });
        }
    }


    /* Initialize global data. */
    void _init()
    {
        static bool once = false;
        if (once)
            return;
        once = true;

        init_square_indices();

        std::generate(&BB_SQUARES[0], &BB_SQUARES[64], [&] {
            static int i = 0;
            return 1ULL << i++;
        });

        std::generate(&BB_KNIGHT_ATTACKS[0], &BB_KNIGHT_ATTACKS[64], [] {
            static int i = 0;
            return step_attacks(i++, {17, 15, 10, 6, -17, -15, -10, -6});
        });
        std::generate(&BB_KING_ATTACKS[0], &BB_KING_ATTACKS[64], [] {
            static int i = 0;
            return step_attacks(i++, {9, 8, 7, 1, -9, -8, -7, -1});
        });

        // black pawn attacks
        std::generate(&BB_PAWN_ATTACKS[0][0], &BB_PAWN_ATTACKS[0][64], [] {
            static int i = 0;
            return step_attacks(i++, {-7, -9});
        });

        // white pawn attacks
        std::generate(&BB_PAWN_ATTACKS[1][0], &BB_PAWN_ATTACKS[1][64], [] {
            static int i = 0;
            return step_attacks(i++, {7, 9});
        });

        init_attack_masks(BB_DIAG_MASKS, BB_DIAG_ATTACKS, {-9, -7, 7, 9});
        init_attack_masks(BB_FILE_MASKS, BB_FILE_ATTACKS, {-8, 8});
        init_attack_masks(BB_RANK_MASKS, BB_RANK_ATTACKS, {-1, 1});

        BB_RAYS = _rays();
    }


    template<typename T>
    void add_move(T& container, Square from_square, Square to_square, PieceType promotion = NONE)
    {
        container.emplace_back(Move(from_square, to_square, promotion));
    }


    template<typename T> void add_pawn_moves(T& moves_list, Square from_square, Square to_square)
    {
        if ((square_rank(to_square) == 0) || (square_rank(to_square) == 7))
        {
            add_move(moves_list, from_square, to_square, QUEEN);
            add_move(moves_list, from_square, to_square, ROOK);
            add_move(moves_list, from_square, to_square, BISHOP);
            add_move(moves_list, from_square, to_square, KNIGHT);
        }
        else
        {
            add_move(moves_list, from_square, to_square);
        }
    }


    Bitboard Position::attacks_mask(Square square) const
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
            Bitboard attacks = 0;
            const auto occupied = black | white;

            if ((bb_square & bishops) || (bb_square & queens))
            {
                attacks = BB_DIAG_ATTACKS[square][BB_DIAG_MASKS[square] & occupied];
            }
            if ((bb_square & rooks) || (bb_square & queens))
            {
                attacks |= (BB_RANK_ATTACKS[square][BB_RANK_MASKS[square] & occupied] |
                            BB_FILE_ATTACKS[square][BB_FILE_MASKS[square] & occupied]);
            }
            return attacks;
        }
    }


    Bitboard Position::attackers_mask(Color color, Square square, Bitboard occupied) const
    {
        const auto rank_pieces = BB_RANK_MASKS[square] & occupied;
        const auto file_pieces = BB_FILE_MASKS[square] & occupied;
        const auto diag_pieces = BB_DIAG_MASKS[square] & occupied;

        const auto queens_and_rooks = queens | rooks;
        const auto queens_and_bishops = queens | bishops;

        const auto attackers = (
            (kings & BB_KING_ATTACKS[square]) |
            (knights & BB_KNIGHT_ATTACKS[square]) |
            (queens_and_rooks & BB_RANK_ATTACKS[square][rank_pieces]) |
            (queens_and_rooks & BB_FILE_ATTACKS[square][file_pieces]) |
            (queens_and_bishops & BB_DIAG_ATTACKS[square][diag_pieces]) |
            (pawns & BB_PAWN_ATTACKS[!color][square]));

        return attackers & occupied_co(color);
    }


    Bitboard& Position::pieces(PieceType piece_type)
    {
        static Bitboard none;

        switch (piece_type)
        {
        case PAWN:
            return pawns;
        case KNIGHT:
            return knights;
        case BISHOP:
            return bishops;
        case ROOK:
            return rooks;
        case QUEEN:
            return queens;
        case KING:
            return kings;

        case NONE:
            ASSERT(false && "invalid piece type");
            break;
        }
        return none;
    }


    std::vector<int> Position::piece_types(Bitboard bb) const
    {
        std::vector<int> result;
        result.reserve(32);

        for_each_square(bb, [&](Square square) {
            if (auto piece_type = piece_type_at(square))
                result.push_back(piece_type);
        });
        return result;
    }


    Square Position::king(Color color) const
    {
        auto king_mask = occupied_co(color) & kings;
        auto king_square = king_mask ? Square(msb(king_mask)) : Square::UNDEFINED;

        /* there should always be a king */
        ASSERT_ALWAYS (king_square >= 0);
        return king_square;
    }


    Bitboard Position::kings_quarter(Color color) const
    {
        const auto king_mask = kings & occupied_co(color);
        for (auto quadrant : BB_QUANDRANTS)
            if (king_mask & quadrant)
                return quadrant;

        return BB_EMPTY;
    }


    /*
     * Rough approximation of the (difference in) move options. No en-passant, no SEE.
     */
    score_t Position::eval_mobility() const
    {
        score_t mobility = 0;

        Bitboard attacks[2][7] = { { 0 }, { 0 } };
        Bitboard attacks_from[2][64] = { { 0 }, { 0 } };
        Bitboard checks_from[2][64] = { { 0 }, { 0 } };
        Bitboard checks[2] = { 0 };
        Bitboard pinned[2] = { 0, 0 };

        /*
         * Step 1, build some tables...
         */
        for (const auto color: {BLACK, WHITE})
        {
            for_each_square(occupied_co(color), [&](Square square) {
                const auto piece_type = piece_type_at(square);
                auto piece_attacks = attacks_mask(square);

                checks_from[color][square] = piece_attacks;
                checks[color] |= piece_attacks;

                const Bitboard mask = (piece_type == KING) ? BB_ALL : pin_mask(color, square);
                piece_attacks &= mask; /* can only attack in the direction of the pin */
                pinned[color] |= (mask != BB_ALL) * BB_SQUARES[square];

                attacks[color][piece_type] |= piece_attacks;
                attacks_from[color][square] = piece_attacks & ~(kings | occupied_co(color));
            });
        }

        const auto occupied = this->occupied();
        /*
         * Step 2, count where pieces can go without being captured by the
         * king or pieces of lower values, king not allowed to move into check.
         */
        for (const auto color: {BLACK, WHITE})
        {
            if (MOBILITY[PAWN])
            {
                /* pawns moves... */
                const auto own_pawns = pawns & occupied_co(color) & ~pinned[color];

                const auto single_pawn_moves =
                    (color ? shift_up(own_pawns) : shift_down(own_pawns)) & ~occupied;

                const auto double_pawn_moves = color
                    ? shift_up(single_pawn_moves) & ~occupied & (BB_RANK_3 | BB_RANK_4)
                    : shift_down(single_pawn_moves) & ~occupied & (BB_RANK_6 | BB_RANK_5);

                /* ... and captures */
                const auto pawn_captures = attacks[color][PAWN] & occupied_co(!color);

                mobility += SIGN[color] * MOBILITY[PAWN]
                    * (popcount(single_pawn_moves | double_pawn_moves) + popcount(pawn_captures));
            }

            // std::cout << popcount(single_pawn_moves | double_pawn_moves);
            // std::cout << " " << color << " pawn move(s)\n";
            // std::cout << popcount(pawn_captures) << " pawn capture(s)\n";

            /* non-pawns */
            for_each_square(occupied_co(color) & ~pawns, [&](Square square) {
                if (attacks_from[color][square] == BB_EMPTY)
                    return;

                const auto piece_type = piece_type_at(square);
                ASSERT(piece_type != PAWN);
                if (MOBILITY[piece_type] == 0)
                    return;

                /* where can the enemy king capture? */
                Bitboard defended = attacks[!color][KING];
                for (int i = 0; i != 64; ++i)
                    // if (i != square) /* skip checks by the piece to move */
                    //     defended &= ~checks_from[color][i];
                    defended &= ~checks_from[color][i] | ((i == square) * BB_ALL);

                switch (piece_type)
                {
                case KNIGHT:
                case BISHOP:
                    defended |= attacks[!color][PAWN];
                    break;

                case ROOK:
                case QUEEN:
                    for (int i = PAWN; i != piece_type; ++i)
                        defended |= attacks[!color][i];
                    break;

                case KING:
                    defended |= checks[!color];
                default:
                    break;
                }

                mobility += SIGN[color] * MOBILITY[piece_type]
                    * popcount(attacks_from[color][square] & ~defended);

                // std::cout << "\n" << color << " " << piece_type << "\n";
                // std::cout << popcount(attacks_from[color][square] & ~defended);
                // std::cout << " attacks\n";
                // print_bb(attacks_from[color][square]);
                // print_bb(defended);
                // print_bb(attacks_from[color][square] & ~defended);
            });
        }

        return mobility;
    }


    /*
     * If the square is pinned for the side of the given color, return the pin mask.
     * If threat_only is true, return mask only if sniper's value is less than the victim's
     * or sniper is not attacked back by victim.
     */
    Bitboard Position::pin_mask(Color color, Square square, bool threat_only) const
    {
        static const AttackTable* const all_attacks[3] =
            { BB_FILE_ATTACKS, BB_RANK_ATTACKS, BB_DIAG_ATTACKS };

        const auto king_square = king(color);
        if (king_square < 0)
            return BB_ALL;

        const auto square_mask = BB_SQUARES[square];
        const Bitboard all_sliders[] = { rooks|queens, rooks|queens, bishops|queens };

        for (int i = 0; i != 3; ++i)
        {
            auto& attacks = all_attacks[i];
            const auto sliders = all_sliders[i];

            const auto rays = attacks[king_square][0];
            if (rays & square_mask)
            {
                if (auto mask = for_each_square_r<Bitboard>(rays & sliders & occupied_co(!color), [&](Square sniper)
                {
                    if ((between(sniper, king_square) & (occupied() | square_mask)) == square_mask)
                    {
                        if (!threat_only
                            || piece_type_at(sniper) < piece_type_at(square)
                            || (attacks_mask(square) & BB_SQUARES[sniper]) == 0)
                        {
                            return BB_RAYS[king_square][sniper];
                        }
                    }
                    return BB_EMPTY;
                })) {
                    return mask;
                }
                break;
            }
        }
        return BB_ALL;
    }


    std::vector<int> scan_forward(Bitboard bb)
    {
        std::vector<int> squares;
        squares.reserve(64);

        for_each_square(bb, [&squares](Square s) {
            squares.push_back(s);
        });
        return squares;
    }


    static inline const int* select_piece_square_table(bool end_game, int piece_type)
    {
        if (end_game)
        {
            switch (piece_type)
            {
                case PAWN:
                    return ENDGAME_PAWN_SQUARE_TABLE;

                case KING:
                    return ENDGAME_KING_SQUARE_TABLE;

                default:
                    break;
            }
        }

        return SQUARE_TABLE[piece_type];
    }


    void State::clone_into(State& state) const
    {
        state = *this;

        state.capture_value = 0;
        state.promotion = NONE;
        state.simple_score = 0;
        state._check = -1;
        state._hash = 0;
        state._endgame = ENDGAME_UNKNOWN;
    }


    PieceType State::remove_piece_at(Square square)
    {
        ASSERT(square != Square::UNDEFINED);

        auto piece_type = piece_type_at(square);
        if (piece_type)
        {
            const auto mask = BB_SQUARES[square];
            pieces(piece_type) ^= mask;
            white &= ~mask;
            black &= ~mask;
        }

        _piece_types[square] = PieceType::NONE;
        return piece_type;
    }


    void State::set_piece_at(Square square, PieceType piece_type, Color color, PieceType promotion)
    {
        ASSERT(square != Square::UNDEFINED);
        ASSERT(piece_type);

        remove_piece_at(square);

        const auto mask = BB_SQUARES[square];

        if (promotion != PieceType::NONE)
        {
            piece_type = promotion;
        }

        pieces(piece_type) |= mask;
        _occupied_co[color] ^= mask;

        ASSERT(_piece_types[square] == PieceType::NONE);
        _piece_types[square] = piece_type;
    }


    static int score_pushed_pawns(const State& state, const BaseMove& move)
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


    void State::apply_move(const BaseMove& move)
    {
        ASSERT (move);
        ASSERT (piece_type_at(move.to_square()) != PieceType::KING);

        _check = -1;

        capture_value = weight(piece_type_at(move.to_square()));
        promotion = move.promotion();

        const auto color = piece_color_at(move.from_square());

        is_castle = is_castling(move);

        if (is_castle)
        {
            auto king_to_file = square_file(move.to_square());
            ASSERT(king_to_file == 2 || king_to_file == 6);

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
            const auto piece_type = remove_piece_at(rook_from_square);
            ASSERT(piece_type == ROOK);

            ASSERT(piece_type_at(rook_to_square) == NONE);
            set_piece_at(rook_to_square, piece_type, color);
        }

        const auto piece_type = remove_piece_at(move.from_square());

        if (piece_type == PieceType::KING)
        {
            /* update castling rights */
            auto backrank = color == WHITE ? BB_RANK_1 : BB_RANK_8;
            castling_rights &= ~backrank;
        }
        castling_rights &= ~BB_SQUARES[move.from_square()] & ~BB_SQUARES[move.to_square()];

        /* save current en-passant square */
        const auto ep_square = en_passant_square;
        en_passant_square = Square::UNDEFINED; /* reset */

        if (piece_type == PieceType::PAWN)
        {
            auto diff = move.to_square() - move.from_square();
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


    /*
     * Apply incremental material and piece squares evaluation.
     */
    void State::eval_apply_delta(const BaseMove& move, const State& prev)
    {
        if (move.promotion()
            || prev.simple_score == 0
            || prev.is_castling(move)
            || prev.is_en_passant(move))
        {
            simple_score = 0;
        }
        else
        {
            simple_score = prev.simple_score + prev.eval_delta(move.from_square(), move.to_square());
        }
    }


    bool State::is_checkmate() const
    {
        if (is_check())
        {
            MovesList moves;
            for (const auto& move : generate_pseudo_legal_moves(moves))
            {
                ASSERT(move);
                ASSERT(piece_type_at(move.to_square()) != KING);

                auto state = clone();
                state.apply_move(move);

                if (state.checkers_mask(turn) == 0)
                {
                    return false;
                }
            }
            return true;
        }
        return false;
    }


    const MovesList&
    State::generate_pseudo_legal_moves(
        MovesList&  moves_list,
        Bitboard    to_mask,
        Bitboard    from_mask) const
    {
        moves_list.clear();
        to_mask &= ~kings;

        const auto our_pieces = occupied_co(turn);

        /* Piece moves.*/
        if (const auto non_pawns = our_pieces & ~pawns & from_mask)
            for_each_square(non_pawns, [&](Square from_square) {
                auto moves = attacks_mask(from_square) & ~our_pieces & to_mask;
                for_each_square(moves, [&](Square to_square) {
                    add_move(moves_list, from_square, to_square);
                });
            });

        if (castling_rights && (kings & from_mask) != BB_EMPTY)
            generate_castling_moves(moves_list, to_mask);

        /* Pawn moves */
        if (const auto our_pawns = our_pieces & pawns & from_mask)
        {
            /* captures */
            const auto capturers = our_pawns;
            for_each_square(capturers, [&](Square from_square) {
                const auto targets = BB_PAWN_ATTACKS[turn][from_square] & occupied_co(!turn) & to_mask;
                for_each_square(targets, [&moves_list, from_square](Square to_square) {
                    add_pawn_moves(moves_list, from_square, to_square);
                });
            });

            /* prepare pawn advance generation */
            const auto occupied = black | white;
            auto single_moves = (turn ? our_pawns << 8 : our_pawns >> 8) & ~occupied;
            auto double_moves = turn
                ? (single_moves << 8) & ~occupied & (BB_RANK_3 | BB_RANK_4)
                : (single_moves >> 8) & ~occupied & (BB_RANK_6 | BB_RANK_5);

            single_moves &= to_mask;
            double_moves &= to_mask;

            const auto sign = SIGN[turn];
            /* single pawn moves */
            for_each_square(single_moves, [&moves_list, sign](Square to_square) {
                auto from_square = Square(to_square - 8 * sign);
                add_pawn_moves(moves_list, from_square, to_square);
            });

            /* double pawn moves */
            for_each_square(double_moves, [&moves_list, sign](Square to_square) {
                auto from_square = Square(to_square - 16 * sign);
                add_move(moves_list, from_square, to_square);
            });

            if (en_passant_square != -1)
                ep_moves(moves_list, to_mask);
        }
        return moves_list;
    }


    void State::generate_castling_moves(MovesList& moves, Bitboard to_mask) const
    {
        const auto king_square = king(turn);
        const auto backrank = (turn == WHITE) ? BB_RANK_1 : BB_RANK_8;

        /* king not on the back rank? */
        if ((BB_SQUARES[king_square] & backrank & BB_FILE_E) == 0)
            return;

        /* king in check? */
        if (attackers_mask(!turn, king_square))
            return;

        for_each_square((rooks & occupied_co(turn) & castling_rights), [&](Square rook_square)
        {
            /* any pieces between the king and the rook? */
            if (between(king_square, rook_square) & occupied())
                return;

            const auto rook_file = square_file(rook_square);
            ASSERT(rook_file == 0 || rook_file == 7);

            const auto king_to_square = Square(msb(backrank & (rook_file ? BB_FILE_G : BB_FILE_C)));

            if ((BB_SQUARES[king_to_square] & to_mask) == 0)
                return;

            const auto path = between(king_square, king_to_square) | BB_SQUARES[king_to_square];

            /* is any square in king's path under attack? */
            if (for_each_square_r<bool>(path, [&](Square sq) {
                return attackers_mask(!turn, sq) != 0;
            }))
                return;

            add_move(moves, king_square, king_to_square);
        });
    }


    void State::ep_moves(MovesList& moves, Bitboard to_mask) const
    {
        if (en_passant_square < 0 || (BB_SQUARES[en_passant_square] & to_mask)==0)
            return;

        if (BB_SQUARES[en_passant_square] & occupied())
            return;

        auto capturers = pawns & occupied_co(turn) &
            BB_PAWN_ATTACKS[!turn][en_passant_square] & BB_RANKS[turn == WHITE ? 4 : 3];

        for_each_square(capturers, [&](Square capturer) {
            moves.emplace_back(Move(capturer, en_passant_square));
        });
    }


    score_t State::eval() const
    {
        auto value = simple_score ? simple_score : eval_simple();

    #if EVAL_MOBILITY
        value += eval_mobility();
    #endif /* EVAL_MOBILITY */

        return value * SIGN[turn];
    }


    /*
     * Evaluate the incremental score change for a move
     */
    score_t State::eval_delta(Square from_square, Square to_square) const
    {
        const auto endgame = is_endgame();
        score_t delta = 0;

        const auto pi = piece_at(from_square);
        const auto i = square_index(from_square, pi.second);
        const auto j = square_index(to_square, pi.second);

        if (pi.first)
        {
            const auto& table = select_piece_square_table(endgame, pi.first);

            /* pi.second == Color */
            delta = SIGN[pi.second] * (table[j] - table[i]);
        }

        /* capture? subtract the value of the captured piece */
        const auto pj = piece_at(to_square);
        if (pj.first)
        {
            int d = weight(pj.first);

            const auto& table = select_piece_square_table(endgame, pj.first);

            /* subtract the piece-square value */
            d += table[square_index(to_square, pj.second)];

            delta -= SIGN[pj.second] * d; /* subtract weight of captured */
        }

        return delta;
    }


    score_t State::eval_material() const
    {
        score_t score = 0;

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


    score_t State::eval_simple() const
    {
        score_t score = 0;
        const auto endgame = is_endgame();

        for (auto color: {BLACK, WHITE})
        {
            const auto sign = SIGN[color];

            for (auto piece_type : PIECES)
            {
                const auto mask = pieces_mask(piece_type, color);
                score += sign * weight(piece_type) * popcount(mask);

                const auto& table = select_piece_square_table(endgame, piece_type);

                for_each_square(mask, [&](Square square) {
                    score += sign * table[square_index(square, color)];
                });
            }
        }

        return score;
    }


    /*
     * Use the number of potential "targets" to gauge the relative strengths of bishops.
     *
     * The difference is calculated only for bishops of colors that both sides have,
     * to maximize orthogonality with material and mobility evaluation terms.
     */
    int State::diff_bishops_strength() const
    {
        int score = 0;

        for (auto mask : { BB_LIGHT_SQUARES, BB_DARK_SQUARES })
        {
            if (bool(bishops & mask & white) == bool(bishops & mask & black))
            {
                for_each_square(bishops & mask, [&](Square square)
                {
                    const auto square_mask = BB_SQUARES[square];
                    const auto color = (white & square_mask) ? WHITE : BLACK;

                    score += SIGN[color] * popcount(occupied_co(!color) & mask);
                });
            }
        }

        return score;
    }


    int State::diff_bishop_pairs() const
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


    int State::diff_doubled_pawns() const
    {
        int count[] = {0, 0};

        for (const auto& bb_file : BB_FILES)
        {
            for (auto color : { BLACK, WHITE })
            {
                auto n = popcount(pawns & bb_file & occupied_co(color));
                if (n > 1)
                {
                    count[color] += n - 1;
                }
            }
        }
        return count[WHITE] - count[BLACK];
    }


    bool State::is_castling(const BaseMove& move) const
    {
        if (castling_rights == BB_EMPTY)
        {
            return false;
        }

        if (kings & BB_SQUARES[move.from_square()])
        {
            const auto diff = square_file(move.from_square()) - square_file(move.to_square());
            return abs(diff) > 1 || (rooks & occupied_co(turn) & BB_SQUARES[move.to_square()]) != 0;
        }
        return false;
    }


    bool State::is_en_passant(const BaseMove& move) const
    {
        return en_passant_square == move.to_square()
            && (pawns & BB_SQUARES[move.from_square()])
            && (abs(move.to_square() - move.from_square()) == 7 || abs(move.to_square() - move.from_square()) == 9)
            && (occupied() & BB_SQUARES[move.to_square()]) == 0;
    }


    uint64_t State::hash() const
    {
        if (_hash == 0)
            _hash = zobrist_hash(*this);

        return _hash;
    }


    bool State::has_connected_rooks(Color color) const
    {
        auto rook_mask = rooks & occupied_co(color);

        return for_each_square_r<bool>(rook_mask, [&](Square rook) {
            return attacks_mask(rook) & rook_mask;
        });
    }


    bool State::has_fork(Color color) const
    {
        const auto attacked = occupied_co(!color) & ~pawns;

        return for_each_square_r<bool>(occupied_co(color) & (pawns | knights | bishops),
            [&](Square attacking_square) {
                return popcount(attacks_mask(attacking_square) & attacked) > 1;
            });
    }


    // Ported from python-chess.
    // https://python-chess.readthedocs.io/en/latest/_modules/chess.html#Board.has_insufficient_material
    bool State::has_insufficient_material(Color color) const
    {
        if (_occupied_co[color] & (pawns | rooks | queens))
            return false;

        // Knights are only insufficient material if:
        // (1) We do not have any other pieces, including more than one knight.
        // (2) The opponent does not have pawns, knights, bishops or rooks.
        if (_occupied_co[color] & knights)
            return popcount(_occupied_co[color]) <= 2
                && (_occupied_co[!color] & ~kings & ~queens) == 0;

        // Bishops are only insufficient material if:
        // (1) We do not have any other pieces, including bishops of the opposite color.
        // (2) The opponent does not have bishops of the opposite color, pawns or knights.
        if (_occupied_co[color] & bishops)
        {
            auto same_color = (bishops & BB_DARK_SQUARES) == 0 || (bishops & BB_LIGHT_SQUARES) == 0;
            return same_color && !pawns && !knights;
        }

        return true;
    }


    bool State::is_pinned(Color color, bool threat) const
    {
        return for_each_square_r<bool>(occupied_co(color), [&] (Square square) {

            auto piece_type = piece_type_at(square);

            return (piece_type != PAWN) && (piece_type != KING)
                && (pin_mask(color, square, threat) != BB_ALL);
        });
    }


    bool State::is_capture(const BaseMove& move) const
    {
        ASSERT (move);
        return (BB_SQUARES[move.to_square()] & occupied_co(!turn)) || is_en_passant(move);
    }


    /* static */ bool State::is_endgame(const State& state)
    {
        return (popcount(state.occupied()) <= ENDGAME_PIECE_COUNT)
            || ((state.knights | state.bishops | state.rooks | state.queens) == 0);
    }


    bool State::is_endgame() const
    {
        if (_endgame == ENDGAME_UNKNOWN)
        {
            _endgame = is_endgame(*this) ? ENDGAME_TRUE : ENDGAME_FALSE;
        }
        return (_endgame == ENDGAME_TRUE);
    }


    int State::longest_pawn_sequence(Bitboard mask) const
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


    Bitboard State::passed_pawns(Color color, Bitboard mask) const
    {
        static constexpr Bitboard BB_PASSED_RANKS[] = {
            BB_RANK_4 | BB_RANK_3 | BB_RANK_2,
            BB_RANK_5 | BB_RANK_6 | BB_RANK_7,
        };

        return pawns & occupied_co(color) & BB_PASSED_RANKS[color] & mask;
    }
}
