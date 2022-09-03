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
#include "chess.h"

#if USE_MAGIC_BITS
const magic_bits::Attacks magic_bits_attacks;
#endif /* USE_MAGIC_BITS */


namespace chess
{
#if MOBILITY_TUNING_ENABLED

    int MOBILITY[] = DEFAULT_MOBILITY_WEIGHTS;

#endif /* MOBILITY_TUNING_ENABLED */

    AttackMasks BB_DIAG_MASKS, BB_FILE_MASKS, BB_RANK_MASKS;

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


    std::string square_name(Square s)
    {
        if (s == UNDEFINED)
            return "n/a";
        const char name[] = {char('a' + square_file(s)), char('1' + square_rank(s)), 0};
        return name;
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


    static Bitboard step_attacks(int square, const std::vector<int>& deltas)
    {
        return sliding_attacks(square, BB_ALL, deltas);
    }


    static Rays init_rays()
    {
        Rays rays;

        for (int a = 0; a < 64; ++a)
        {
            const auto bb_a = BB_SQUARES[a];
            RaysRow rays_row;

            for (int b = 0; b < 64; ++b)
            {
                const auto bb_b = BB_SQUARES[b];

                if (BB_DIAG_ATTACKS.get(a, 0) & bb_b)
                    rays_row[b] = ((BB_DIAG_ATTACKS.get(a, 0) & BB_DIAG_ATTACKS.get(b, 0)) | bb_a | bb_b);

                else if (BB_RANK_ATTACKS.get(a, 0) & bb_b)
                    rays_row[b] = (BB_RANK_ATTACKS.get(a, 0) | bb_a);

                else if (BB_FILE_ATTACKS.get(a, 0) & bb_b)
                    rays_row[b] = (BB_FILE_ATTACKS.get(a, 0) | bb_a);

                else
                    rays_row[b] = BB_EMPTY;
            }
            rays[a] = rays_row;
        }

        return rays;
    }


    using AttackTable = impl::AttackTable;

    template<typename T> static void
    init_attack_masks(AttackMasks& mask_table, const T& tables, const std::vector<int>& deltas)
    {
        for (int square = 0; square < 64; ++square)
        {
            auto mask = sliding_attacks(square, 0, deltas) & ~edges(square);
            mask_table[square] = mask;

            /* validate the generated attack tables */
            for_each_subset(mask, [&](Bitboard key)
            {
                const auto value = sliding_attacks(square, key, deltas);
                ASSERT_ALWAYS(tables.get(square, key) == value);
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

        BB_RAYS = init_rays();
    }


    template<typename T>
    INLINE void add_move(T& container, Square from_square, Square to_square, PieceType promo)
    {
        container.emplace_back(from_square, to_square, promo);
    }


    template<typename T>
    INLINE void add_move(T& container, Square from_square, Square to_square)
    {
        container.emplace_back(from_square, to_square);
    }


    template<typename T>
    INLINE void add_pawn_moves(T& moves_list, Square from_square, Square to_square)
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

        const auto occupied = this->occupied();

        /*
         * Step 1, build some tables...
         */
        for (const auto color: {BLACK, WHITE})
        {
            const auto our_pieces = this->occupied_co(color);

            for_each_square(our_pieces, [&](Square square) {
                const auto piece_type = piece_type_at(square);
                auto piece_attacks = attacks_mask(square, occupied);

                checks_from[color][square] = piece_attacks;
                checks[color] |= piece_attacks;

                const Bitboard mask = (piece_type == KING) ? BB_ALL : pin_mask(color, square);
                piece_attacks &= mask; /* can only attack in the direction of the pin */
                pinned[color] |= (mask != BB_ALL) * BB_SQUARES[square];

                attacks[color][piece_type] |= piece_attacks;
                attacks_from[color][square] = piece_attacks & ~(kings | our_pieces);
            });
        }
        /*
         * Step 2, count where pieces can go without being captured by the
         * king or pieces of lower values, king not allowed to move into check.
         */
        for (const auto color: {BLACK, WHITE})
        {
            if constexpr(MOBILITY_TUNING_ENABLED || MOBILITY[PieceType::PAWN] != 0)
            {
                /* pawns moves... */
                const auto own_pawns = pawns & occupied_co(color) & ~pinned[color];

                Bitboard single_pawn_moves, double_pawn_moves;

                if (color)
                {
                    single_pawn_moves = shift_up(own_pawns) & ~occupied;
                    double_pawn_moves = shift_up(single_pawn_moves) & ~occupied & (BB_RANK_3 | BB_RANK_4);
                }
                else
                {
                    single_pawn_moves = shift_down(own_pawns) & ~occupied;
                    double_pawn_moves = shift_down(single_pawn_moves) & ~occupied & (BB_RANK_6 | BB_RANK_5);
                }

                /* ... and captures */
                const auto pawn_captures = attacks[color][PieceType::PAWN] & occupied_co(!color);

                mobility += SIGN[color] * MOBILITY[PieceType::PAWN]
                    * (popcount(single_pawn_moves | double_pawn_moves) + popcount(pawn_captures));
            }

            /* non-pawns */
            for_each_square(occupied_co(color) & ~pawns, [&](Square square) {
                if (attacks_from[color][square] == BB_EMPTY)
                    return;

                const auto piece_type = piece_type_at(square);
                ASSERT(piece_type != PieceType::PAWN);
                if (MOBILITY[piece_type] == 0)
                    return;

                /* where can the enemy king capture? */
                Bitboard defended = attacks[!color][KING];
                for (int i = 0; i != 64; ++i)
                {
                    /* skip checks by the piece to move */
                    // if (i != square)
                    //     defended &= ~checks_from[color][i];
                    defended &= ~checks_from[color][i] | ((i == square) * BB_ALL);
                }
                switch (piece_type)
                {
                case PieceType::KNIGHT:
                case PieceType::BISHOP:
                    defended |= attacks[!color][PieceType::PAWN];
                    break;

                case PieceType::ROOK:
                case PieceType::QUEEN:
                    for (int i = PieceType::PAWN; i != piece_type; ++i)
                        defended |= attacks[!color][i];
                    break;

                case PieceType::KING:
                    defended |= checks[!color];
                default:
                    break;
                }

                mobility += SIGN[color] * MOBILITY[piece_type]
                    * popcount(attacks_from[color][square] & ~defended);
            });
        }

        return mobility;
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
                    return false; /* got one valid move, bail */
                }
            }
            return true;
        }
        return false;
    }


    void State::generate_moves(MovesList& moves, MovesList& buffer) const
    {
        State temp;
        moves.clear();

        for (const auto& move : generate_pseudo_legal_moves(buffer))
        {
            clone_into(temp);

            temp.apply_move(move);

            if (!temp.is_check(turn))
                moves.emplace_back(move);
        }
    }


    const MovesList&
    State::generate_pseudo_legal_moves(
        MovesList&  moves_list,
        Bitboard    to_mask,
        Bitboard    from_mask) const
    {
        moves_list.clear();

        to_mask &= ~kings;

        const auto our_pieces = this->occupied_co(turn);
        const auto occupied = this->occupied();

        /* Piece moves. */
        if (const auto non_pawns = our_pieces & ~pawns & from_mask)
            for_each_square(non_pawns, [&](Square from_square) {
                const auto moves = attacks_mask(from_square, occupied) & ~our_pieces & to_mask;
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

            Bitboard single_moves, double_moves;
            if (turn)
            {
                single_moves = (our_pawns << 8) & ~occupied;
                double_moves = (single_moves << 8) & ~occupied & (BB_RANK_3 | BB_RANK_4);
            }
            else
            {
                single_moves = (our_pawns >> 8) & ~occupied;
                double_moves = (single_moves >> 8) & ~occupied & (BB_RANK_6 | BB_RANK_5);
            }

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

        const auto occupied = this->occupied();

        /* king in check? */
        if (attackers_mask(!turn, king_square, occupied))
            return;

        for_each_square((rooks & occupied_co(turn) & castling_rights), [&](Square rook_square)
        {
            /* any pieces between the king and the rook? */
            if (between(king_square, rook_square) & occupied)
                return;

            const auto rook_file = square_file(rook_square);
            ASSERT(rook_file == 0 || rook_file == 7);

            const auto king_to_square = Square(msb(backrank & (rook_file ? BB_FILE_G : BB_FILE_C)));

            if ((BB_SQUARES[king_to_square] & to_mask) == 0)
                return;

            const auto path = between(king_square, king_to_square) | BB_SQUARES[king_to_square];

            /* is any square in king's path under attack? */
            if (for_each_square_r<bool>(path, [&](Square sq) {
                return attackers_mask(!turn, sq, occupied) != 0;
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
            moves.emplace_back(capturer, en_passant_square);
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


    /* for testing */
    size_t State::make_pseudo_legal_moves(MovesList& moves) const
    {
        size_t count = 0;
        generate_pseudo_legal_moves(moves);

        for (const auto& move : moves)
        {
            State state = clone();
            state.apply_move(move);

            if (!state.is_check(!state.turn))
                ++count;
        }

        return moves.size();
    }
}
