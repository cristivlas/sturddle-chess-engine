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
 * Approximate evaluation of captures and exchanges (naive SEE).
 * https://www.chessprogramming.org/Static_Exchange_Evaluation
 */
#include <algorithm>
#include <iterator>
#include "common.h"
#include "chess.h"
#include "utility.h"

namespace chess
{
    /* Get attackers of given color targetting a square, discounting pinned pieces */
    INLINE static Bitboard get_direct_attackers(
        const State&    board,
        Color           color,
        Square          target_square,
        int             depth,
        Bitboard        counterattacks)
    {
        auto attackers_mask = board.attackers_mask(color, target_square);

        if (color == board.turn && board.is_check())
        {
            attackers_mask &= board.kings;
        }

        if (depth > SEE_PIN_AWARENESS_DEPTH)
        {
            return attackers_mask;
        }

        for_each_square(attackers_mask, [&](Square square)
        {
            auto pin_mask = board.pin_mask(color, square);

            if (pin_mask == BB_ALL)
                return; /* not pinned */

            if (BB_SQUARES[target_square] & pin_mask)
                return; /* pinned by the target */

            /* unless there's one single counterattacker pinning us...
               (moving to capture is likely to remove the pin)
               or we're pinned by other pieces than the counterattackers */
            if (popcount(counterattacks) > 1 || (counterattacks & pin_mask) == 0)
                /* then remove the pinned attacker */
                attackers_mask &= ~BB_SQUARES[square];
        });

        return attackers_mask;
    }


    struct Attack
    {
        using Support = std::vector<Attack>;
        Square      _from;
        PieceType   _type;
        Support     _support;

        Attack(Square from_square, PieceType piece_type)
            : _from(from_square)
            , _type(piece_type)
        {
        }

        Attack(const Attack&) = default;
        Attack& operator=(const Attack&) = default;
    };


    inline bool greater_than(const Attack& lhs, const Attack& rhs)
    {
        return lhs._type > rhs._type;
    }


    std::ostream& operator <<(std::ostream& os, const Attack& attack)
    {
        return os << '[' << square_name(attack._from) << ": " << piece_name(attack._type) << ']';
    }


    std::ostream& operator <<(std::ostream& os, const std::vector<Attack>& attacks)
    {
        std::copy(attacks.cbegin(), attacks.cend(), std::ostream_iterator<Attack>(os, " "));
        return os;
    }


    class ExchangePlan
    {
    public:
        using AttackVector = std::vector<Attack>;

        INLINE const AttackVector& attack_plan(
            const State&    board_state,
            Color           color,
            Square          square,
            Bitboard*       counterattacks)
        {
            _attackers.clear();
            _unsorted_attackers.clear();
            get_attacks(board_state, color, square, BB_ALL, 0, _unsorted_attackers, counterattacks);
            sort_attacks(_unsorted_attackers, _attackers);
            return _attackers;
        }

        INLINE const AttackVector& defense_plan(
            const State&    board_state,
            Color           color,
            Square          square,
            Bitboard*       counterattacks)
        {
            _defenders.clear();
            _unsorted_defenders.clear();
            get_attacks(board_state, color, square, BB_ALL, 0, _unsorted_defenders, counterattacks);
            sort_attacks(_unsorted_defenders, _defenders);
            return _defenders;
        }

    private:
        /* Get attacks from pieces of given color to target square */
        static void get_attacks(
            const State&    board,
            Color           color,
            Square          target_square,
            Bitboard        mask,
            int             depth,
            AttackVector&   attacks,
            Bitboard*       counter_attack_mask = nullptr)
        {
            auto counterattacks = counter_attack_mask ? *counter_attack_mask : BB_EMPTY;
            auto attackers_mask =
                get_direct_attackers(board, color, target_square, depth, counterattacks) & mask;

            if (counter_attack_mask)
                *counter_attack_mask = attackers_mask;

            attacks.reserve(popcount(attackers_mask));

            mask &= ~(attackers_mask | board.pawns | board.kings);

            for_each_square(attackers_mask, [&] (Square square)
            {
                attacks.emplace_back(square, board.piece_type_at(square));

                if (BB_SQUARES[square] & (board.pawns | board.bishops | board.rooks | board.queens))
                {
                    auto rays = BB_RAYS[target_square][square] & ~BB_SQUARES[target_square];
                    get_attacks(board, color, square, mask & rays, depth + 1, attacks.back()._support);
                }
            });
        }


        INLINE static void sort_attacks(AttackVector& attacks, AttackVector& plan)
        {
            plan.reserve(attacks.size());

            /*
             * sort in descending order so that pop_back() gets the least valuable piece.
             */
            insertion_sort(attacks.begin(), attacks.end(), greater_than);

            while (!attacks.empty())
            {
                auto a = attacks.back();
                attacks.pop_back();

                plan.emplace_back(a);
                if (!a._support.empty())
                {
                    attacks.insert(attacks.end(), a._support.cbegin(), a._support.cend());
                    insertion_sort(attacks.begin(), attacks.end(), greater_than);
                }
            }
        }

        AttackVector    _attackers;
        AttackVector    _defenders;
        AttackVector    _unsorted_attackers;
        AttackVector    _unsorted_defenders;
    };


    THREAD_LOCAL ExchangePlan exchanges;


    score_t estimate_static_exchanges(const State& board, Color color, Square square, PieceType piece_type)
    {
        score_t value = 0;
        Bitboard attack_mask = BB_EMPTY;

        if (piece_type == NONE)
        {
            piece_type = board.piece_type_at(square);
            ASSERT(piece_type);
        }

        const auto& attackers = exchanges.attack_plan(board, color, square, &attack_mask);

        if (!attackers.empty())
        {
            value = board.weight(piece_type);

            const auto& defenders = exchanges.defense_plan(board, !color, square, &attack_mask);

            if (!defenders.empty())
            {
                int n_attackers = int(attackers.size());
                int n_defenders = int(defenders.size());

                if (n_attackers > n_defenders)
                {
                    /* if there are more attackers than defenders, then the king
                       may not be able to defend, as doing may place it in check */

                    if (defenders.back()._type == KING)
                        --n_defenders;
                }

                /* ... and similarly for attacks */
                if (n_defenders >= n_attackers && attackers.back()._type == KING)
                {
                    --n_attackers;
                    ASSERT(n_attackers >= 0);
                }

                int n = 0;
                while (n < n_defenders)
                {
                    /* assume defender recaptures */
                    value -= board.weight(attackers[n]._type);
                    ++n;

                    if (n < n_attackers && value <= board.weight(BISHOP) - board.weight(KNIGHT))
                    {
                        /* attacker recaptures */
                        value += board.weight(defenders[n-1]._type);
                    }
                    else
                        break;
                }

                /*
                 * Rewind to the point where the capture gain
                 * is above the value of the initial target.
                 */
                while (n > 0 && value > board.weight(piece_type))
                {
                    value -= board.weight(defenders[n-1]._type);
                    --n;
                    ASSERT(n >= 0);
                    value += board.weight(attackers[n]._type);
                }

                ASSERT (value > -WEIGHT[KING]);
                ASSERT (value < WEIGHT[KING]);
            }
        }

        return value > 0 ? value : 0;
    }


    score_t estimate_captures(const State& board)
    {
        score_t value = 0;

        const auto color = board.turn;

        for (auto piece_type : {QUEEN, ROOK, BISHOP, KNIGHT, PAWN})
        {
            if (value >= board.weight(piece_type))
                break;

            const auto victims = board.pieces_mask(piece_type, Color(color ^ 1));

            for_each_square(victims, [&](Square square)
            {
                auto v = estimate_static_exchanges(board, color, square, piece_type);
                value = std::max(value, v);
            });
        }

        return value;
    }

} /* namespace chess */

