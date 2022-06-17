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
/* Generates the attacks.h file */

#include "chess.h"
#include "hash_builder.h"

using namespace chess;
using namespace perfect_hash;


static bool build_attack_tables(
    const char*             name,
    const std::vector<int>& deltas,
    HashBuilder&            builder,
    CodeGenerator&          code)
{
    for (int square = 0; square < 64; ++square)
    {
        std::unordered_map<Bitboard, Bitboard> attacks;

        auto mask = sliding_attacks(square, 0, deltas) & ~edges(square);
        for_each_subset(mask, [&](Bitboard subset) {
            attacks.emplace(subset, sliding_attacks(square, subset, deltas));
        });

        if (!builder.build(code, name, attacks))
        {
            std::cerr << "Could not build hash function: " << name << "[" << square << "]\n";
            return false;
        }

        std::clog << name << "[" << square << "]: " << builder.strategy() << "\n";
    }

    return true;
}


int main(int argc, const char* argv[])
{
    CodeGenerator code;
    CompositeHashBuilder builder;

    chess::_init();

    if (build_attack_tables("_DIAG_ATTACKS", {-9, -7, 7, 9}, builder, code)
     && build_attack_tables("_FILE_ATTACKS", {-8, 8}, builder, code)
     && build_attack_tables("_RANK_ATTACKS", {-1, 1}, builder, code))
    {
        code.write(std::cout);
        return 0;
    }
    return -1;
}