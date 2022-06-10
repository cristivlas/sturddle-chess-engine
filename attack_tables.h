#pragma once
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
#include "common.h"

namespace chess
{
    enum class AttacksType : int
    {
        Diag,
        File,
        Rank,
    };

    namespace impl
    {
        class AttackTable
        {
            const int _strategy = -1;

        public:
            template<typename F> AttackTable(int strategy, F init)
                : _strategy(strategy)
            {
                init(_data);
            }

            uint64_t operator[] (uint64_t k) const;
            uint64_t* _data = nullptr;
        };

    } /* namespace impl */


    template<AttacksType> struct Attacks {};

} /* namespace chess */


#if TESTGEN
  #include "codegen/test.h"
#else
  #include "attacks.h"
#endif

INLINE uint64_t chess::impl::AttackTable::operator[] (uint64_t k) const
{
    return _data[chess::impl::hash(_strategy, k)];
}

namespace chess
{
    static Attacks<AttacksType::Diag> BB_DIAG_ATTACKS;
    static Attacks<AttacksType::File> BB_FILE_ATTACKS;
    static Attacks<AttacksType::Rank> BB_RANK_ATTACKS;

} /* namespace chess */
