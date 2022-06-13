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
 * Part of the code generator for attacks.h
 * Find fast perfect hash functions by trial and error.
 */
#include <iomanip>
#include <map>
#include <memory>
#include <ostream>
#include <unordered_map>
#include <vector>
#include <sstream>
#include <string>

#include "common.h"
#include "perfect_hash.h"


namespace
{
    using TableData = std::vector<uint64_t>;


    class CodeGenerator
    {
        struct Group
        {
            std::vector<std::string> _hash_funcs;
            std::vector<std::string> _init_funcs;
        };

    public:
        void add_hash_template(const std::string& hasher_template)
        {
            _templates.insert(hasher_template);
        }

        void add_instance(const char* name, const std::string& hf, const TableData& data)
        {
            auto& g = _groups[name];

            g._init_funcs.emplace_back(generate_data_init(data));
            g._hash_funcs.emplace_back(hf);

            _hash_funcs.emplace(hf, _hash_funcs.size());
            _hash_funcs_index.emplace(_hash_funcs[hf], hf);
        }

        void write(std::ostream& os) const
        {
            os << "/* Auto-generated attack tables */\n";
            os << "#pragma once\n\n";
            os << "#include <cstdint>\n";
            os << "#include <cstdlib>\n";

            os << "\nnamespace chess {\n";
            os << "\nnamespace impl {\n";

            for (const auto& c : _templates)
            {
                os << c << "\n";
            }
            os << "\n";

            generate_unified_hash_function(os);

            os << "\n";
            os << "/************************************************************/\n";
            os << "/* Tables */\n";
            os << "/************************************************************/\n";

            os << "class AttackTable\n";
            os << "{\n";
            os << "    const int _strategy = -1;\n";
            os << "\n";
            os << "public:\n";
            os << "    template<typename F> AttackTable(int s, F init) : _strategy(s)\n";
            os << "    {\n";
            os << "        init(_data);\n";
            os << "    }\n\n";
            os << "    INLINE uint64_t operator[] (uint64_t mask) const\n";
            os << "    {\n";
            os << "        return _data[chess::impl::hash(_strategy, mask)];\n";
            os << "    }\n\n";
            os << "    uint64_t* _data = nullptr;\n";
            os << "};\n\n";

            for (const auto& elem : _groups)
            {
                const auto& group = elem.second;

                ASSERT (group._hash_funcs.size()==64);

                os << "const AttackTable " << elem.first;
                os << "[" << group._hash_funcs.size() << "] = {\n";

                write(group, os);

                os << "};\n\n";
            }
            os << "} /* namespace impl */\n\n";

            os << "/************************************************************/\n";
            os << "/* Template specializations */\n";
            os << "/************************************************************/\n";

            std::unordered_map<std::string, std::string> type = {
                { "_FILE_ATTACKS", "AttacksType::File" },
                { "_RANK_ATTACKS", "AttacksType::Rank" },
                { "_DIAG_ATTACKS", "AttacksType::Diag" }
            };

            for (const auto& elem : _groups)
            {
                os << "\ntemplate<> struct Attacks<" << type[elem.first] << ">\n";
                os << "{\n";
                os << "    INLINE static uint64_t get(int square, uint64_t mask)\n";
                os << "    {\n";
                os << "        return impl::" << elem.first << "[square][mask];\n";
                os << "    }\n";
                os << "};\n";
            }

            os << "} /* namespace chess */\n";
        }

    private:
        void write(const Group& g, std::ostream& os) const
        {
            for (size_t i = 0; i != g._hash_funcs.size(); ++i)
            {
                const auto index = _hash_funcs.at(g._hash_funcs[i]);
                os << "    AttackTable(" << index << ", " << g._init_funcs[i] << "),\n";
            }
        }

        std::string generate_data_init(const TableData& data) const
        {
            std::ostringstream os;

            os << "[](uint64_t *& data) {\n";
            os << "        data = new uint64_t [" << data.size() + 1 << "]();\n";

            for (size_t i = 0; i != data.size(); ++i)
            {
                if (data[i] == 0)
                    continue;

                os << "        data[" << i << "] = " << "0x" << std::hex;

                os << std::setw(16) << std::setfill<char>('0') << data[i];
                os << std::dec << "ULL;\n";
            }
            os << "    }";
            return os.str();
        }

        void generate_unified_hash_function(std::ostream& os) const
        {
            os << "/************************************************************/\n";
            os << "/* Unified hash function. */\n";
            os << "/************************************************************/\n";
            os << "INLINE size_t hash(int i, uint64_t u)\n";
            os << "{\n";
            os << "    switch (i)\n";
            os << "    {\n";
            for (const auto& index : _hash_funcs_index)
            {
                os << "    case " << index.first << ":\n";
                os << "        return " << index.second << "(u);\n";
            }

            os << "    }\n";
            os << "    return 0;\n";
            os << "}\n\n";
        }

    private:
        std::unordered_map<std::string, int> _hash_funcs;
        std::map<int, std::string> _hash_funcs_index;

        std::set<std::string> _templates; /* hasher templates */
        std::unordered_map<std::string, Group> _groups;
    };


    /* Interface */
    class HashBuilder
    {
    public:
        using Input = std::unordered_map<uint64_t, uint64_t>;

        virtual ~HashBuilder() = default;

        virtual std::string strategy() const = 0;

        virtual bool build(
            CodeGenerator&  code,
            const char*     name,
            const Input&    data) = 0;

        virtual void write_hash_template(CodeGenerator&) const = 0;

        virtual void write_instance(
            CodeGenerator&  code,
            const char*     name,
            const Input&    data) const = 0;
    };


    class HashBuilderBase : public HashBuilder
    {
    protected:
        mutable size_t _data_size = 0;
        virtual size_t hash(uint64_t) const = 0;
        virtual bool build_impl(const Input& input) = 0;

        bool build(CodeGenerator& code, const char* name, const Input& input) override final
        {
            if (build_impl(input))
            {
                write_hash_template(code);
                write_instance(code, name, input);
                return true;
            }
            return false;
        }

        TableData transform(const Input& input, size_t max_table_size) const
        {
            TableData data;

            for (const auto& elem : input)
            {
                const auto i = hash(elem.first);
                ASSERT(i < max_table_size);
                if (i >= data.size())
                    data.resize(i + 1);
                data[i] = elem.second;
            }
            ASSERT(data.size() <= max_table_size);

            _data_size = data.size();
            return data;
        }
    };


    class PerfectHashBuilder : public HashBuilderBase
    {
        perfect_hash::HashMatrix _hm;

        std::string strategy() const override
        {
            return "perfect hash";
        }

        bool build_impl(const Input& input) override
        {
            _hm = perfect_hash::HashMatrix();
            return perfect_hash::generate(input, _hm);
        }

        size_t hash(uint64_t k) const override final
        {
            return _hm.hash(k);
        }

        void write_hash_template(CodeGenerator& code) const override
        {
            std::ostringstream os;

            os << "static inline uint64_t bitparity(uint64_t u) {\n";
            os << "#if _MSC_VER\n";
            os << "    return __popcnt64(u) % 2;\n";
            os << "#else\n";
            os << "    static_assert(__GNUC__, \"unsupported compiler\");\n";
            os << "    return __builtin_parityll(u);\n";
            os << "#endif\n";
            os << "}\n\n";
            os << "template<int B, uint64_t R> struct RHash {\n";
            os << "    static constexpr inline size_t hash(uint64_t n) {\n";
            os << "        return RHash<B-1, R>::_hash(n);\n";
            os << "    }\n";
            os << "    static constexpr inline size_t _hash(uint64_t n) {\n";
            os << "         return bitparity(R & n) | (RHash<B-1, (R >> 1)>::_hash(n) << 1);\n";
            os << "    }\n";
            os << "};\n\n";
            os << "template<uint64_t R> struct RHash<0, R> {\n";
            os << "    static constexpr inline size_t _hash(uint64_t n) {\n";
            os << "        return bitparity(R & n);\n";
            os << "    }\n";
            os << "};\n\n";

            code.add_hash_template(os.str());
        }

        void write_instance(CodeGenerator& code, const char* name, const Input& input) const override
        {
            std::ostringstream os;

            os << "RHash<" << _hm.size() << ", " << _hm.r() << "ULL>::hash";
            const auto max_table_size = 1ULL << _hm.size();
            code.add_instance(name, os.str(), transform(input, max_table_size));
        }
    };


    class Hash64Builder : public HashBuilderBase
    {
    protected:
        bool build_impl(const Input& input) override final
        {
            std::unordered_map<size_t, uint64_t> mapping;

            for (const auto& elem : input)
            {
                auto hval = hash(elem.first);
                auto iter = mapping.find(hval);
                if (iter == mapping.end())
                    mapping.emplace(hval, elem.second);
                else if (iter->second != elem.second)
                    return false;
            }
            return true;
        }
    };

    /*----------------------------------------------------------------------*/
    /* Mixins                                                               */
    /*----------------------------------------------------------------------*/
    struct Identity
    {
        static uint64_t hash(uint64_t key) { return key; }

        static std::string mixin()
        {
            std::ostringstream os;
            os << "inline uint64_t " << name() << "(uint64_t key) {\n";
            os << "    return key;\n";
            os << "}\n";
            return os.str();
        }

        static std::string name() { return "id" ; }

        static void add(CodeGenerator& code) { code.add_hash_template(mixin()); }
    };


    template<uint64_t M> struct Multiply
    {
        static uint64_t hash(uint64_t key) { return key * M; }

        static std::string mixin()
        {
            std::ostringstream os;
            os << "inline uint64_t " << name() << "(uint64_t key) {\n";
            os << "    return key * " << M << "ULL;\n";
            os << "}\n";
            return os.str();
        }

        static std::string name() { return "mul_" + std::to_string(M); }

        static void add(CodeGenerator& code) { code.add_hash_template(mixin()); }
    };


    template<typename BaseMixin, size_t N> struct RightShiftMixin
    {
        static uint64_t hash(uint64_t key)
        {
            return BaseMixin::hash(key) >> N;
        }

        static std::string mixin()
        {
            std::ostringstream os;

            os << "inline uint64_t " << name() << "(uint64_t key) {\n";
            os << "    return " << BaseMixin::name() << "(key) >> " << N << ";\n";
            os << "}\n";
            return os.str();
        }

        static std::string name()
        {
            return BaseMixin::name() + std::string("_rshift_") + std::to_string(N);
        }

        static void add(CodeGenerator& code)
        {
            BaseMixin::add(code);
            code.add_hash_template(mixin());
        }
    };


    /*----------------------------------------------------------------------*/
    /* Builders                                                             */
    /*----------------------------------------------------------------------*/

    /*
     * Wrap a mixin and apply mask.
     */
    template<typename Mixin> class MixinHashBuilder : public Hash64Builder
    {
        const size_t _max_table_size;
        std::ostringstream _name;

    public:
        explicit MixinHashBuilder(size_t max_table_size) : _max_table_size(max_table_size)
        {
            _name << Mixin::name() << "_" << _max_table_size;
        }

    private:
        std::string strategy() const override
        {
            return _name.str();
        }

        size_t hash(uint64_t key) const override final
        {
            return Mixin::hash(key) & (_max_table_size - 1);
        }

        void write_hash_template(CodeGenerator& code) const override
        {
            Mixin::add(code);

            std::ostringstream os;
            os << "template<size_t Mask>\n";
            os << "inline size_t " << Mixin::name() << "_mixin(uint64_t key) {\n";
            os << "    return " << Mixin::name() << "(key) & Mask;\n";
            os << "}\n";
            code.add_hash_template(os.str());
        }

        void write_instance(CodeGenerator& code, const char* name, const Input& input) const override
        {
            std::ostringstream os;
            os << Mixin::name() << "_mixin<0x" << std::hex << (_max_table_size - 1) << std::dec << ">";
            code.add_instance(name, os.str(), transform(input, _max_table_size));
        }
    };


    /* Apply base Mixin, then RightShift, then mask. */
    template<typename Mixin, size_t S>
    using HashMaskBuilder = MixinHashBuilder<RightShiftMixin<Mixin, S>>;


    class CompositeHashBuilder : public HashBuilder
    {
        std::vector<std::unique_ptr<HashBuilder>> _builders;
        std::string _name = "composite"; /* name of last strategy used */

        /*
         * utils for adding strategies to _builders
         */
        /* wrap mixin M in HashMaskBuilder and add it to builders */
        template<typename M, size_t S>
        void add_hash_mask_builder(size_t table_size)
        {
            _builders.emplace_back(std::make_unique<HashMaskBuilder<M, S>>(table_size));
        }

        /* add group of HashMaskBuilder-wrapped mixins, for a variadic list of right shifts */
        template<typename M, size_t... I>
        void add_group(size_t table_size, std::integer_sequence<size_t, I...>)
        {
            (add_hash_mask_builder<M, I>(table_size), ...);
        }

        template<typename M> void add_group(size_t table_size)
        {
            add_group<M>(table_size, std::make_integer_sequence<size_t, 64>{});
        }

        template<uint64_t... M>
        void add_multipliers(size_t table_size, std::integer_sequence<uint64_t, M...>)
        {
            (add_group<Multiply<M>>(table_size), ...);
        }

        /*
         * Sources of ideas for some good multipliers:
         *   http://zimbry.blogspot.com/2011/09/better-bit-mixing-improving-on.html
         *   https://www.chessprogramming.org/Best_Magics_so_far
         */

        void add_multipliers(size_t table_size)
        {
            add_multipliers(table_size, std::integer_sequence<uint64_t,
                0x81dadef4bc2dd44d,
                0x54c77c86f6913e45,
                0x7fb5d329728ea185,
                0xe36aa5c613612997,
                0x64dd81482cbd31d7,
                0>{});
        }

    public:
        explicit CompositeHashBuilder()
        {
            /*
             * Try a bunch of strategies and see which one fits the data best.
             * The idea is to maximize performance with minimum memory.
             */

            for (size_t table_size : { 32, 64 })
                add_group<Identity>(table_size);

            for (size_t table_size : { 256, 512, 1024, 2048, 4096 })
                add_multipliers(table_size);

            /*
             * This is catch all strategy. Guaranteed to find perfect hashing, but slow.
             */
            _builders.emplace_back(std::make_unique<PerfectHashBuilder>());
        }

        std::string strategy() const override
        {
            return _name;
        }

        bool build(CodeGenerator& code, const char* name, const Input& input) override
        {
            _name = "composite";
            for (auto& builder : _builders)
            {
                if (builder->build(code, name, input))
                {
                    _name = builder->strategy();
                    return true;
                }
            }
            return false;
        }

        void write_hash_template(CodeGenerator&) const override
        {
            ASSERT(false);
        }

        void write_instance(CodeGenerator&, const char*, const Input&) const override
        {
            ASSERT(false);
        }
    };
}
