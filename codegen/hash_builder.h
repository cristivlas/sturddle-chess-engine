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
#include <memory>
#include <ostream>
#include <unordered_map>
#include <vector>
#include <sstream>
#include <string>

#include "perfect_hash.h"

#define USE_VECTOR false

namespace
{
    auto get_msb = chess::msb;
    auto get_lsb = chess::lsb;


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
            g._hash_funcs.push_back(hf);
        }

        void write(std::ostream& os) const
        {
            os << "/* Auto-generated attack tables */\n";
            os << "#pragma once\n\n";
            os << "#include <cstdint>\n";
            os << "#include <cstdlib>\n";
            os << "#include <functional>\n";
            os << "#include <vector>\n\n";

            os << "namespace chess {\n";
            write_table_class(os);

            for (const auto& c : _templates)
            {
                os << c << "\n";
            }

            os << "\n";
            for (const auto& elem : _groups)
            {
                const auto& group = elem.second;

                ASSERT (group._hash_funcs.size()==64);

                os << "static const AttackTable " << elem.first;
                os << "[" << group._hash_funcs.size() << "] = {\n";

                write(group, os);

                os << "};\n\n";
            }
            os << "} /* namespace chess */\n";
        }

    private:
        void write_table_class(std::ostream& os) const
        {
        #if USE_VECTOR
            os << "class AttackTable {\n";
            os << "    size_t (* const _hash)(uint64_t);\n";
            os << "    std::vector<uint64_t> _data;\n\n";
            os << "public:\n";
            os << "    AttackTable(size_t (*hash)(uint64_t), std::function<void(std::vector<uint64_t>&)> init)\n";
            os << "        : _hash(hash) { init(_data);\n";
            os << "    }\n";
            os << "    inline uint64_t operator[] (uint64_t k) const {\n";
            os << "        return _data[_hash(k)];\n";
            os << "    }\n";
            os << "};\n\n";
        #else
            os << "class AttackTable {\n";
            os << "    size_t (* const _hash)(uint64_t);\n";
            os << "    uint64_t* _data = nullptr;\n\n";
            os << "public:\n";
            os << "    AttackTable(size_t (*hash)(uint64_t), std::function<void(uint64_t*&)> init)\n";
            os << "        : _hash(hash) { init(_data);\n";
            os << "    }\n";
            os << "    inline uint64_t operator[] (uint64_t k) const {\n";
            os << "        return _data[_hash(k)];\n";
            os << "    }\n";
            os << "};\n\n";
        #endif
        }

        void write(const Group& g, std::ostream& os) const
        {
            for (size_t i = 0; i != g._hash_funcs.size(); ++i)
            {
                os << "    AttackTable(" << g._hash_funcs[i] << ", " << g._init_funcs[i] << "),\n";
            }
        }

        std::string generate_data_init(const TableData& data)
        {
            std::ostringstream os;

        #if USE_VECTOR
            os << "[](std::vector<uint64_t>& data) {\n";
            os << "        data.resize(" << data.size() << ");\n";
        #else
            os << "[](uint64_t *& data) {\n";
            os << "        data = new uint64_t [" << data.size() << "];\n";
        #endif
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

    private:
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
            return data;
        }
    };

    /* The "hash" function is simply a bitshift */
    class RShiftHashBuilder : public HashBuilderBase
    {
    public:
        RShiftHashBuilder(size_t max_table_bits) : _max_table_bits(max_table_bits)
        {
        }

    private:
        std::string strategy() const override
        {
            return "right_shift";
        }

        bool build_impl(const Input& input) override
        {
            reset();

            for (const auto& elem : input)
            {
                if (const auto key = elem.first)
                {
                    _lsb = std::min(_lsb, get_lsb(key));
                    _msb = std::max(_msb, get_msb(key));
                }
            }

            return bits_required_per_table() <= _max_table_bits;
        }

        size_t bits_required_per_table() const
        {
            ASSERT(_msb >= _lsb);
            return _msb - _lsb + 1;
        }

        void write_hash_template(CodeGenerator& code) const override
        {
            static constexpr char right_shift[] = {
                "template<size_t S> struct RightShift {\n"
                "   static inline size_t hash(uint64_t u) {\n"
                "       return (u >> S);\n"
                "   }\n"
                "};\n"
            };

            code.add_hash_template(right_shift);
        }

        void write_instance(
            CodeGenerator&  code,
            const char*     name,
            const Input&    input) const override
        {
            std::ostringstream os;

            os << "RightShift<" << _lsb << ">::hash";
            const auto max_table_size = 1ULL << bits_required_per_table();
            code.add_instance(name, os.str(), transform(input, max_table_size));
        }

        void reset()
        {
            _msb = 0;
            _lsb = 64;
        }

        size_t hash(uint64_t k) const override final
        {
            return k >> _lsb;
        }


    private:
        const size_t _max_table_bits = 0;

        int _msb = 0;
        int _lsb = 0;
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
    template<size_t S1, uint64_t M, size_t S2>
    struct ShiftMultiply
    {
        static uint64_t hash(uint64_t key)
        {
            key ^= key >> S1;
            key *= M;
            key ^= key >> S2;
            return key;
        }

        static std::string mixin()
        {
            std::ostringstream os;
            os << "inline uint64_t " << name() << "(uint64_t key) {\n";
            os << "    key ^= key >> " << S1 << ";\n";
            os << "    key *= " << M << "ULL;\n";
            os << "    key ^= key >> " << S2 << ";\n";
            os << "    return key;\n";
            os << "}\n";
            return os.str();
        }

        static std::string name()
        {
            std::ostringstream os;
            os << "shift_mul_" << S1 << "_" << std::hex << M << std::dec << "_" << S2;
            return os.str();
        }

        static void add(CodeGenerator& code)
        {
            code.add_hash_template(mixin());
        }
    };


    template<size_t S, uint64_t M>
    struct MultiplyAndShift
    {
        static uint64_t hash(uint64_t key)
        {
            key *= M;
            key = key >> S;
            return key;
        }

        static std::string mixin()
        {
            std::ostringstream os;
            os << "inline uint64_t " << name() << "(uint64_t key) {\n";
            os << "    key *= " << M << "ULL;\n";
            os << "    key >>= " << S << ";\n";
            os << "    return key;\n";
            os << "}\n";
            return os.str();
        }

        static std::string name()
        {
            std::ostringstream os;
            os << "mul_shift_" << S << "_" << std::hex << M << std::dec;
            return os.str();
        }

        static void add(CodeGenerator& code)
        {
            code.add_hash_template(mixin());
        }
    };


    // http://zimbry.blogspot.com/2011/09/better-bit-mixing-improving-on.html
    // using Mix0Mixer = ShiftMultiply<27, 0x02271eb7c6c4cd6b, 32>;
    // using Mix1Mixer = ShiftMultiply<31, 0x81dadef4bc2dd44d, 33>;
    // using Mix2Mixer = ShiftMultiply<33, 0xe36aa5c613612997, 31>;
    // using Mix3Mixer = ShiftMultiply<31, 0x14020a57acced8b7, 33>;
    // using Mix4Mixer = ShiftMultiply<33, 0xcb24d0a5c88c35b3, 32>;
    // using Mix5Mixer = ShiftMultiply<27, 0xfda871baea35a293, 33>;
    using Mix6Mixer = ShiftMultiply<31, 0xfc0846a64a34fff6, 33>;
    using Mix7Mixer = ShiftMultiply<27, 0xfc0962854a77f576, 33>;
    // using Mix8Mixer = ShiftMultiply<31, 0xfc087e8e4bb2f736, 33>;
    using Mix9Mixer = ShiftMultiply<27, 0xfffffcfcfd79edff, 33>;

    using MixAMixer = MultiplyAndShift<31, 0x81dadef4bc2dd44d>;
    using MixBMixer = MultiplyAndShift<33, 0xe36aa5c613612997>;
    using MixCMixer = MultiplyAndShift<27, 0xfc0962854a77f576>;


    template<typename BaseMixin, size_t N> struct RightShiftMixin
    {
        static uint64_t hash(uint64_t key)
        {
            return BaseMixin::hash(key) >> N;
        }

        static std::string mixin()
        {
            std::ostringstream os;

            os << "static inline uint64_t " << name() << "(uint64_t key) {\n";
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
    template<typename Mixin> class MixinHashBuilder : public Hash64Builder
    {
        const size_t _max_table_size;
        std::ostringstream _name;

    public:
        explicit MixinHashBuilder(size_t max_table_size) : _max_table_size(max_table_size)
        {
            _name << Mixin::name() << "_mixin_" << _max_table_size;
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


    // using HashMix0Builder = MixinHashBuilder<Mix0Mixer>;
    // using HashMix1Builder = MixinHashBuilder<Mix1Mixer>;
    // using HashMix2Builder = MixinHashBuilder<Mix2Mixer>;
    // using HashMix3Builder = MixinHashBuilder<Mix3Mixer>;
    // using HashMix4Builder = MixinHashBuilder<Mix4Mixer>;
    // using HashMix5Builder = MixinHashBuilder<Mix5Mixer>;
    using HashMix6Builder = MixinHashBuilder<Mix6Mixer>;
    using HashMix7Builder = MixinHashBuilder<Mix7Mixer>;
    //using HashMix8Builder = MixinHashBuilder<Mix8Mixer>;
    using HashMix9Builder = MixinHashBuilder<Mix9Mixer>;

    using HashMixABuilder = MixinHashBuilder<MixAMixer>;
    using HashMixBBuilder = MixinHashBuilder<MixBMixer>;
    using HashMixCBuilder = MixinHashBuilder<MixCMixer>;


    template<typename Mixin, size_t N>
    using HashMaskBuilder = MixinHashBuilder<RightShiftMixin<Mixin, N>>;


    class CompositeHashBuilder : public HashBuilder
    {
        std::vector<std::unique_ptr<HashBuilder>> _builders;
        std::string _name = "composite"; /* name of last strategy used */

    public:
        explicit CompositeHashBuilder()
        {
            /* Try a bunch of strategies and see which one fits the data best. */
            /* The idea is to maximize performance with minimum memory. */
            _builders.emplace_back(std::move(std::make_unique<RShiftHashBuilder>(16)));

            for (size_t table_size : { 512, 1024, 2048, 4096, 8192 })
            {
                _builders.emplace_back(std::move(std::make_unique<HashMaskBuilder<MixAMixer,  3>>(table_size)));
                _builders.emplace_back(std::move(std::make_unique<HashMaskBuilder<MixAMixer,  5>>(table_size)));
                _builders.emplace_back(std::move(std::make_unique<HashMaskBuilder<MixAMixer,  7>>(table_size)));
                _builders.emplace_back(std::move(std::make_unique<HashMaskBuilder<MixAMixer,  9>>(table_size)));
                _builders.emplace_back(std::move(std::make_unique<HashMaskBuilder<MixAMixer, 11>>(table_size)));
                _builders.emplace_back(std::move(std::make_unique<HashMaskBuilder<MixAMixer, 15>>(table_size)));
                _builders.emplace_back(std::move(std::make_unique<HashMaskBuilder<MixAMixer, 17>>(table_size)));
                _builders.emplace_back(std::move(std::make_unique<HashMaskBuilder<MixAMixer, 19>>(table_size)));
                _builders.emplace_back(std::move(std::make_unique<HashMaskBuilder<MixAMixer, 21>>(table_size)));

                _builders.emplace_back(std::move(std::make_unique<HashMaskBuilder<MixBMixer,  3>>(table_size)));
                _builders.emplace_back(std::move(std::make_unique<HashMaskBuilder<MixBMixer,  5>>(table_size)));
                _builders.emplace_back(std::move(std::make_unique<HashMaskBuilder<MixBMixer,  7>>(table_size)));
                _builders.emplace_back(std::move(std::make_unique<HashMaskBuilder<MixBMixer,  9>>(table_size)));
                _builders.emplace_back(std::move(std::make_unique<HashMaskBuilder<MixBMixer, 11>>(table_size)));
                _builders.emplace_back(std::move(std::make_unique<HashMaskBuilder<MixBMixer, 15>>(table_size)));
                _builders.emplace_back(std::move(std::make_unique<HashMaskBuilder<MixBMixer, 17>>(table_size)));

                // _builders.emplace_back(std::move(std::make_unique<HashMaskBuilder<MixBMixer, 19>>(table_size)));
                // _builders.emplace_back(std::move(std::make_unique<HashMaskBuilder<MixBMixer, 21>>(table_size)));

                _builders.emplace_back(std::move(std::make_unique<HashMixABuilder>(table_size)));
                _builders.emplace_back(std::move(std::make_unique<HashMixBBuilder>(table_size)));
                _builders.emplace_back(std::move(std::make_unique<HashMix6Builder>(table_size)));
                _builders.emplace_back(std::move(std::make_unique<HashMix7Builder>(table_size)));
                _builders.emplace_back(std::move(std::make_unique<HashMix9Builder>(table_size)));
            }

            _builders.emplace_back(std::move(std::make_unique<PerfectHashBuilder>()));
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
