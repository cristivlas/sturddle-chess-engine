#include "chess.h"
namespace test
{
#include "test.h"
}
#include <bitset>
#include <chrono>
#include <ostream>
#include <set>
#include <unordered_map>

using namespace chess;


using AttackMasks = std::array<chess::Bitboard, 64>;
static AttackMasks BB_DIAG_MASKS, BB_FILE_MASKS, BB_RANK_MASKS;


static void
test_attack_tables(
    const std::vector<int>& deltas,
    const chess::AttackTable (&tables)[64])
{
    for (int square = 0; square < 64; ++square)
    {
        auto mask = sliding_attacks(square, 0, deltas) & ~edges(square);

        for_each_subset(mask, [&](Bitboard key)
        {
            auto value = sliding_attacks(square, key, deltas);

            if (tables[square][key] != value)
            {
                std::cerr << "Fail: [" << key << "] " << tables[square][key] << " != " << value << "\n";
                exit(1);
            }
        });
    }
}


static void
perf_test(
    const chess::AttackTable (&tables)[64],
    int iterations,
    const std::vector<int>& deltas)
{
    uint64_t count = 0;
    double total_time = 0;

    for (int square = 0; square < 64; ++square)
    {
        auto mask = sliding_attacks(square, 0, deltas) & ~edges(square);

        std::unordered_map<uint64_t, uint64_t> attacks;

        for_each_subset(mask, [&](Bitboard key)
        {
            auto value = sliding_attacks(square, key, deltas);
            attacks[key] = value;
        });

        auto start = std::chrono::system_clock::now();

        for (int i = 0; i != iterations; ++i)
        {
            for (const auto& elem : attacks)
            {
                ASSERT_ALWAYS(elem.second == tables[square][elem.first]);
                ++count;
            }
        }
        auto end = std::chrono::system_clock::now();
        auto millis = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
        total_time += millis.count();
    }
    total_time /= 1000;
    std::cout << count / total_time << " ops/sec\n";
}


static void
init_attack_masks(
    AttackMasks& mask_table,
    const chess::AttackTable (&tables)[64],
    const std::vector<int>& deltas)
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


int main()
{
    static const std::vector<int> diag_attacks = {-9, -7, 7, 9};
    static const std::vector<int> file_attacks = {-8, 8};
    static const std::vector<int> rank_attacks = {-1, 1};

    chess::_init();

    init_attack_masks(chess::BB_DIAG_MASKS, test::chess::BB_DIAG_ATTACKS, diag_attacks);
    init_attack_masks(chess::BB_FILE_MASKS, test::chess::BB_FILE_ATTACKS, file_attacks);
    init_attack_masks(chess::BB_RANK_MASKS, test::chess::BB_RANK_ATTACKS, rank_attacks);

    test_attack_tables(diag_attacks, test::chess::BB_DIAG_ATTACKS);
    test_attack_tables(file_attacks, test::chess::BB_FILE_ATTACKS);
    test_attack_tables(rank_attacks, test::chess::BB_RANK_ATTACKS);

    const int iterations = 100000;

    std::cout << "BB_DIAG_ATTACKS: ";
    perf_test(test::chess::BB_DIAG_ATTACKS, iterations, diag_attacks);

    std::cout << "BB_FILE_ATTACKS: ";
    perf_test(test::chess::BB_FILE_ATTACKS, iterations, file_attacks);

    std::cout << "BB_RANK_ATTACKS: ";
    perf_test(test::chess::BB_RANK_ATTACKS, iterations, rank_attacks);
}
