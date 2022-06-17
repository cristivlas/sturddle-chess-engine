#include "common.h"
#include <chrono>
#include <unordered_map>

#include "chess.h"

using namespace chess;

namespace chess
{
    AttackMasks BB_FILE_MASKS, BB_RANK_MASKS, BB_DIAG_MASKS;
}

template<typename T>
static void perf_test(const T& attack_tables, int iterations, const std::vector<int>& deltas)
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
                ASSERT_ALWAYS(elem.second == attack_tables.get(square, elem.first));
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


static void init_attack_masks(AttackMasks& mask_table, const std::vector<int>& deltas)
{
    for (int square = 0; square < 64; ++square)
    {
        auto mask = sliding_attacks(square, 0, deltas) & ~edges(square);
        mask_table[square] = mask;
    }
}

int main()
{
    static const std::vector<int> diag_attacks = {-9, -7, 7, 9};
    static const std::vector<int> file_attacks = {-8, 8};
    static const std::vector<int> rank_attacks = {-1, 1};

    init_attack_masks(BB_DIAG_MASKS, diag_attacks);
    init_attack_masks(BB_FILE_MASKS, file_attacks);
    init_attack_masks(BB_RANK_MASKS, rank_attacks);

    const int iterations = 100000;

    std::cout << "BB_DIAG_ATTACKS: ";
    perf_test(BB_DIAG_ATTACKS, iterations, diag_attacks);

    std::cout << "BB_FILE_ATTACKS: ";
    perf_test(BB_FILE_ATTACKS, iterations, file_attacks);

    std::cout << "BB_RANK_ATTACKS: ";
    perf_test(BB_RANK_ATTACKS, iterations, rank_attacks);
}
