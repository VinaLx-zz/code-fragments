#include <algorithm>
#include <cstdint>
#include <utility>
#include <bitset>
#include <vector>

class DES {
  public:
    static uint64_t Encrypt(uint64_t text, uint64_t key) {
        text = InitialPermutation(text);
        auto subkeys = GenerateSubkeys(key);
        text = Iterate(text, subkeys);
        return FinalPermutation(text);
    }
    static uint64_t Decrypt(uint64_t text, uint64_t key) {
        text = InitialPermutation(text);
        auto subkeys = GenerateSubkeys(key);
        std::reverse(begin(subkeys), end(subkeys));
        text = Iterate(text, subkeys);
        return FinalPermutation(text);
    }
    template <size_t OutLength, size_t InLength = OutLength, typename I>
    static uint64_t Permutation(I i, const size_t (&table)[OutLength]) {
        std::bitset<InLength> reference(i);
        std::bitset<OutLength> result;
        for (size_t i = 0; i < OutLength; ++i) {
            result.set(i, reference.test(table[i] - 1));
        }
        return result.to_ullong();
    }

    static constexpr size_t kInitialPermTable[64] = {
        58, 50, 42, 34, 26, 18, 10, 2, 60, 52, 44, 36, 28, 20, 12, 4,
        62, 54, 46, 38, 30, 22, 14, 6, 64, 56, 48, 40, 32, 24, 16, 8,
        57, 49, 41, 33, 25, 17, 9,  1, 59, 51, 43, 35, 27, 19, 11, 3,
        61, 53, 45, 37, 29, 21, 13, 5, 63, 55, 47, 39, 31, 23, 15, 7};

    static uint64_t InitialPermutation(uint64_t i) {
        return Permutation(i, kInitialPermTable);
    }

    static constexpr size_t kFinalPermTable[64] = {
        40, 8, 48, 16, 56, 24, 64, 32, 39, 7, 47, 15, 55, 23, 63, 31,
        38, 6, 46, 14, 54, 22, 62, 30, 37, 5, 45, 13, 53, 21, 61, 29,
        36, 4, 44, 12, 52, 20, 60, 28, 35, 3, 43, 11, 51, 19, 59, 27,
        34, 2, 42, 10, 50, 18, 58, 26, 33, 1, 41, 9,  49, 17, 57, 25};

    static uint64_t FinalPermutation(uint64_t i) {
        return Permutation(i, kFinalPermTable);
    }

    static uint64_t Iterate(uint64_t i, const std::vector<uint64_t> &subkeys) {
        uint32_t left = i >> 32, right = i & 0xFFFF'FFFF;
        for (int c = 0; c < 16; ++c) {
            uint32_t new_left = right,
                     new_right = left ^ Feistel(right, subkeys[c]);
            left = new_left, right = new_right;
        }
        return uint64_t(right) << 32 | left;
    }

    static uint32_t Feistel(uint32_t i, uint64_t k) {
        uint64_t t = ExpansionPermutation(i) ^ k;
        uint32_t substituted = Substitution(t);
        return FeistelPermutation(substituted);
    }

    static constexpr size_t kExpandPermTable[48] = {
        32, 1,  2,  3,  4,  5,  4,  5,  6,  7,  8,  9,  8,  9,  10, 11,
        12, 13, 12, 13, 14, 15, 16, 17, 16, 17, 18, 19, 20, 21, 20, 21,
        22, 23, 24, 25, 24, 25, 26, 27, 28, 29, 28, 29, 30, 31, 32, 1};

    // expand to 48 bits
    static uint64_t ExpansionPermutation(uint32_t i) {
        return Permutation(i, kExpandPermTable);
    }

    static constexpr size_t kFeistelPermTable[32] = {
        16, 7, 20, 21, 29, 12, 28, 17, 1,  15, 23, 26, 5,  18, 31, 10,
        2,  8, 24, 14, 32, 27, 3,  9,  19, 13, 30, 6,  22, 11, 4,  25};

    static uint32_t FeistelPermutation(uint32_t i) {
        return Permutation(i, kFeistelPermTable);
    }

    static constexpr uint8_t kSBoxes[8][4][16] = {
        // 1
        {
            {14, 4, 13, 1, 2, 15, 11, 8, 3, 10, 6, 12, 5, 9, 0, 7},
            {0, 15, 7, 4, 14, 2, 13, 1, 10, 6, 12, 11, 9, 5, 3, 8},
            {4, 1, 14, 8, 13, 6, 2, 11, 15, 12, 9, 7, 3, 10, 5, 0},
            {15, 12, 8, 2, 4, 9, 1, 7, 5, 11, 3, 14, 10, 0, 6, 13},
        },
        // 2
        {
            {15, 1, 8, 14, 6, 11, 3, 4, 9, 7, 2, 13, 12, 0, 5, 10},
            {3, 13, 4, 7, 15, 2, 8, 14, 12, 0, 1, 10, 6, 9, 11, 5},
            {0, 14, 7, 11, 10, 4, 13, 1, 5, 8, 12, 6, 9, 3, 2, 15},
            {13, 8, 10, 1, 3, 15, 4, 2, 11, 6, 7, 12, 0, 5, 14, 9},
        },
        // 3
        {
            {10, 0, 9, 14, 6, 3, 15, 5, 1, 13, 12, 7, 11, 4, 2, 8},
            {13, 7, 0, 9, 3, 4, 6, 10, 2, 8, 5, 14, 12, 11, 15, 1},
            {13, 6, 4, 9, 8, 15, 3, 0, 11, 1, 2, 12, 5, 10, 14, 7},
            {1, 10, 13, 0, 6, 9, 8, 7, 4, 15, 14, 3, 11, 5, 2, 12},
        },
        // 4
        {
            {7, 13, 14, 3, 0, 6, 9, 10, 1, 2, 8, 5, 11, 12, 4, 15},
            {13, 8, 11, 5, 6, 15, 0, 3, 4, 7, 2, 12, 1, 10, 14, 9},
            {10, 6, 9, 0, 12, 11, 7, 13, 15, 1, 3, 14, 5, 2, 8, 4},
            {3, 15, 0, 6, 10, 1, 13, 8, 9, 4, 5, 11, 12, 7, 2, 14},
        },
        // 5
        {
            {2, 12, 4, 1, 7, 10, 11, 6, 8, 5, 3, 15, 13, 0, 14, 9},
            {14, 11, 2, 12, 4, 7, 13, 1, 5, 0, 15, 10, 3, 9, 8, 6},
            {4, 2, 1, 11, 10, 13, 7, 8, 15, 9, 12, 5, 6, 3, 0, 14},
            {11, 8, 12, 7, 1, 14, 2, 13, 6, 15, 0, 9, 10, 4, 5, 3},
        },
        // 6
        {
            {12, 1, 10, 15, 9, 2, 6, 8, 0, 13, 3, 4, 14, 7, 5, 11},
            {10, 15, 4, 2, 7, 12, 9, 5, 6, 1, 13, 14, 0, 11, 3, 8},
            {9, 14, 15, 5, 2, 8, 12, 3, 7, 0, 4, 10, 1, 13, 11, 6},
            {4, 3, 2, 12, 9, 5, 15, 10, 11, 14, 1, 7, 6, 0, 8, 13},
        },
        // 7
        {
            {4, 11, 2, 14, 15, 0, 8, 13, 3, 12, 9, 7, 5, 10, 6, 1},
            {13, 0, 11, 7, 4, 9, 1, 10, 14, 3, 5, 12, 2, 15, 8, 6},
            {1, 4, 11, 13, 12, 3, 7, 14, 10, 15, 6, 8, 0, 5, 9, 2},
            {6, 11, 13, 8, 1, 4, 10, 7, 9, 5, 0, 15, 14, 2, 3, 12},
        },
        // 8
        {
            {13, 2, 8, 4, 6, 15, 11, 1, 10, 9, 3, 14, 5, 0, 12, 7},
            {1, 15, 13, 8, 10, 3, 7, 4, 12, 5, 6, 11, 0, 14, 9, 2},
            {7, 11, 4, 1, 9, 12, 14, 2, 0, 6, 10, 13, 15, 3, 5, 8},
            {2, 1, 14, 7, 4, 10, 8, 13, 15, 12, 9, 0, 3, 5, 6, 11},
        }};

    // input is only 48 bits
    static uint32_t Substitution(uint64_t i) {
        uint32_t result = 0;
        for (int c = 0; c < 8; ++c) {
            uint32_t s = SubstituteOne(i & 0x3F, kSBoxes[c]);
            result |= s << c * 4;
            i >>= 6;
        }
        return result;
    }

    static uint8_t SubstituteOne(uint8_t i, const uint8_t (&table)[4][16]) {
        size_t x = (i & 0x1) | (i & 0x20) >> 4;
        size_t y = (i >> 1) & 0xF;
        return table[x][y];
    }

    static constexpr size_t kKeyShifts[16] = {1, 1, 2, 2, 2, 2, 2, 2,
                                              1, 2, 2, 2, 2, 2, 2, 1};

    static constexpr size_t kKeyPermTable1[56] = {
        57, 49, 41, 33, 25, 17, 9,  1,  58, 50, 42, 34, 26, 18,
        10, 2,  59, 51, 43, 35, 27, 19, 11, 3,  60, 52, 44, 36,
        63, 55, 47, 39, 31, 23, 15, 7,  62, 54, 46, 38, 30, 22,
        14, 6,  61, 53, 45, 37, 29, 21, 13, 5,  28, 20, 12, 4};

    static constexpr size_t kKeyPermTable2[48] = {
        14, 17, 11, 24, 1,  5,  3,  28, 15, 6,  21, 10, 23, 19, 12, 4,
        26, 8,  16, 7,  27, 20, 13, 2,  41, 52, 31, 37, 47, 55, 30, 40,
        51, 45, 33, 48, 44, 49, 39, 56, 34, 53, 46, 42, 50, 36, 29, 32};

    // only use 56 bits of key
    static std::vector<uint64_t>
    GenerateSubkeys(uint64_t key) {
        std::vector<uint64_t> subkeys;
        uint64_t permed =
            Permutation<56, 64>(key, kKeyPermTable1);
        uint32_t c = permed >> 28, d = permed & 0xFFF'FFFF;
        for (int i = 0; i < 16; ++i) {
            auto shift = kKeyShifts[i];
            c = c >> (28 - shift) | c << shift;
            d = d >> (28 - shift) | d << shift;
            subkeys.push_back(
                Permutation<48, 56>(uint64_t(c) << 28 | d, kKeyPermTable2));
        }
        return subkeys;
    }
};