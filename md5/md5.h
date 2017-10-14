#ifndef VINALX_MD5_
#define VINALX_MD5_

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <string>

class MD5 {
  private:
    static constexpr size_t kShift[64] = {
        7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
        5, 9,  14, 20, 5, 9,  14, 20, 5, 9,  14, 20, 5, 9,  14, 20,
        4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
        6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21};

    static constexpr uint32_t kK[64] = {
        0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf, 0x4787c62a,
        0xa8304613, 0xfd469501, 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
        0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821, 0xf61e2562, 0xc040b340,
        0x265e5a51, 0xe9b6c7aa, 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
        0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed, 0xa9e3e905, 0xfcefa3f8,
        0x676f02d9, 0x8d2a4c8a, 0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
        0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70, 0x289b7ec6, 0xeaa127fa,
        0xd4ef3085, 0x04881d05, 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
        0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039, 0x655b59c3, 0x8f0ccc92,
        0xffeff47d, 0x85845dd1, 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
        0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391,
    };

    static constexpr uint32_t kInit[4] = {0x67452301, 0xefcdab89, 0x98badcfe,
                                          0x10325476};

    using RoundF = uint32_t (*)(uint32_t, uint32_t, uint32_t);

    static uint32_t F(uint32_t b, uint32_t c, uint32_t d) {
        return (b & c) | (~b & d);
    }
    static uint32_t G(uint32_t b, uint32_t c, uint32_t d) {
        return (b & d) | (c & ~d);
    }
    static uint32_t H(uint32_t b, uint32_t c, uint32_t d) {
        return b ^ c ^ d;
    }
    static uint32_t I(uint32_t b, uint32_t c, uint32_t d) {
        return c ^ (b | ~d);
    }

    static constexpr RoundF kF[] = {&F, &G, &H, &I};

    using IndexF = size_t (*)(size_t);

    static size_t IdxF0(size_t i) {
        return i;
    }
    static size_t IdxF1(size_t i) {
        return (5 * i + 1) % 16;
    }
    static size_t IdxF2(size_t i) {
        return (3 * i + 5) % 16;
    }
    static size_t IdxF3(size_t i) {
        return 7 * i % 16;
    }

    static constexpr IndexF kIdxF[] = {&IdxF0, &IdxF1, &IdxF2, &IdxF3};

    using Chunk = uint32_t[16];

    static std::pair<std::unique_ptr<Chunk[]>, size_t>
    Padding(const uint8_t *text, size_t b) {
        size_t bits = b * 8;
        // ((bits + 1 + 64 - 1) / 512 + 1) * 512 / 8
        size_t pl = ((bits + 64) / 512 + 1) * 64;

        std::unique_ptr<uint8_t[]> data(new uint8_t[pl]);
        uint8_t *text_end = data.get() + b, *size_start = data.get() + pl - 8;

        std::copy(text, text + b, data.get());
        *text_end = 0x80;
        std::fill(text_end + 1, size_start, 0);
        *reinterpret_cast<uint64_t *>(size_start) = bits;

        return {
            std::unique_ptr<Chunk[]>(reinterpret_cast<Chunk *>(data.release())),
            pl / 64};
    }

    static uint32_t RotateL(uint32_t x, size_t n) {
        return x << n | x >> (32 - n);
    }

    static void AbsordChunk(
        Chunk &chunk, uint32_t &a, uint32_t &b, uint32_t &c, uint32_t &d) {
        uint32_t aa = a, bb = b, cc = c, dd = d;
        for (size_t i = 0; i < 64; ++i) {
            uint32_t f = RotateL(
                aa + kF[i / 16](bb, cc, dd) + chunk[kIdxF[i / 16](i)] + kK[i],
                kShift[i]);
            aa = dd;
            dd = cc;
            cc = bb;
            bb = f + bb;
        }
        a += aa;
        b += bb;
        c += cc;
        d += dd;
    }

    static std::unique_ptr<uint8_t[]>
    Pack(uint32_t a, uint32_t b, uint32_t c, uint32_t d) {
        std::unique_ptr<uint32_t[]> res(new uint32_t[4]);
        res[0] = a;
        res[1] = b;
        res[2] = c;
        res[3] = d;
        return std::unique_ptr<uint8_t[]>(
            reinterpret_cast<uint8_t *>(res.release()));
    }

  public:
    static std::string Pretty(uint8_t *hash) {
        std::string result;
        const char table[] = "0123456789abcdef";
        for (int i = 0; i < 16; ++i) {
            result.push_back(table[hash[i] / 16]);
            result.push_back(table[hash[i] % 16]);
        }
        return result;
    }

    static std::unique_ptr<uint8_t[]> Hash(const std::string &s) {
        return Hash(reinterpret_cast<const uint8_t *>(s.data()), s.size());
    }
    static std::unique_ptr<uint8_t[]> Hash(const uint8_t *text, size_t bytes) {
        auto chunks = Padding(text, bytes);

        uint32_t a = kInit[0], b = kInit[1], c = kInit[2], d = kInit[3];

        for (int i = 0; i < chunks.second; ++i) {
            AbsordChunk(chunks.first[i], a, b, c, d);
        }
        return Pack(a, b, c, d);
    }
};

constexpr MD5::RoundF MD5::kF[];
constexpr MD5::IndexF MD5::kIdxF[];
constexpr uint32_t MD5::kK[];
constexpr size_t MD5::kShift[];
constexpr uint32_t MD5::kInit[];

#endif