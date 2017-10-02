#include "./des.h"
#include <cstdint>
#include <cstdio>
#include <limits>
#include <random>
#include <type_traits>

constexpr int kTestCases = 100;

template <typename IntType>
auto RandomGenerator(
    IntType i = 0, IntType j = std::numeric_limits<IntType>::max()) {
    std::mt19937 gen((std::random_device())());
    std::uniform_int_distribution<IntType> dist(i, j);
    return [ gen = std::move(gen), dist = std::move(dist) ]() mutable {
        return dist(gen);
    };
}

void TestPermutation() {
    std::printf("permutation test: ");
    auto gen = RandomGenerator<uint64_t>();
    for (int i = 0; i < kTestCases; ++i) {
        auto num = gen();
        auto perm = DES::InitialPermutation(num);
        auto should_be_num = DES::FinalPermutation(perm);
        if (num != should_be_num) {
            std::printf(
                "Fail on case %d, where\n n = %llu, IP(n) = %llu, FP(IP(n)) = "
                "%llu\n",
                i, num, perm, should_be_num);
        }
    }
    std::puts("pass");
}

void TestDES() {
    std::printf("des test: ");
    auto gen = RandomGenerator<uint64_t>();
    for (int i = 0; i < kTestCases; ++i) {
        auto text = gen(), key = gen();
        auto crypted = DES::Encrypt(text, key);
        auto should_be_text = DES::Decrypt(crypted, key);
        if (text != should_be_text) {
            std::printf(
                "Fail on case %d, where\n T = %llu, K = %llu, Encrypt(T, K) = "
                "%llu, Decrypt(C, K) = %llu",
                i, text, key, crypted, should_be_text);
            return;
        } else {
            std::printf(
                "text: %llu, crypted: %llu, decryted: %llu\n", text, crypted,
                should_be_text);
        }
    }
    std::puts("pass");
}

int main() {
    TestPermutation();
    TestDES();
}