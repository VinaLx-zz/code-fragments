#ifndef VINALX_MIPS_HELPER_
#define VINALX_MIPS_HELPER_

#include <memory>
#include <stdexcept>
#include <string>
#include <type_traits>
#include "util.h"

namespace vinalx {
namespace mips {

namespace instruction {

template <int Offset, int Length>
class Field : util::Require<util::LessEq(Offset + Length, 32)> {
  public:
    constexpr Field(int value) : value_(ComputeField(value)) {}

    constexpr bool operator==(Field f) const {
        return value_ == f.value_;
    }

    constexpr bool operator!=(Field f) const {
        return not(*this == f);
    }

  private:
    static constexpr int32_t ComputeField(int value) {
        return (value & (0xFFFFFFFFu >> (32 - Length))) << Offset;
    }
    int32_t value_;
};

using RS = Field<21, 5>;
using RT = Field<16, 5>;
using RD = Field<10, 6>;
using SA = Field<6, 5>;
using Immediate = Field<0, 16>;
using Address = Field<0, 26>;
using Opcode = Field<26, 6>;

template <int Code>
class Operation {
  public:
    static constexpr Opcode field = Field<26, 6>(Code);
};

using ADD = Operation<0b000000>;
using SUB = Operation<0b000001>;
using ADDI = Operation<0b000010>;
using OR = Operation<0b010000>;
using AND = Operation<0b010001>;
using ORI = Operation<0b010010>;
using SLL = Operation<0b011000>;
using SLT = Operation<0b100110>;
using SLTI = Operation<0b100111>;
using BEQ = Operation<0b110100>;
using SW = Operation<0b110000>;
using LW = Operation<0b110001>;
using J = Operation<0b111000>;
using JR = Operation<0b111001>;
using JAL = Operation<0b111010>;

template <typename Op, typename = void>
class Instruction;

template <typename Op>
class Instruction<
    Op, std::enable_if_t<util::In<Op, ADD, SUB, OR, AND, SLL, SLT, JR>::value,
                         void>> {};

template <typename Op>
class Instruction<
    Op,
    std::enable_if_t<util::In<Op, ADDI, ORI, SLTI, SW, LW, BEQ>::value, void>> {
};

template <typename Op>
class Instruction<Op, std::enable_if_t<util::In<Op, J, JAL>::value, void>> {};

}  // namespace instruction

}  // namespace mips
}  // namespace vinalx

#endif  // VINALX_MIPS_HELPER_
