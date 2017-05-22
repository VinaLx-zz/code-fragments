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
    constexpr Field(int value = 0) : value_(ComputeField(value)) {}

    constexpr int32_t Value() const {
        return value_;
    }

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

constexpr int32_t CombineImpl() {
    return 0;
}

template <int Offset, int Length, typename... Fields>
constexpr int32_t CombineImpl(
    const Field<Offset, Length>& head, const Fields&... rest) {
    return head.Value() | CombineImpl(rest...);
}

template <typename... Fields>
constexpr int32_t Combine(const Fields&... fields) {
    return CombineImpl(fields...);
}

using RS = Field<21, 5>;
using RT = Field<16, 5>;
using RD = Field<11, 5>;
using SA = Field<6, 5>;
using Immediate = Field<0, 16>;
using Address = Field<0, 26>;
using Opcode = Field<26, 6>;

template <int Code>
struct  Operation {
  public:
    static constexpr int value = Code;
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

template <typename Op>
constexpr Opcode ToOpcode() {
    return Opcode(Op::value);
}

template <typename Op, typename = void>
class Instruction;

template <typename Op>
class Instruction<
    Op,
    std::enable_if_t<util::In<Op, ADD, SUB, OR, AND, SLL, SLT, JR>::value>> {
  public:
    constexpr Instruction(RS rs, RT rt, RD rd, SA sa)
        : rs_(rs), rt_(rt), rd_(rd), sa_(sa){};

    constexpr int32_t Generate() const {
        return Combine(ToOpcode<Op>(), rs_, rt_, rd_, sa_);
    }

  private:
    RS rs_;
    RT rt_;
    RD rd_;
    SA sa_;
};

template <typename Op>
class Instruction<
    Op, std::enable_if_t<util::In<Op, ADDI, ORI, SLTI, SW, LW, BEQ>::value>> {
  public:
    constexpr Instruction(RS rs, RT rt, Immediate imm)
        : rs_(rs), rt_(rt), imm_(imm) {}

    constexpr int32_t Generate() const {
        return Combine(ToOpcode<Op>(), rs_, rt_, imm_);
    }

  private:
    RS rs_;
    RT rt_;
    Immediate imm_;
};

template <typename Op>
class Instruction<Op, std::enable_if_t<util::In<Op, J, JAL>::value>> {
  public:
    Instruction(Address addr) : addr_(addr) {}

    constexpr int32_t Generate() const {
        return Combine(ToOpcode<Op>(), addr_);
    }

  private:
    Address addr_;
};

}  // namespace instruction

}  // namespace mips
}  // namespace vinalx

#endif  // VINALX_MIPS_HELPER_
