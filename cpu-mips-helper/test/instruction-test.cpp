#include "instruction.h"
#include <gtest/gtest.h>

using namespace vinalx::mips::instruction;

class InstructionTest : public testing::Test {
  protected:
    virtual void SetUp() {
        rs = RS(31);
        rt = RT(15);
        rd = RD(10);
        sa = SA(4);
        imm = Immediate(0xFEDC);
        addr = Address(0xFEDCBA0 >> 2);
    }
    RS rs;
    RT rt;
    RD rd;
    SA sa;
    Immediate imm;
    Address addr;
};

TEST_F(InstructionTest, TestFieldConversion) {
    EXPECT_EQ(rs.Value(), 0b00000011'11100000'00000000'00000000);
    EXPECT_EQ(rt.Value(), 0b00000000'00001111'00000000'00000000);
    EXPECT_EQ(rd.Value(), 0b00000000'00000000'01010000'00000000);
    EXPECT_EQ(sa.Value(), 0b00000000'00000000'00000001'00000000);
    EXPECT_EQ(imm.Value(), 0b00000000'00000000'11111110'11011100);
    EXPECT_EQ(addr.Value(), 0b00000011'11111011'01110010'11101000);
}

TEST_F(InstructionTest, TestRType) {
    Instruction<ADD> ins(rs, rt, rd, 0);
    EXPECT_EQ(
        ins.Generate(),
        rs.Value() | rd.Value() | rt.Value() | ToOpcode<ADD>().Value());
    Instruction<SLL> ins2(0, rt, rd, sa);
    EXPECT_EQ(
        ins2.Generate(),
        rt.Value() | rd.Value() | sa.Value() | ToOpcode<SLL>().Value());
}

TEST_F(InstructionTest, TestIType) {
    Instruction<ADDI> ins(rs, rt, imm);
    EXPECT_EQ(
        ins.Generate(),
        rs.Value() | rt.Value() | imm.Value() | ToOpcode<ADDI>().Value());
    Instruction<BEQ> ins2(rs, rt, imm);
    EXPECT_EQ(
        ins2.Generate(),
        rs.Value() | rt.Value() | imm.Value() | ToOpcode<BEQ>().Value());
}

TEST_F(InstructionTest, TestJType) {
    Instruction<J> ins(addr);
    EXPECT_EQ(ins.Generate(), addr.Value() | ToOpcode<J>().Value());
    Instruction<JAL> ins2(addr);
    EXPECT_EQ(ins2.Generate(), addr.Value() | ToOpcode<JAL>().Value());
}
