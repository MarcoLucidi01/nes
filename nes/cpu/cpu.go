// See license file for copyright and license details.

package cpu

import (
	"fmt"
	"strings"
)

const (
	stackBase = 0x0100
	brkVector = 0xfffe
	irqVector = brkVector
)

type CPU struct {
	a  uint8 // accumulator.
	x  uint8 // x register.
	y  uint8 // y register.
	sp uint8 // stack pointer.

	prevpc uint16 // previous program counter, used for logging and to detect infinite loops.
	pc     uint16 // program counter.

	// status flags.
	n bool // negative.
	v bool // overflow.
	b bool // break command.
	d bool // decimal.
	i bool // interrupt disable.
	z bool // zero.
	c bool // carry.

	cycles uint64 // number of cycles.

	mem []uint8 // memory.

	inst   instruction // current fetched instruction.
	instB2 uint8       // instruction second byte (optional).
	instB3 uint8       // instruction third byte (optional).
}

// instruction definition.
type instruction struct {
	opcode uint8

	// number of CPU cycles required by the instruction.
	ncycles int

	// how many extra CPU cycles the instruction takes when a page is
	// crossed during addressing.
	extracycle int

	// instruction's asm name, illegal instructions start with a *, normal
	// instructions start with a space (to nicely line up the table). it's
	// used for producing the nestest.log
	name string

	// CPU function that implements the instruction.
	exec func(cpu *CPU)

	// instruction's addressing mode.
	mode addrMode
}

// addressing mode.
type addrMode struct {
	// number of bytes to fetch when an instruction uses this addressing mode.
	nbytes int

	// CPU function that does the addressing and returns the "real" address,
	// it also returns a bool which tells if a page has been crossed (or
	// not) during addressing.
	addr func(cpu *CPU) (uint16, bool)

	// logAddr returns a string containing the addressing information using
	// nestest.log format.
	logAddr func(cpu *CPU) string

	// custom read() and write() functions for addressing modes which don't
	// return a valid memory address and needs to read somewhere else (e.g.
	// accumulator addressing mode reads and writes from cpu.a).
	// it's kind of a hack.
	read  func(cpu *CPU) uint8
	write func(cpu *CPU, b uint8)
}

// instructions table.
var table = []instruction{
	{0x00, 7, 0, " BRK", (*CPU).brk, implied},
	{0x01, 6, 0, " ORA", (*CPU).ora, zeroPageXIndexedIndirect},
	{0x02, 1, 0, " UNK", (*CPU).unk, absolute},
	{0x03, 8, 0, "*SLO", (*CPU).slo, zeroPageXIndexedIndirect},
	{0x04, 3, 0, "*NOP", (*CPU).nop, zeroPage},
	{0x05, 3, 0, " ORA", (*CPU).ora, zeroPage},
	{0x06, 5, 0, " ASL", (*CPU).asl, zeroPage},
	{0x07, 5, 0, "*SLO", (*CPU).slo, zeroPage},
	{0x08, 3, 0, " PHP", (*CPU).php, implied},
	{0x09, 2, 0, " ORA", (*CPU).ora, immediate},
	{0x0a, 2, 0, " ASL", (*CPU).asl, accumulator},
	{0x0b, 1, 0, " UNK", (*CPU).unk, absolute},
	{0x0c, 4, 0, "*NOP", (*CPU).nop, absolute},
	{0x0d, 4, 0, " ORA", (*CPU).ora, absolute},
	{0x0e, 6, 0, " ASL", (*CPU).asl, absolute},
	{0x0f, 6, 0, "*SLO", (*CPU).slo, absolute},
	{0x10, 2, 1, " BPL", (*CPU).bpl, relative},
	{0x11, 5, 1, " ORA", (*CPU).ora, zeroPageIndirectYIndexed},
	{0x12, 1, 0, " UNK", (*CPU).unk, absolute},
	{0x13, 8, 0, "*SLO", (*CPU).slo, zeroPageIndirectYIndexed},
	{0x14, 4, 0, "*NOP", (*CPU).nop, zeroPageXIndexed},
	{0x15, 4, 0, " ORA", (*CPU).ora, zeroPageXIndexed},
	{0x16, 6, 0, " ASL", (*CPU).asl, zeroPageXIndexed},
	{0x17, 6, 0, "*SLO", (*CPU).slo, zeroPageXIndexed},
	{0x18, 2, 0, " CLC", (*CPU).clc, implied},
	{0x19, 4, 1, " ORA", (*CPU).ora, absoluteYIndexed},
	{0x1a, 2, 0, "*NOP", (*CPU).nop, implied},
	{0x1b, 7, 0, "*SLO", (*CPU).slo, absoluteYIndexed},
	{0x1c, 4, 1, "*NOP", (*CPU).nop, absoluteXIndexed},
	{0x1d, 4, 1, " ORA", (*CPU).ora, absoluteXIndexed},
	{0x1e, 7, 0, " ASL", (*CPU).asl, absoluteXIndexed},
	{0x1f, 7, 0, "*SLO", (*CPU).slo, absoluteXIndexed},
	{0x20, 6, 0, " JSR", (*CPU).jsr, absolute},
	{0x21, 6, 0, " AND", (*CPU).and, zeroPageXIndexedIndirect},
	{0x22, 1, 0, " UNK", (*CPU).unk, absolute},
	{0x23, 8, 0, "*RLA", (*CPU).rla, zeroPageXIndexedIndirect},
	{0x24, 3, 0, " BIT", (*CPU).bit, zeroPage},
	{0x25, 3, 0, " AND", (*CPU).and, zeroPage},
	{0x26, 5, 0, " ROL", (*CPU).rol, zeroPage},
	{0x27, 5, 0, "*RLA", (*CPU).rla, zeroPage},
	{0x28, 4, 0, " PLP", (*CPU).plp, implied},
	{0x29, 2, 0, " AND", (*CPU).and, immediate},
	{0x2a, 2, 0, " ROL", (*CPU).rol, accumulator},
	{0x2b, 1, 0, " UNK", (*CPU).unk, absolute},
	{0x2c, 4, 0, " BIT", (*CPU).bit, absolute},
	{0x2d, 4, 0, " AND", (*CPU).and, absolute},
	{0x2e, 6, 0, " ROL", (*CPU).rol, absolute},
	{0x2f, 6, 0, "*RLA", (*CPU).rla, absolute},
	{0x30, 2, 1, " BMI", (*CPU).bmi, relative},
	{0x31, 5, 1, " AND", (*CPU).and, zeroPageIndirectYIndexed},
	{0x32, 1, 0, " UNK", (*CPU).unk, absolute},
	{0x33, 8, 0, "*RLA", (*CPU).rla, zeroPageIndirectYIndexed},
	{0x34, 4, 0, "*NOP", (*CPU).nop, zeroPageXIndexed},
	{0x35, 4, 0, " AND", (*CPU).and, zeroPageXIndexed},
	{0x36, 6, 0, " ROL", (*CPU).rol, zeroPageXIndexed},
	{0x37, 6, 0, "*RLA", (*CPU).rla, zeroPageXIndexed},
	{0x38, 2, 0, " SEC", (*CPU).sec, implied},
	{0x39, 4, 1, " AND", (*CPU).and, absoluteYIndexed},
	{0x3a, 2, 0, "*NOP", (*CPU).nop, implied},
	{0x3b, 7, 0, "*RLA", (*CPU).rla, absoluteYIndexed},
	{0x3c, 4, 1, "*NOP", (*CPU).nop, absoluteXIndexed},
	{0x3d, 4, 1, " AND", (*CPU).and, absoluteXIndexed},
	{0x3e, 7, 0, " ROL", (*CPU).rol, absoluteXIndexed},
	{0x3f, 7, 0, "*RLA", (*CPU).rla, absoluteXIndexed},
	{0x40, 6, 0, " RTI", (*CPU).rti, implied},
	{0x41, 6, 0, " EOR", (*CPU).eor, zeroPageXIndexedIndirect},
	{0x42, 1, 0, " UNK", (*CPU).unk, absolute},
	{0x43, 8, 0, "*SRE", (*CPU).sre, zeroPageXIndexedIndirect},
	{0x44, 3, 0, "*NOP", (*CPU).nop, zeroPage},
	{0x45, 3, 0, " EOR", (*CPU).eor, zeroPage},
	{0x46, 5, 0, " LSR", (*CPU).lsr, zeroPage},
	{0x47, 5, 0, "*SRE", (*CPU).sre, zeroPage},
	{0x48, 3, 0, " PHA", (*CPU).pha, implied},
	{0x49, 2, 0, " EOR", (*CPU).eor, immediate},
	{0x4a, 2, 0, " LSR", (*CPU).lsr, accumulator},
	{0x4b, 1, 0, " UNK", (*CPU).unk, absolute},
	{0x4c, 3, 0, " JMP", (*CPU).jmp, absolute},
	{0x4d, 4, 0, " EOR", (*CPU).eor, absolute},
	{0x4e, 6, 0, " LSR", (*CPU).lsr, absolute},
	{0x4f, 6, 0, "*SRE", (*CPU).sre, absolute},
	{0x50, 2, 1, " BVC", (*CPU).bvc, relative},
	{0x51, 5, 1, " EOR", (*CPU).eor, zeroPageIndirectYIndexed},
	{0x52, 1, 0, " UNK", (*CPU).unk, absolute},
	{0x53, 8, 0, "*SRE", (*CPU).sre, zeroPageIndirectYIndexed},
	{0x54, 4, 0, "*NOP", (*CPU).nop, zeroPageXIndexed},
	{0x55, 4, 0, " EOR", (*CPU).eor, zeroPageXIndexed},
	{0x56, 6, 0, " LSR", (*CPU).lsr, zeroPageXIndexed},
	{0x57, 6, 0, "*SRE", (*CPU).sre, zeroPageXIndexed},
	{0x58, 2, 0, " CLI", (*CPU).cli, implied},
	{0x59, 4, 1, " EOR", (*CPU).eor, absoluteYIndexed},
	{0x5a, 2, 0, "*NOP", (*CPU).nop, implied},
	{0x5b, 7, 0, "*SRE", (*CPU).sre, absoluteYIndexed},
	{0x5c, 4, 1, "*NOP", (*CPU).nop, absoluteXIndexed},
	{0x5d, 4, 1, " EOR", (*CPU).eor, absoluteXIndexed},
	{0x5e, 7, 0, " LSR", (*CPU).lsr, absoluteXIndexed},
	{0x5f, 7, 0, "*SRE", (*CPU).sre, absoluteXIndexed},
	{0x60, 6, 0, " RTS", (*CPU).rts, implied},
	{0x61, 6, 0, " ADC", (*CPU).adc, zeroPageXIndexedIndirect},
	{0x62, 1, 0, " UNK", (*CPU).unk, absolute},
	{0x63, 8, 0, "*RRA", (*CPU).rra, zeroPageXIndexedIndirect},
	{0x64, 3, 0, "*NOP", (*CPU).nop, zeroPage},
	{0x65, 3, 0, " ADC", (*CPU).adc, zeroPage},
	{0x66, 5, 0, " ROR", (*CPU).ror, zeroPage},
	{0x67, 5, 0, "*RRA", (*CPU).rra, zeroPage},
	{0x68, 4, 0, " PLA", (*CPU).pla, implied},
	{0x69, 2, 0, " ADC", (*CPU).adc, immediate},
	{0x6a, 2, 0, " ROR", (*CPU).ror, accumulator},
	{0x6b, 1, 0, " UNK", (*CPU).unk, absolute},
	{0x6c, 5, 0, " JMP", (*CPU).jmp, absoluteIndirect},
	{0x6d, 4, 0, " ADC", (*CPU).adc, absolute},
	{0x6e, 6, 0, " ROR", (*CPU).ror, absolute},
	{0x6f, 6, 0, "*RRA", (*CPU).rra, absolute},
	{0x70, 2, 1, " BVS", (*CPU).bvs, relative},
	{0x71, 5, 1, " ADC", (*CPU).adc, zeroPageIndirectYIndexed},
	{0x72, 1, 0, " UNK", (*CPU).unk, absolute},
	{0x73, 8, 0, "*RRA", (*CPU).rra, zeroPageIndirectYIndexed},
	{0x74, 4, 0, "*NOP", (*CPU).nop, zeroPageXIndexed},
	{0x75, 4, 0, " ADC", (*CPU).adc, zeroPageXIndexed},
	{0x76, 6, 0, " ROR", (*CPU).ror, zeroPageXIndexed},
	{0x77, 6, 0, "*RRA", (*CPU).rra, zeroPageXIndexed},
	{0x78, 2, 0, " SEI", (*CPU).sei, implied},
	{0x79, 4, 1, " ADC", (*CPU).adc, absoluteYIndexed},
	{0x7a, 2, 0, "*NOP", (*CPU).nop, implied},
	{0x7b, 7, 0, "*RRA", (*CPU).rra, absoluteYIndexed},
	{0x7c, 4, 1, "*NOP", (*CPU).nop, absoluteXIndexed},
	{0x7d, 4, 1, " ADC", (*CPU).adc, absoluteXIndexed},
	{0x7e, 7, 0, " ROR", (*CPU).ror, absoluteXIndexed},
	{0x7f, 7, 0, "*RRA", (*CPU).rra, absoluteXIndexed},
	{0x80, 2, 0, "*NOP", (*CPU).nop, immediate},
	{0x81, 6, 0, " STA", (*CPU).sta, zeroPageXIndexedIndirect},
	{0x82, 1, 0, " UNK", (*CPU).unk, absolute},
	{0x83, 6, 0, "*SAX", (*CPU).sax, zeroPageXIndexedIndirect},
	{0x84, 3, 0, " STY", (*CPU).sty, zeroPage},
	{0x85, 3, 0, " STA", (*CPU).sta, zeroPage},
	{0x86, 3, 0, " STX", (*CPU).stx, zeroPage},
	{0x87, 3, 0, "*SAX", (*CPU).sax, zeroPage},
	{0x88, 2, 0, " DEY", (*CPU).dey, implied},
	{0x89, 1, 0, " UNK", (*CPU).unk, absolute},
	{0x8a, 2, 0, " TXA", (*CPU).txa, implied},
	{0x8b, 1, 0, " UNK", (*CPU).unk, absolute},
	{0x8c, 4, 0, " STY", (*CPU).sty, absolute},
	{0x8d, 4, 0, " STA", (*CPU).sta, absolute},
	{0x8e, 4, 0, " STX", (*CPU).stx, absolute},
	{0x8f, 4, 0, "*SAX", (*CPU).sax, absolute},
	{0x90, 2, 1, " BCC", (*CPU).bcc, relative},
	{0x91, 6, 0, " STA", (*CPU).sta, zeroPageIndirectYIndexed},
	{0x92, 1, 0, " UNK", (*CPU).unk, absolute},
	{0x93, 1, 0, " UNK", (*CPU).unk, absolute},
	{0x94, 4, 0, " STY", (*CPU).sty, zeroPageXIndexed},
	{0x95, 4, 0, " STA", (*CPU).sta, zeroPageXIndexed},
	{0x96, 4, 0, " STX", (*CPU).stx, zeroPageYIndexed},
	{0x97, 4, 0, "*SAX", (*CPU).sax, zeroPageYIndexed},
	{0x98, 2, 0, " TYA", (*CPU).tya, implied},
	{0x99, 5, 0, " STA", (*CPU).sta, absoluteYIndexed},
	{0x9a, 2, 0, " TXS", (*CPU).txs, implied},
	{0x9b, 1, 0, " UNK", (*CPU).unk, absolute},
	{0x9c, 1, 0, " UNK", (*CPU).unk, absolute},
	{0x9d, 5, 0, " STA", (*CPU).sta, absoluteXIndexed},
	{0x9e, 1, 0, " UNK", (*CPU).unk, absolute},
	{0x9f, 1, 0, " UNK", (*CPU).unk, absolute},
	{0xa0, 2, 0, " LDY", (*CPU).ldy, immediate},
	{0xa1, 6, 0, " LDA", (*CPU).lda, zeroPageXIndexedIndirect},
	{0xa2, 2, 0, " LDX", (*CPU).ldx, immediate},
	{0xa3, 6, 0, "*LAX", (*CPU).lax, zeroPageXIndexedIndirect},
	{0xa4, 3, 0, " LDY", (*CPU).ldy, zeroPage},
	{0xa5, 3, 0, " LDA", (*CPU).lda, zeroPage},
	{0xa6, 3, 0, " LDX", (*CPU).ldx, zeroPage},
	{0xa7, 3, 0, "*LAX", (*CPU).lax, zeroPage},
	{0xa8, 2, 0, " TAY", (*CPU).tay, implied},
	{0xa9, 2, 0, " LDA", (*CPU).lda, immediate},
	{0xaa, 2, 0, " TAX", (*CPU).tax, implied},
	{0xab, 1, 0, " UNK", (*CPU).unk, absolute},
	{0xac, 4, 0, " LDY", (*CPU).ldy, absolute},
	{0xad, 4, 0, " LDA", (*CPU).lda, absolute},
	{0xae, 4, 0, " LDX", (*CPU).ldx, absolute},
	{0xaf, 4, 0, "*LAX", (*CPU).lax, absolute},
	{0xb0, 2, 1, " BCS", (*CPU).bcs, relative},
	{0xb1, 5, 1, " LDA", (*CPU).lda, zeroPageIndirectYIndexed},
	{0xb2, 1, 0, " UNK", (*CPU).unk, absolute},
	{0xb3, 5, 1, "*LAX", (*CPU).lax, zeroPageIndirectYIndexed},
	{0xb4, 4, 0, " LDY", (*CPU).ldy, zeroPageXIndexed},
	{0xb5, 4, 0, " LDA", (*CPU).lda, zeroPageXIndexed},
	{0xb6, 4, 0, " LDX", (*CPU).ldx, zeroPageYIndexed},
	{0xb7, 4, 0, "*LAX", (*CPU).lax, zeroPageYIndexed},
	{0xb8, 2, 0, " CLV", (*CPU).clv, implied},
	{0xb9, 4, 1, " LDA", (*CPU).lda, absoluteYIndexed},
	{0xba, 2, 0, " TSX", (*CPU).tsx, implied},
	{0xbb, 1, 1, " UNK", (*CPU).unk, absolute},
	{0xbc, 4, 1, " LDY", (*CPU).ldy, absoluteXIndexed},
	{0xbd, 4, 1, " LDA", (*CPU).lda, absoluteXIndexed},
	{0xbe, 4, 1, " LDX", (*CPU).ldx, absoluteYIndexed},
	{0xbf, 4, 1, "*LAX", (*CPU).lax, absoluteYIndexed},
	{0xc0, 2, 0, " CPY", (*CPU).cpy, immediate},
	{0xc1, 6, 0, " CMP", (*CPU).cmp, zeroPageXIndexedIndirect},
	{0xc2, 1, 0, " UNK", (*CPU).unk, absolute},
	{0xc3, 8, 0, "*DCP", (*CPU).dcp, zeroPageXIndexedIndirect},
	{0xc4, 3, 0, " CPY", (*CPU).cpy, zeroPage},
	{0xc5, 3, 0, " CMP", (*CPU).cmp, zeroPage},
	{0xc6, 5, 0, " DEC", (*CPU).dec, zeroPage},
	{0xc7, 5, 0, "*DCP", (*CPU).dcp, zeroPage},
	{0xc8, 2, 0, " INY", (*CPU).iny, implied},
	{0xc9, 2, 0, " CMP", (*CPU).cmp, immediate},
	{0xca, 2, 0, " DEX", (*CPU).dex, implied},
	{0xcb, 1, 0, " UNK", (*CPU).unk, absolute},
	{0xcc, 4, 0, " CPY", (*CPU).cpy, absolute},
	{0xcd, 4, 0, " CMP", (*CPU).cmp, absolute},
	{0xce, 6, 0, " DEC", (*CPU).dec, absolute},
	{0xcf, 6, 0, "*DCP", (*CPU).dcp, absolute},
	{0xd0, 2, 1, " BNE", (*CPU).bne, relative},
	{0xd1, 5, 1, " CMP", (*CPU).cmp, zeroPageIndirectYIndexed},
	{0xd2, 1, 0, " UNK", (*CPU).unk, absolute},
	{0xd3, 8, 0, "*DCP", (*CPU).dcp, zeroPageIndirectYIndexed},
	{0xd4, 4, 0, "*NOP", (*CPU).nop, zeroPageXIndexed},
	{0xd5, 4, 0, " CMP", (*CPU).cmp, zeroPageXIndexed},
	{0xd6, 6, 0, " DEC", (*CPU).dec, zeroPageXIndexed},
	{0xd7, 6, 0, "*DCP", (*CPU).dcp, zeroPageXIndexed},
	{0xd8, 2, 0, " CLD", (*CPU).cld, implied},
	{0xd9, 4, 1, " CMP", (*CPU).cmp, absoluteYIndexed},
	{0xda, 2, 0, "*NOP", (*CPU).nop, implied},
	{0xdb, 7, 0, "*DCP", (*CPU).dcp, absoluteYIndexed},
	{0xdc, 4, 1, "*NOP", (*CPU).nop, absoluteXIndexed},
	{0xdd, 4, 1, " CMP", (*CPU).cmp, absoluteXIndexed},
	{0xde, 7, 0, " DEC", (*CPU).dec, absoluteXIndexed},
	{0xdf, 7, 0, "*DCP", (*CPU).dcp, absoluteXIndexed},
	{0xe0, 2, 0, " CPX", (*CPU).cpx, immediate},
	{0xe1, 6, 0, " SBC", (*CPU).sbc, zeroPageXIndexedIndirect},
	{0xe2, 1, 0, " UNK", (*CPU).unk, absolute},
	{0xe3, 8, 0, "*ISB", (*CPU).isb, zeroPageXIndexedIndirect},
	{0xe4, 3, 0, " CPX", (*CPU).cpx, zeroPage},
	{0xe5, 3, 0, " SBC", (*CPU).sbc, zeroPage},
	{0xe6, 5, 0, " INC", (*CPU).inc, zeroPage},
	{0xe7, 5, 0, "*ISB", (*CPU).isb, zeroPage},
	{0xe8, 2, 0, " INX", (*CPU).inx, implied},
	{0xe9, 2, 0, " SBC", (*CPU).sbc, immediate},
	{0xea, 2, 0, " NOP", (*CPU).nop, implied},
	{0xeb, 2, 0, "*SBC", (*CPU).sbc, immediate},
	{0xec, 4, 0, " CPX", (*CPU).cpx, absolute},
	{0xed, 4, 0, " SBC", (*CPU).sbc, absolute},
	{0xee, 6, 0, " INC", (*CPU).inc, absolute},
	{0xef, 6, 0, "*ISB", (*CPU).isb, absolute},
	{0xf0, 2, 1, " BEQ", (*CPU).beq, relative},
	{0xf1, 5, 1, " SBC", (*CPU).sbc, zeroPageIndirectYIndexed},
	{0xf2, 1, 0, " UNK", (*CPU).unk, absolute},
	{0xf3, 8, 0, "*ISB", (*CPU).isb, zeroPageIndirectYIndexed},
	{0xf4, 4, 0, "*NOP", (*CPU).nop, zeroPageXIndexed},
	{0xf5, 4, 0, " SBC", (*CPU).sbc, zeroPageXIndexed},
	{0xf6, 6, 0, " INC", (*CPU).inc, zeroPageXIndexed},
	{0xf7, 6, 0, "*ISB", (*CPU).isb, zeroPageXIndexed},
	{0xf8, 2, 0, " SED", (*CPU).sed, implied},
	{0xf9, 4, 1, " SBC", (*CPU).sbc, absoluteYIndexed},
	{0xfa, 2, 0, "*NOP", (*CPU).nop, implied},
	{0xfb, 7, 0, "*ISB", (*CPU).isb, absoluteYIndexed},
	{0xfc, 4, 1, "*NOP", (*CPU).nop, absoluteXIndexed},
	{0xfd, 4, 1, " SBC", (*CPU).sbc, absoluteXIndexed},
	{0xfe, 7, 0, " INC", (*CPU).inc, absoluteXIndexed},
	{0xff, 7, 0, "*ISB", (*CPU).isb, absoluteXIndexed},
}

// addressing modes.
var (
	absolute                 = addrMode{3, (*CPU).addrAbsolute, (*CPU).logAbsolute, nil, nil}
	absoluteIndirect         = addrMode{3, (*CPU).addrAbsoluteIndirect, (*CPU).logAbsoluteIndirect, nil, nil}
	absoluteXIndexed         = addrMode{3, (*CPU).addrAbsoluteXIndexed, (*CPU).logAbsoluteXIndexed, nil, nil}
	absoluteYIndexed         = addrMode{3, (*CPU).addrAbsoluteYIndexed, (*CPU).logAbsoluteYIndexed, nil, nil}
	zeroPage                 = addrMode{2, (*CPU).addrZeroPage, (*CPU).logZeroPage, nil, nil}
	zeroPageIndirectYIndexed = addrMode{2, (*CPU).addrZeroPageIndirectYIndexed, (*CPU).logZeroPageIndirectYIndexed, nil, nil}
	zeroPageXIndexed         = addrMode{2, (*CPU).addrZeroPageXIndexed, (*CPU).logZeroPageXIndexed, nil, nil}
	zeroPageXIndexedIndirect = addrMode{2, (*CPU).addrZeroPageXIndexedIndirect, (*CPU).logZeroPageXIndexedIndirect, nil, nil}
	zeroPageYIndexed         = addrMode{2, (*CPU).addrZeroPageYIndexed, (*CPU).logZeroPageYIndexed, nil, nil}
	accumulator              = addrMode{1, (*CPU).addrAccumulator, (*CPU).logAccumulator,
		func(cpu *CPU) uint8 { return cpu.a },
		func(cpu *CPU, b uint8) { cpu.a = b },
	}
	immediate = addrMode{2, (*CPU).addrImmediate, (*CPU).logImmediate,
		func(cpu *CPU) uint8 { return cpu.instB2 },
		func(cpu *CPU, b uint8) { cantWrite(cpu.inst.opcode, "immediate") },
	}
	implied = addrMode{1, (*CPU).addrImplied, (*CPU).logImplied,
		func(cpu *CPU) uint8 { return cantRead(cpu.inst.opcode, "implied") },
		func(cpu *CPU, b uint8) { cantWrite(cpu.inst.opcode, "implied") },
	}
	relative = addrMode{2, (*CPU).addrRelative, (*CPU).logRelative,
		func(cpu *CPU) uint8 { return cantRead(cpu.inst.opcode, "relative") },
		func(cpu *CPU, b uint8) { cantWrite(cpu.inst.opcode, "relative") },
	}

	cantRead = func(opcode uint8, name string) uint8 {
		panic(fmt.Sprintf("0x%02X: can't read() using %q addressing mode", opcode, name))
	}
	cantWrite = func(opcode uint8, name string) {
		panic(fmt.Sprintf("0x%02X: can't write() using %q addressing mode", opcode, name))
	}
)

func (cpu *CPU) step() {
	cpu.fetch()
	cpu.exec()
}

func (cpu *CPU) fetch() {
	cpu.prevpc = cpu.pc

	fetchByte := func() uint8 {
		b := cpu.mem[cpu.pc]
		cpu.pc++
		return b
	}

	cpu.inst = table[fetchByte()]
	if cpu.inst.mode.nbytes > 1 {
		cpu.instB2 = fetchByte()
	}
	if cpu.inst.mode.nbytes > 2 {
		cpu.instB3 = fetchByte()
	}
}

func (cpu *CPU) exec() {
	cpu.inst.exec(cpu)
	cpu.cycles += uint64(cpu.inst.ncycles)
}

func (cpu *CPU) setZN(b uint8) {
	cpu.z = b == 0
	cpu.n = b&0x80 != 0
}

func (cpu *CPU) push(b uint8) {
	cpu.mem[stackBase+uint16(cpu.sp)] = b
	cpu.sp--
}

func (cpu *CPU) pop() uint8 {
	cpu.sp++
	return cpu.mem[stackBase+uint16(cpu.sp)]
}

func (cpu *CPU) pushPC() {
	cpu.push(uint8((cpu.pc & 0xff00) >> 8))
	cpu.push(uint8(cpu.pc))
}

func (cpu *CPU) popPC() {
	cpu.pc = uint16(cpu.pop())
	cpu.pc |= uint16(cpu.pop()) << 8
}

func (cpu *CPU) branchIf(cond bool) {
	if cond {
		cpu.cycles++
		cpu.pc = cpu.addr()
	}
}

// read a byte using the current instruction addressing mode.
func (cpu *CPU) read() uint8 {
	if cpu.inst.mode.read != nil {
		return cpu.inst.mode.read(cpu)
	}
	return cpu.mem[cpu.addr()]
}

// read a byte using the current instruction addressing mode, but don't
// increment cycles when a page is crossed during addressing.
// this is used *only* for logging because another read() during logging would
// increment cycles too much.
func (cpu *CPU) readWithoutExtracycle() uint8 {
	if cpu.inst.mode.read != nil {
		return cpu.inst.mode.read(cpu)
	}
	addr, _ := cpu.inst.mode.addr(cpu)
	return cpu.mem[addr]
}

// write a byte using the current instruction addressing mode.
func (cpu *CPU) write(b uint8) {
	if cpu.inst.mode.write != nil {
		cpu.inst.mode.write(cpu, b)
		return
	}
	cpu.mem[cpu.addr()] = b
}

// addr returns the "real" address calculated using the current instruction
// addressing mode and increments cycles by cpu.inst.extracycle if a page is
// crossed during addressing.
func (cpu *CPU) addr() uint16 {
	addr, crossed := cpu.inst.mode.addr(cpu)
	if crossed {
		cpu.cycles += uint64(cpu.inst.extracycle)
	}
	return addr
}

func (cpu *CPU) addrAbsolute() (uint16, bool) {
	return uint16(cpu.instB3)<<8 | uint16(cpu.instB2), false
}

func (cpu *CPU) addrAbsoluteIndirect() (uint16, bool) {
	base, _ := cpu.addrAbsolute()
	base1 := ((base + 1) & 0x00ff) | (base & 0xff00)
	return uint16(cpu.mem[base1])<<8 | uint16(cpu.mem[base]), false
}

func (cpu *CPU) addrAbsoluteXIndexed() (uint16, bool) {
	base, _ := cpu.addrAbsolute()
	addr := base + uint16(cpu.x)
	return addr, pageCrossed(base, addr)
}

func (cpu *CPU) addrAbsoluteYIndexed() (uint16, bool) {
	base, _ := cpu.addrAbsolute()
	addr := base + uint16(cpu.y)
	return addr, pageCrossed(base, addr)
}

func (cpu *CPU) addrAccumulator() (uint16, bool) {
	return 0, false // placeholder address, should never be used to access memory.
}

func (cpu *CPU) addrImmediate() (uint16, bool) {
	return 0, false // placeholder address, should never be used to access memory.
}

func (cpu *CPU) addrImplied() (uint16, bool) {
	return 0, false // placeholder address, should never be used to access memory.
}

func (cpu *CPU) addrRelative() (uint16, bool) {
	if cpu.instB2&0x80 == 0 { // positive or 0
		addr := cpu.pc + uint16(cpu.instB2)
		return addr, pageCrossed(cpu.pc, addr)
	}
	addr := cpu.pc - uint16(^cpu.instB2+1)
	return addr, pageCrossed(cpu.pc, addr)
}

func (cpu *CPU) addrZeroPage() (uint16, bool) {
	return uint16(cpu.instB2), false
}

func (cpu *CPU) addrZeroPageIndirectYIndexed() (uint16, bool) {
	base := cpu.mem[cpu.instB2]
	lo := uint16(base) + uint16(cpu.y) // uint16 to get carry.
	base1 := cpu.mem[cpu.instB2+1]
	hi := base1 + uint8((lo&0x100)>>8)
	addr := uint16(hi)<<8 | uint16(lo&0xff)
	return addr, pageCrossed(uint16(base1)<<8|uint16(base), addr)
}

func (cpu *CPU) addrZeroPageXIndexed() (uint16, bool) {
	return uint16(cpu.instB2 + cpu.x), false
}

func (cpu *CPU) addrZeroPageXIndexedIndirect() (uint16, bool) {
	base := cpu.instB2 + cpu.x
	return uint16(cpu.mem[base+1])<<8 | uint16(cpu.mem[base]), false
}

func (cpu *CPU) addrZeroPageYIndexed() (uint16, bool) {
	return uint16(cpu.instB2 + cpu.y), false
}

func pageCrossed(a, b uint16) bool {
	return a&0xff00 != b&0xff00
}

// log returns a string containing the current CPU execution status using
// nestest.log format. see testroms/nestest.log
func (cpu *CPU) log() string {
	var s strings.Builder
	fmt.Fprintf(&s, "%04X  %02X ", cpu.prevpc, cpu.inst.opcode)

	b2 := "   "
	if cpu.inst.mode.nbytes > 1 {
		b2 = fmt.Sprintf("%02X ", cpu.instB2)
	}
	s.WriteString(b2)

	b3 := "   "
	if cpu.inst.mode.nbytes > 2 {
		b3 = fmt.Sprintf("%02X ", cpu.instB3)
	}
	s.WriteString(b3)

	fmt.Fprintf(&s, "%s ", cpu.inst.name)

	n, _ := s.WriteString(cpu.inst.mode.logAddr(cpu))
	for i := n; i < 28; i++ {
		s.WriteString(" ")
	}

	fmt.Fprintf(&s, "A:%02X X:%02X Y:%02X P:%02X SP:%02X", cpu.a, cpu.x, cpu.y, cpu.flags(), cpu.sp)
	fmt.Fprintf(&s, " PPU:%3d,%3d", 0, 0) // TODO
	fmt.Fprintf(&s, " CYC:%d", cpu.cycles)
	return s.String()
}

func (cpu *CPU) logAbsolute() string {
	addr, _ := cpu.addrAbsolute()
	if cpu.inst.opcode == 0x4c || cpu.inst.opcode == 0x20 {
		// jump instructions (JMP and JSR) don't log memory content.
		return fmt.Sprintf("$%04X", addr)
	}
	return fmt.Sprintf("$%04X = %02X", addr, cpu.readWithoutExtracycle())
}

func (cpu *CPU) logAbsoluteIndirect() string {
	base, _ := cpu.addrAbsolute()
	addr, _ := cpu.addrAbsoluteIndirect()
	return fmt.Sprintf("($%04X) = %04X", base, addr)
}

func (cpu *CPU) logAbsoluteXIndexed() string {
	base, _ := cpu.addrAbsolute()
	addr, _ := cpu.addrAbsoluteXIndexed()
	return fmt.Sprintf("$%04X,X @ %04X = %02X", base, addr, cpu.readWithoutExtracycle())
}

func (cpu *CPU) logAbsoluteYIndexed() string {
	base, _ := cpu.addrAbsolute()
	addr, _ := cpu.addrAbsoluteYIndexed()
	return fmt.Sprintf("$%04X,Y @ %04X = %02X", base, addr, cpu.readWithoutExtracycle())
}

func (cpu *CPU) logAccumulator() string {
	return "A"
}

func (cpu *CPU) logImmediate() string {
	return fmt.Sprintf("#$%02X", cpu.readWithoutExtracycle())
}

func (cpu *CPU) logImplied() string {
	return ""
}

func (cpu *CPU) logRelative() string {
	addr, _ := cpu.addrRelative()
	return fmt.Sprintf("$%04X", addr)
}

func (cpu *CPU) logZeroPage() string {
	addr, _ := cpu.addrZeroPage()
	return fmt.Sprintf("$%02X = %02X", addr, cpu.readWithoutExtracycle())
}

func (cpu *CPU) logZeroPageIndirectYIndexed() string {
	base := uint16(cpu.mem[cpu.instB2+1])<<8 | uint16(cpu.mem[cpu.instB2])
	addr, _ := cpu.addrZeroPageIndirectYIndexed()
	return fmt.Sprintf("($%02X),Y = %04X @ %04X = %02X", cpu.instB2, base, addr, cpu.readWithoutExtracycle())
}

func (cpu *CPU) logZeroPageXIndexed() string {
	addr, _ := cpu.addrZeroPageXIndexed()
	return fmt.Sprintf("$%02X,X @ %02X = %02X", cpu.instB2, addr, cpu.readWithoutExtracycle())
}

func (cpu *CPU) logZeroPageXIndexedIndirect() string {
	base, _ := cpu.addrZeroPageXIndexed()
	addr, _ := cpu.addrZeroPageXIndexedIndirect()
	return fmt.Sprintf("($%02X,X) @ %02X = %04X = %02X", cpu.instB2, base, addr, cpu.readWithoutExtracycle())
}

func (cpu *CPU) logZeroPageYIndexed() string {
	addr, _ := cpu.addrZeroPageYIndexed()
	return fmt.Sprintf("$%02X,Y @ %02X = %02X", cpu.instB2, addr, cpu.readWithoutExtracycle())
}

// flags returns the CPU status flags packed into a byte.
func (cpu *CPU) flags() uint8 {
	b := uint8(0)
	if cpu.n {
		b |= 0x80
	}
	if cpu.v {
		b |= 0x40
	}

	b |= 0x20 // bit 5 is always set to 1.

	if cpu.b {
		b |= 0x10
	}
	if cpu.d {
		b |= 0x08
	}
	if cpu.i {
		b |= 0x04
	}
	if cpu.z {
		b |= 0x02
	}
	if cpu.c {
		b |= 0x01
	}
	return b
}

// lax load accumulator and index register x from memory.
func (cpu *CPU) lax() {
	b := cpu.read()
	cpu.a = b
	cpu.x = b
	cpu.setZN(b)
}

// lda load accumulator with memory.
func (cpu *CPU) lda() {
	cpu.a = cpu.read()
	cpu.setZN(cpu.a)
}

// ldx load index register x from memory.
func (cpu *CPU) ldx() {
	cpu.x = cpu.read()
	cpu.setZN(cpu.x)
}

// ldy load index register y from memory.
func (cpu *CPU) ldy() {
	cpu.y = cpu.read()
	cpu.setZN(cpu.y)
}

// sax store accumulator "and" index register x in memory.
func (cpu *CPU) sax() {
	cpu.write(cpu.a & cpu.x)
}

// sta store accumulator in memory.
func (cpu *CPU) sta() {
	cpu.write(cpu.a)
}

// stx store index register x in memory.
func (cpu *CPU) stx() {
	cpu.write(cpu.x)
}

// sty store index register y in memory.
func (cpu *CPU) sty() {
	cpu.write(cpu.y)
}

// tax transfer accumulator to index x.
func (cpu *CPU) tax() {
	cpu.x = cpu.a
	cpu.setZN(cpu.x)
}

// tay transfer accumula tor to index y.
func (cpu *CPU) tay() {
	cpu.y = cpu.a
	cpu.setZN(cpu.y)
}

// tsx transfer stack pointer to index x.
func (cpu *CPU) tsx() {
	cpu.x = cpu.sp
	cpu.setZN(cpu.x)
}

// txa transfer index x to accumulator.
func (cpu *CPU) txa() {
	cpu.a = cpu.x
	cpu.setZN(cpu.a)
}

// txs transfer index x to stack pointer.
func (cpu *CPU) txs() {
	cpu.sp = cpu.x
}

// tya transfer index y to accumulator.
func (cpu *CPU) tya() {
	cpu.a = cpu.y
	cpu.setZN(cpu.a)
}

// pha push accumulator on stack.
func (cpu *CPU) pha() {
	cpu.push(cpu.a)
}

// pla pull accumulator from stack.
func (cpu *CPU) pla() {
	cpu.a = cpu.pop()
	cpu.setZN(cpu.a)
}

// php push processor status on stack.
func (cpu *CPU) php() {
	// https://www.nesdev.org/wiki/Status_flags#The_B_flag
	// bit 5 is always set to 1, bit 4 is 1 if from an instruction (php or
	// brk) or 0 if from an interrupt (irq or nmi).
	cpu.push(cpu.flags() | 0x10)
}

// plp pull processor status from stack.
func (cpu *CPU) plp() {
	// two instructions (plp and rti) pull a byte from the stack and set all
	// the flags. they ignore bits 5 and 4.
	b := cpu.pop()
	cpu.n = b&0x80 != 0
	cpu.v = b&0x40 != 0
	cpu.d = b&0x08 != 0
	cpu.i = b&0x04 != 0
	cpu.z = b&0x02 != 0
	cpu.c = b&0x01 != 0
}

// asl arithmetic shift left.
func (cpu *CPU) asl() {
	b := cpu.read()
	cpu.c = b&0x80 != 0
	b <<= 1
	cpu.write(b)
	cpu.setZN(b)
}

// lsr logical shift right.
func (cpu *CPU) lsr() {
	b := cpu.read()
	cpu.c = b&0x01 != 0
	b >>= 1
	cpu.write(b)
	cpu.setZN(b)
}

// rol rotate left.
func (cpu *CPU) rol() {
	b := cpu.read()
	c := b&0x80 != 0
	b <<= 1
	if cpu.c {
		b |= 0x01
	}
	cpu.c = c
	cpu.write(b)
	cpu.setZN(b)
}

// ror rotate right.
func (cpu *CPU) ror() {
	b := cpu.read()
	c := b&0x01 != 0
	b >>= 1
	if cpu.c {
		b |= 0x80
	}
	cpu.c = c
	cpu.write(b)
	cpu.setZN(b)
}

// and "and" memory with accumulator.
func (cpu *CPU) and() {
	cpu.a = cpu.a & cpu.read()
	cpu.setZN(cpu.a)
}

// bit test bits in memory with accumulator.
func (cpu *CPU) bit() {
	b := cpu.read()
	cpu.z = cpu.a&b == 0
	cpu.n = b&0x80 != 0
	cpu.v = b&0x40 != 0
}

// eor "exclusive or" memory with accumulator.
func (cpu *CPU) eor() {
	cpu.a = cpu.a ^ cpu.read()
	cpu.setZN(cpu.a)
}

// ora "or" memory with accumulator.
func (cpu *CPU) ora() {
	cpu.a = cpu.a | cpu.read()
	cpu.setZN(cpu.a)
}

// adc add memory to accumulator with carry.
func (cpu *CPU) adc() {
	b := cpu.read()
	r := cpu.a + b
	c := r < cpu.a
	if cpu.c {
		r++
		c = c || r == 0
	}
	cpu.c = c
	cpu.v = r&0x80 != cpu.a&0x80 && r&0x80 != b&0x80
	cpu.a = r
	cpu.setZN(cpu.a)
}

// cmp compare memory and accumulator.
func (cpu *CPU) cmp() {
	b := cpu.read()
	cpu.c = b <= cpu.a
	cpu.setZN(cpu.a - b)
}

// cpx compare index register x to memory.
func (cpu *CPU) cpx() {
	b := cpu.read()
	cpu.c = cpu.x >= b
	cpu.setZN(cpu.x - b)
}

// cpy compare index register y to memory.
func (cpu *CPU) cpy() {
	b := cpu.read()
	cpu.c = cpu.y >= b
	cpu.setZN(cpu.y - b)
}

// dcp decrement memory by one then compare with accumulator.
func (cpu *CPU) dcp() {
	b := cpu.read() - 1
	cpu.write(b)
	cpu.c = b <= cpu.a
	cpu.setZN(cpu.a - b)
}

// isb increment memory by one then sbc.
func (cpu *CPU) isb() {
	cpu.write(cpu.read() + 1)
	cpu.sbc()
}

// rla rotate left then "and" with accumulator.
func (cpu *CPU) rla() {
	cpu.rol()
	cpu.and()
}

// rra rotate right and add memory to accumulator.
func (cpu *CPU) rra() {
	cpu.ror()
	cpu.adc()
}

// sbc subtract memory from accumulator with borrow.
func (cpu *CPU) sbc() {
	// it's just adc with complemented memory operand.
	b := ^cpu.read()
	r := cpu.a + b
	c := r < cpu.a
	if cpu.c {
		r++
		c = c || r == 0
	}
	cpu.c = c
	cpu.v = r&0x80 != cpu.a&0x80 && r&0x80 != b&0x80
	cpu.a = r
	cpu.setZN(cpu.a)
}

// slo arithmetic shift left then "or" memory with accumulator.
func (cpu *CPU) slo() {
	cpu.asl()
	cpu.ora()
}

// sre logical shift right then "exclusive or" memory with accumulator.
func (cpu *CPU) sre() {
	cpu.lsr()
	cpu.eor()
}

// dec decrement memory by one.
func (cpu *CPU) dec() {
	b := cpu.read() - 1
	cpu.write(b)
	cpu.setZN(b)
}

// dex decrement index register x by one.
func (cpu *CPU) dex() {
	cpu.x--
	cpu.setZN(cpu.x)
}

// dey decrement index register y by one.
func (cpu *CPU) dey() {
	cpu.y--
	cpu.setZN(cpu.y)
}

// inc increment memory by one.
func (cpu *CPU) inc() {
	b := cpu.read() + 1
	cpu.write(b)
	cpu.setZN(b)
}

// inx increment index register x by one.
func (cpu *CPU) inx() {
	cpu.x++
	cpu.setZN(cpu.x)
}

// iny increment index register y by one.
func (cpu *CPU) iny() {
	cpu.y++
	cpu.setZN(cpu.y)
}

// brk break command.
func (cpu *CPU) brk() {
	cpu.b = true
	cpu.pc++
	cpu.pushPC()
	cpu.php()
	cpu.sei()
	cpu.pc = uint16(cpu.mem[brkVector])
	cpu.pc |= uint16(cpu.mem[brkVector+1]) << 8
}

// jmp jmp indirect.
func (cpu *CPU) jmp() {
	cpu.pc = cpu.addr()
}

// jsr jump to subroutine.
func (cpu *CPU) jsr() {
	// PC was already incremented, but we need to push the address of the
	// last byte of the JSR instruction onto the stack.
	cpu.pc--
	cpu.pushPC()
	cpu.pc = cpu.addr()
}

// rts return from subroutme.
func (cpu *CPU) rts() {
	cpu.popPC()
	cpu.pc++
}

// rti return from interrupt.
func (cpu *CPU) rti() {
	cpu.plp()
	cpu.popPC()
}

// bcc branch on carry clear.
func (cpu *CPU) bcc() {
	cpu.branchIf(!cpu.c)
}

// bcs branch on carry set.
func (cpu *CPU) bcs() {
	cpu.branchIf(cpu.c)
}

// bne branch on result not zero.
func (cpu *CPU) bne() {
	cpu.branchIf(!cpu.z)
}

// beq branch on result zero.
func (cpu *CPU) beq() {
	cpu.branchIf(cpu.z)
}

// bpl branch on result plus.
func (cpu *CPU) bpl() {
	cpu.branchIf(!cpu.n)
}

// bmi branch on result minus.
func (cpu *CPU) bmi() {
	cpu.branchIf(cpu.n)
}

// bvc branch on overflow clear.
func (cpu *CPU) bvc() {
	cpu.branchIf(!cpu.v)
}

// bvs branch on overflow set.
func (cpu *CPU) bvs() {
	cpu.branchIf(cpu.v)
}

// clc clear carry flag.
func (cpu *CPU) clc() {
	cpu.c = false
}

// cld clear decimal mode.
func (cpu *CPU) cld() {
	cpu.d = false
}

// cli clear interrupt disable.
func (cpu *CPU) cli() {
	cpu.i = false
}

// clv clear overflow flag.
func (cpu *CPU) clv() {
	cpu.v = false
}

// sec set carry flag.
func (cpu *CPU) sec() {
	cpu.c = true
}

// sed set decimal mode.
func (cpu *CPU) sed() {
	cpu.d = true
}

// sei set interrupt disable.
func (cpu *CPU) sei() {
	cpu.i = true
}

// nop no operation.
func (cpu *CPU) nop() {
	// this is to make illegal nops work since they have an addressing mode
	// which could require an extra cycle on page cross.
	cpu.addr()
}

// unk unknown instruction, panic!
func (cpu *CPU) unk() {
	panic(fmt.Sprintf("unknown opcode: 0x%02X", cpu.inst.opcode))
}
