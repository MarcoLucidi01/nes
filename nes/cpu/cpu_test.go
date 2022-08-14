// See license file for copyright and license details.

package cpu

import (
	"bufio"
	"os"
	"regexp"
	"strconv"
	"testing"
)

const nestestFailFmt = `nestest.log:%d
prev:
    %s
expected:
    %s
got:
    %s

expected PS:
    nv-bdizc
    %08b
got PS:
    nv-bdizc
    %08b
`

func TestNestestNoPPU(t *testing.T) {
	rom, err := os.ReadFile("testdata/nestest.nes")
	if err != nil {
		t.Fatal(err)
	}

	logFile, err := os.Open("testdata/nestest.log")
	if err != nil {
		t.Fatal(err)
	}
	defer logFile.Close()

	cpu := CPU{
		sp:     0xfd,
		pc:     0xc000,
		i:      true,
		cycles: 7,
		mem:    make([]uint8, 1<<16),
	}
	copy(cpu.mem[0x8000:0xBFFF], rom[0x0010:0x4010])
	copy(cpu.mem[0xC000:0xFFFF], rom[0x0010:0x4010])

	// TODO these addresses are APU memory map which are not implemented
	// yet, putting 0xff there makes nestest.log pass the last few lines.
	cpu.mem[0x4004] = 0xff
	cpu.mem[0x4005] = 0xff
	cpu.mem[0x4006] = 0xff
	cpu.mem[0x4007] = 0xff
	cpu.mem[0x4015] = 0xff

	rePPU := regexp.MustCompile(" PPU:...,...")
	rePS := regexp.MustCompile(" P:(..)")

	prev := ""
	scan := bufio.NewScanner(logFile)
	for i := 1; scan.Scan(); i++ {
		exp := rePPU.ReplaceAllString(scan.Text(), "")
		expPS, err := strconv.ParseInt(rePS.FindStringSubmatch(exp)[1], 16, 64)
		if err != nil {
			t.Fatal(err)
		}

		cpu.fetch()

		got := rePPU.ReplaceAllString(cpu.log(), "")
		gotPS := cpu.flags()
		if exp != got {
			t.Fatalf(nestestFailFmt, i, prev, exp, got, expPS, gotPS)
		}
		prev = got

		cpu.exec()
	}
	if scan.Err() != nil {
		t.Fatal(err)
	}
}

func Test6502FunctionalTest(t *testing.T) {
	rom, err := os.ReadFile("testdata/6502_functional_test.bin")
	if err != nil {
		t.Fatal(err)
	}

	cpu := CPU{
		sp:  0xfd,
		pc:  0x0400,
		i:   true,
		mem: make([]uint8, 1<<16),
	}
	copy(cpu.mem[0x000a:], rom)

	for {
		cpu.fetch()
		//fmt.Println(cpu.log())
		cpu.exec()
		if cpu.prevpc == cpu.pc {
			if cpu.pc == 0x3699 {
				return // if we get to 0x3699 SUCCESS!
			}
			t.Fatalf("detected infinite loop at 0x%04X", cpu.pc)
		}
	}
}

func TestTTL6502(t *testing.T) {
	rom, err := os.ReadFile("testdata/TTL6502.bin")
	if err != nil {
		t.Fatal(err)
	}

	cpu := CPU{
		sp:  0xfd,
		pc:  0xc000,
		i:   true,
		mem: make([]uint8, 1<<16),
	}
	copy(cpu.mem[0xe000:], rom)

	for {
		cpu.fetch()
		//fmt.Println(cpu.log())
		cpu.exec()
		if cpu.pc == 0xf5b6 {
			// after 0xf5b6, TTL6502.bin tests behavior that is
			// slightly different between the original 6502 and the
			// version used in the NES.
			return
		}
		if cpu.prevpc == cpu.pc {
			t.Fatalf("detected infinite loop at 0x%04X", cpu.pc)
		}
	}
}
