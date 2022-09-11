// See license file for copyright and license details.

package cpu

import (
	"bufio"
	"os"
	"regexp"
	"strconv"
	"testing"

	"github.com/MarcoLucidi01/nes/nes"
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
	logFile, err := os.Open("testdata/nestest.log")
	if err != nil {
		t.Fatal(err)
	}
	defer logFile.Close()

	var bus nes.Bus
	if err := bus.LoadRom("testdata/nestest.nes"); err != nil {
		t.Fatal(err)
	}

	cpu := CPU{
		sp:     0xfd,
		pc:     0xc000,
		i:      true,
		cycles: 7,
		bus:    &bus,
	}

	// TODO these addresses are APU memory map which are not implemented
	// yet, putting 0xff there makes nestest.log pass the last few lines.
	cpu.bus.Write(0x4004, 0xff)
	cpu.bus.Write(0x4005, 0xff)
	cpu.bus.Write(0x4006, 0xff)
	cpu.bus.Write(0x4007, 0xff)
	cpu.bus.Write(0x4015, 0xff)

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

	var bus nes.Bus
	cpu := CPU{
		sp:  0xfd,
		pc:  0x4020,
		i:   true,
		bus: &bus,
	}
	for i, b := range rom {
		bus.Write(0x4020+uint16(i), b)
	}

	for {
		cpu.fetch()
		//fmt.Println(cpu.log())
		cpu.exec()
		if cpu.prevpc == cpu.pc {
			if cpu.pc == 0x72d9 {
				return // if we get here SUCCESS!
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

	var bus nes.Bus
	cpu := CPU{
		sp:  0xfd,
		pc:  0xc000,
		i:   true,
		bus: &bus,
	}
	for i, b := range rom {
		bus.Write(0xe000+uint16(i), b)
	}

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
