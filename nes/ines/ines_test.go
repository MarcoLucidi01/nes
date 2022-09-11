// See license file for copyright and license details.

package ines

import (
	"bytes"
	"testing"
)

// TestReadHeader tests iNes header parsing.
func TestReadHeader(t *testing.T) {
	tests := []struct {
		header  []uint8
		mustErr bool
		ines    Ines
	}{
		{
			mustErr: true,
		}, {
			header:  []uint8("\x4e\x45\x53\x1a\x02\x01\x01\x00"),
			mustErr: true,
		}, {
			header:  []uint8("\x44\x55\x66\x1a\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00"),
			mustErr: true,
		}, {
			header:  []uint8("\x4e\x45\x53\x1a\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00"),
			mustErr: false,
			ines: Ines{
				PrgBanks:          2,
				PrgRamBanks:       1,
				ChrBanks:          1,
				Mirroring:         Vertical,
				HasBattery:        false,
				HasTrainer:        false,
				HasFourScreenVRam: false,
				Mapper:            0,
				Console:           NES,
				TvSystem:          NTSC,
			},
		}, {
			header:  []uint8("\x4e\x45\x53\x1a\x02\x01\x01\x08\x00\x00\x00\x00\x02\x00\x00\x01"),
			mustErr: false,
			ines: Ines{
				PrgBanks:          2,
				PrgRamBanks:       1,
				ChrBanks:          1,
				Mirroring:         Vertical,
				HasBattery:        false,
				HasTrainer:        false,
				HasFourScreenVRam: false,
				Mapper:            0,
				Console:           NES,
				TvSystem:          NTSC,
			},
		}, {
			header:  []uint8("\x4e\x45\x53\x1a\x10\x10\x40\x08\x00\x00\x07\x00\x00\x00\x00\x01"),
			mustErr: false,
			ines: Ines{
				PrgBanks:          16,
				PrgRamBanks:       1,
				ChrBanks:          16,
				Mirroring:         Horizontal,
				HasBattery:        false,
				HasTrainer:        false,
				HasFourScreenVRam: false,
				Mapper:            4,
				Console:           NES,
				TvSystem:          NTSC,
			},
		}, {
			header:  []uint8("\x4e\x45\x53\x1a\x02\x02\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00"),
			mustErr: false,
			ines: Ines{
				PrgBanks:          2,
				PrgRamBanks:       1,
				ChrBanks:          2,
				Mirroring:         Horizontal,
				HasBattery:        false,
				HasTrainer:        false,
				HasFourScreenVRam: false,
				Mapper:            1,
				Console:           NES,
				TvSystem:          NTSC,
			},
		}, {
			header:  []uint8("\x4e\x45\x53\x1a\x20\x20\x42\x08\x00\x00\x70\x00\x00\x00\x00\x01"),
			mustErr: false,
			ines: Ines{
				PrgBanks:          32,
				PrgRamBanks:       1,
				ChrBanks:          32,
				Mirroring:         Horizontal,
				HasBattery:        true,
				HasTrainer:        false,
				HasFourScreenVRam: false,
				Mapper:            4,
				Console:           NES,
				TvSystem:          NTSC,
			},
		}, {
			header:  []uint8("\x4e\x45\x53\x1a\x08\x00\x12\x08\x00\x00\x70\x07\x00\x00\x00\x01"),
			mustErr: false,
			ines: Ines{
				PrgBanks:          8,
				PrgRamBanks:       1,
				ChrBanks:          0,
				Mirroring:         Horizontal,
				HasBattery:        true,
				HasTrainer:        false,
				HasFourScreenVRam: false,
				Mapper:            1,
				Console:           NES,
				TvSystem:          NTSC,
			},
		}, {
			header:  []uint8("\x4e\x45\x53\x1a\x10\x00\x10\x08\x00\x00\x00\x07\x00\x00\x00\x01"),
			mustErr: false,
			ines: Ines{
				PrgBanks:          16,
				PrgRamBanks:       1,
				ChrBanks:          0,
				Mirroring:         Horizontal,
				HasBattery:        false,
				HasTrainer:        false,
				HasFourScreenVRam: false,
				Mapper:            1,
				Console:           NES,
				TvSystem:          NTSC,
			},
		}, {
			header:  []uint8("\x4e\x45\x53\x1a\x08\x00\x21\x08\x20\x00\x00\x07\x00\x00\x00\x01"),
			mustErr: false,
			ines: Ines{
				PrgBanks:          8,
				PrgRamBanks:       32,
				ChrBanks:          0,
				Mirroring:         Vertical,
				HasBattery:        false,
				HasTrainer:        false,
				HasFourScreenVRam: false,
				Mapper:            2,
				Console:           NES,
				TvSystem:          NTSC,
			},
		}, {
			header:  []uint8("\x4e\x45\x53\x1a\x08\x00\x10\x08\x00\x00\x07\x07\x00\x00\x00\x01"),
			mustErr: false,
			ines: Ines{
				PrgBanks:          8,
				PrgRamBanks:       1,
				ChrBanks:          0,
				Mirroring:         Horizontal,
				HasBattery:        false,
				HasTrainer:        false,
				HasFourScreenVRam: false,
				Mapper:            1,
				Console:           NES,
				TvSystem:          NTSC,
			},
		}, {
			header:  []uint8("\x4e\x45\x53\x1a\x01\x01\x00\x08\x00\x00\x00\x00\x00\x00\x00\x01"),
			mustErr: false,
			ines: Ines{
				PrgBanks:          1,
				PrgRamBanks:       1,
				ChrBanks:          1,
				Mirroring:         Horizontal,
				HasBattery:        false,
				HasTrainer:        false,
				HasFourScreenVRam: false,
				Mapper:            0,
				Console:           NES,
				TvSystem:          NTSC,
			},
		}, {
			header:  []uint8("\x4e\x45\x53\x1a\x08\x10\x40\x08\x00\x00\x07\x00\x00\x00\x00\x01"),
			mustErr: false,
			ines: Ines{
				PrgBanks:          8,
				PrgRamBanks:       1,
				ChrBanks:          16,
				Mirroring:         Horizontal,
				HasBattery:        false,
				HasTrainer:        false,
				HasFourScreenVRam: false,
				Mapper:            4,
				Console:           NES,
				TvSystem:          NTSC,
			},
		}, {
			header:  []uint8("\x4e\x45\x53\x1a\x20\x20\xa0\x58\x00\x00\x00\x00\x00\x00\x00\x01"),
			mustErr: false,
			ines: Ines{
				PrgBanks:          32,
				PrgRamBanks:       1,
				ChrBanks:          32,
				Mirroring:         Horizontal,
				HasBattery:        false,
				HasTrainer:        false,
				HasFourScreenVRam: false,
				Mapper:            90,
				Console:           NES,
				TvSystem:          NTSC,
			},
		}, {
			header:  []uint8("\x4e\x45\x53\x1a\x08\x10\x12\x08\x00\x00\x70\x00\x00\x00\x00\x01"),
			mustErr: false,
			ines: Ines{
				PrgBanks:          8,
				PrgRamBanks:       1,
				ChrBanks:          16,
				Mirroring:         Horizontal,
				HasBattery:        true,
				HasTrainer:        false,
				HasFourScreenVRam: false,
				Mapper:            1,
				Console:           NES,
				TvSystem:          NTSC,
			},
		},
	}

	for i, exp := range tests {
		got, err := ReadHeader(bytes.NewBuffer(exp.header))
		mustEqual(t, i, "mustErr", exp.mustErr, err != nil)
		mustEqual(t, i, "PrgBanks", exp.ines.PrgBanks, got.PrgBanks)
		mustEqual(t, i, "PrgRamBanks", exp.ines.PrgRamBanks, got.PrgRamBanks)
		mustEqual(t, i, "ChrBanks", exp.ines.ChrBanks, got.ChrBanks)
		mustEqual(t, i, "Mirroring", exp.ines.Mirroring, got.Mirroring)
		mustEqual(t, i, "HasBattery", exp.ines.HasBattery, got.HasBattery)
		mustEqual(t, i, "HasTrainer", exp.ines.HasTrainer, got.HasTrainer)
		mustEqual(t, i, "HasFourScreenVRam", exp.ines.HasFourScreenVRam, got.HasFourScreenVRam)
		mustEqual(t, i, "Mapper", exp.ines.Mapper, got.Mapper)
		mustEqual(t, i, "Console", exp.ines.Console, got.Console)
		mustEqual(t, i, "TvSystem", exp.ines.TvSystem, got.TvSystem)
	}
}

func mustEqual[T comparable](t *testing.T, i int, name string, exp, got T) {
	if exp != got {
		t.Fatalf("%d: %s: expected %v got %v", i, name, exp, got)
	}
}
