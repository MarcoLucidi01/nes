// See license file for copyright and license details.

package nes

import (
	"io"

	"github.com/MarcoLucidi01/nes/nes/ines"
)

type Rom struct {
	ines.Ines // header

	Trainer []uint8 // trainer data if present
	Prg     []uint8 // PRG ROM data
	Chr     []uint8 // CHR ROM data if present
}

func ReadRom(r io.Reader) (Rom, error) {
	header, err := ines.ReadHeader(r)
	if err != nil {
		return Rom{}, err
	}

	rom := Rom{Ines: header}

	if rom.HasTrainer {
		rom.Trainer = make([]uint8, ines.TrainerSize)
		if _, err := io.ReadFull(r, rom.Trainer); err != nil {
			return Rom{}, err
		}
	}

	rom.Prg = make([]uint8, uint(rom.PrgBanks)*ines.PrgUnit)
	if _, err := io.ReadFull(r, rom.Prg); err != nil {
		return Rom{}, err
	}

	if rom.ChrBanks > 0 {
		rom.Chr = make([]uint8, uint(rom.ChrBanks)*ines.ChrUnit)
		if _, err := io.ReadFull(r, rom.Chr); err != nil {
			return Rom{}, err
		}
	}

	return rom, nil
}
