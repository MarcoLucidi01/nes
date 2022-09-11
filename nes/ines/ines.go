// See license file for copyright and license details.

package ines

import (
	"errors"
	"io"
)

const (
	magic       = "NES\x1a"
	headerSize  = 0x10
	TrainerSize = 0x200
	PrgUnit     = 0x4000
	PrgRamUnit  = 0x2000
	ChrUnit     = 0x2000
)

type MirroringType int

const (
	Horizontal MirroringType = iota
	Vertical
)

type ConsoleType int

const (
	NES ConsoleType = iota
	VsSystem
	PlayChoice
	UnknownConsole
)

type TvSystemType int

const (
	NTSC TvSystemType = iota
	PAL
)

var (
	errUnsupported = errors.New("unsupported rom header")
)

// Ines contains information read/parsed from the rom header.
// see https://www.nesdev.org/wiki/INES
type Ines struct {
	PrgBanks          uint8
	PrgRamBanks       uint8
	ChrBanks          uint8
	Mirroring         MirroringType
	HasBattery        bool
	HasTrainer        bool
	HasFourScreenVRam bool
	Mapper            uint8
	Console           ConsoleType
	TvSystem          TvSystemType
}

// ReadHeader reads the iNes header from r.
// it supports only iNes 1.0 file format at the moment.
func ReadHeader(r io.Reader) (Ines, error) {
	var header [headerSize]uint8
	if _, err := io.ReadFull(r, header[:]); err != nil {
		return Ines{}, err
	}

	if string(header[:len(magic)]) != magic {
		return Ines{}, errUnsupported
	}

	var ines Ines
	ines.PrgBanks = header[4]
	ines.ChrBanks = header[5]

	ines.Mirroring = MirroringType(header[6] & 0x01)
	ines.HasBattery = header[6]&0x02 != 0
	ines.HasTrainer = header[6]&0x04 != 0
	ines.HasFourScreenVRam = header[6]&0x08 != 0
	ines.Mapper = (header[6] & 0xf0) >> 4

	ines.Console = ConsoleType(header[7] & 0x03)
	ines.Mapper |= header[7] & 0xf0

	ines.PrgRamBanks = header[8]
	if ines.PrgRamBanks == 0 {
		ines.PrgRamBanks = 1
	}

	ines.TvSystem = TvSystemType(header[9] & 0x01)

	return ines, nil
}
