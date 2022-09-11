// See license file for copyright and license details.

package nes

import "fmt"

// TODO this is just a basic stub implementation of the bus and memory mappings.
type Bus struct {
	ram [0x0800]uint8
	ppu [0x0008]uint8
	apu [0x0020]uint8
	car [0xbfe0]uint8
}

func (bus *Bus) Read(addr uint16) uint8 {
	switch {
	case addr <= 0x1fff:
		return bus.ram[addr&0x07ff]
	case addr <= 0x3fff:
		return bus.ppu[addr&0x0007]
	case addr <= 0x401f:
		return bus.apu[addr&0x001f]
	case addr <= 0xffff:
		return bus.car[addr-0x4020]
	default:
		panic(fmt.Sprintf("address out of range: 0x%04X", addr))
	}
}

func (bus *Bus) Write(addr uint16, b uint8) {
	switch {
	case addr <= 0x1fff:
		bus.ram[addr&0x07ff] = b
	case addr <= 0x3fff:
		bus.ppu[addr&0x0007] = b
	case addr <= 0x401f:
		bus.apu[addr&0x001f] = b
	case addr <= 0xffff:
		bus.car[addr-0x4020] = b
	default:
		panic(fmt.Sprintf("address out of range: 0x%04X", addr))
	}
}
