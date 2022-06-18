#include <cstdint>
#include <vector>

#define noop static_cast<void>(0)

using dzbool = bool;
using dzint = std::int64_t;

template<typename T>
using dzlist = std::vector<T>;
using dzbytes = dzlist<std::uint8_t>;

struct Memory {
  Memory() {
    rom.resize(0x4000, 0);
    vram.resize(0x2000, 0);
    eram.resize(0x2000, 0);
    wram.resize(0x2000, 0);
    oam.resize(0x100, 0);
    io.resize(0x80, 0);
    hram.resize(0x7F, 0);
    ie = 0;
  }

  auto read_byte(dzint addr) const -> dzint {
    if (addr > 0xFFFF) {
      return 0;
    } else if (addr >= 0xFFFF) {
      return ie;
    } else if (addr >= 0xFF80) {
      return hram[addr - 0xFF80];
    } else if (addr >= 0xFF00) {
      return io[addr - 0xFF00];
    } else if (addr >= 0xFEA0) {
      return 0;
    } else if (addr >= 0xFE00) {
      return oam[addr - 0xFE00];
    } else if (addr >= 0xE000) {
      return wram[addr - 0xE000];
    } else if (addr >= 0xC000) {
      return wram[addr - 0xC000];
    } else if (addr >= 0xA000) {
      return eram[addr - 0xA000];
    } else if (addr >= 0x8000) {
      return vram[addr - 0x8000];
    } else if (addr >= 0x4000) {
      return 0;  // Todo: implement, see https://gbdev.io/pandocs/The_Cartridge_Header.html#the-cartridge-header
    } else {
      return rom[addr];
    }
  }

  auto read_half(dzint addr) const -> dzint {
    addr = addr & ~0x1;
    return read_byte(addr) | (read_byte(addr + 1) << 8);
  }

  void write_byte(dzint addr, dzint byte) {
    if (addr > 0xFFFF) {
      noop;
    } else if (addr >= 0xFFFF) {
      ie = byte;
    } else if (addr >= 0xFF80) {
      hram[addr - 0xFF80] = byte;
    } else if (addr >= 0xFF00) {
      io[addr - 0xFF00] = byte;
    } else if (addr >= 0xFEA0) {
      noop;
    } else if (addr >= 0xFE00) {
      oam[addr - 0xFE00] = byte;
    } else if (addr >= 0xE000) {
      wram[addr - 0xE000] = byte;
    } else if (addr >= 0xC000) {
      wram[addr - 0xC000] = byte;
    } else if (addr >= 0xA000) {
      eram[addr - 0xA000] = byte;
    } else if (addr >= 0x8000) {
      vram[addr - 0x8000] = byte;
    } else if (addr >= 0x4000) {
      noop;  // Todo: implement, see https://gbdev.io/pandocs/The_Cartridge_Header.html#the-cartridge-header
    } else {
      rom[addr] = byte;
    }
  }

  void write_half(dzint addr, dzint half) {
    addr = addr & ~0x1;
    write_byte(addr + 0, (half >> 0) & 0xFF);
    write_byte(addr + 1, (half >> 8) & 0xFF);
  }

  dzbytes rom;
  dzbytes vram;
  dzbytes eram;
  dzbytes wram;
  dzbytes oam;
  dzbytes io;
  dzbytes hram;
  dzint ie;
};

struct Cpu {
  dzint a = 0;
  dzint b = 0;
  dzint c = 0;
  dzint d = 0;
  dzint e = 0;
  dzint f = 0;

  auto zero() const -> dzint {
    return (f >> 7) & 0x1;
  }

  auto subtraction() const -> dzint {
    return (f >> 6) & 0x1;
  }

  auto half_carry() const -> dzint {
    return (f >> 5) & 0x1;
  }

  auto carry() const -> dzint {
    return (f >> 4) & 0x1;
  }
};

struct GameBoy {
  Cpu cpu;
  Memory memory;
};

int main(int argc, char* argv[]) {
  GameBoy gb;
  return 0;
}
