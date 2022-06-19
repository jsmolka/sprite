#include <cstdint>
#include <optional>
#include <vector>

#define noop static_cast<void>(0)
#define null std::nullopt

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
  dzint f = 0;
  dzint b = 0;
  dzint c = 0;
  dzint d = 0;
  dzint e = 0;
  dzint h = 0;
  dzint l = 0;

  dzint pc = 0;
  dzint sp = 0;
  Memory memory;

  auto bc() const -> dzint {
    return c | (b << 8);
  }

  void set_bc(dzint value) {
    c = value & 0xFF;
    b = (value >> 8) & 0xFF;
  }

  auto de() const -> dzint {
    return d | (e << 8);
  }

  void set_de(dzint value) {
    e = value & 0xFF;
    d = (value >> 8) & 0xFF;
  }

  auto hl() const -> dzint {
    return l | (h << 8);
  }

  void set_hl(dzint value) {
    l = value & 0xFF;
    h = (value >> 8) & 0xFF;
  }

  auto fz() const -> dzint {
    return (f >> 7) & 0x1;
  }

  auto fn() const -> dzint {
    return (f >> 6) & 0x1;
  }

  auto fh() const -> dzint {
    return (f >> 5) & 0x1;
  }

  auto fc() const -> dzint {
    return (f >> 4) & 0x1;
  }

  void set_f(std::optional<dzbool> z, std::optional<dzbool> n, std::optional<dzbool> h, std::optional<dzbool> c) {
    if (z != null) {
      if (*z) {
        f = f | 0x80;
      } else {
        f = f & ~0x80;
      }
    }
    if (n != null) {
      if (*n) {
        f = f | 0x40;
      } else {
        f = f & ~0x40;
      }
    }
    if (h != null) {
      if (*h) {
        f = f | 0x20;
      } else {
        f = f & ~0x20;
      }
    }
    if (c != null) {
      if (*c) {
        f = f | 0x10;
      } else {
        f = f & ~0x10;
      }
    }
  }

  auto read_byte_pc() -> dzint {
    auto value = memory.read_byte(pc);
    pc = pc + 1;
    return value;
  }

  auto read_half_pc() -> dzint {
    auto value = memory.read_half(pc);
    pc = pc + 2;
    return value;
  }

  void step() {
    switch (read_byte_pc()) {
      case 0x00:  // NOP
        noop;
        break;
      case 0x01:  // LD BC, imm
        set_bc(read_half_pc());
        break;
      case 0x02:  // LD (BC), A
        memory.write_byte(bc(), a);
        break;
      case 0x03:  // INC BC
        set_bc(bc() + 1);
        break;
      case 0x04:  // INC B
        b = (b + 1) & 0xFF;
        set_f(b == 0, 0, b == 0x10, null);
        break;
      case 0x05:  // DEC B
        b = (b - 1) & 0xFF;
        set_f(b == 0, 1, b == 0x0F, null);
        break;
      case 0x06:  // LD B, imm
        b = read_byte_pc();
        break;
      case 0x07:  // RCLA
        a = ((a << 1) | (a >> 7)) & 0xFF;
        set_f(0, 0, 0, a & 0x1);
        break;
      case 0x08:  // LD (imm), SP
        memory.write_half(read_half_pc(), sp);
        break;
      case 0x09: {  // ADD HL, BC
        auto res = hl() + bc();
        set_f(null, 0, (res ^  hl() ^ bc()) & 0x1000, res > 0x10000);
        set_hl(res);
        break;
      }
      case 0x0A:  // LD A, (BC)
        a = memory.read_half(bc());
        break;
      case 0x0B:  // DEC BC
        set_bc(bc() - 1);
        break;
      case 0x0C:  // INC C
        c = (c + 1) & 0xFF;
        set_f(c == 0, 0, c == 0x10, null);
        break;
      case 0x0D:  // DEC C
        c = (c - 1) & 0xFF;
        set_f(c == 0, 1, c == 0x0F, null);
        break;
      case 0x0E:  // LD C, imm
        c = read_byte_pc();
        break;
      case 0x0F:  // RRCA
        set_f(0, 0, 0, a & 0x1);
        a = ((a >> 1) | (a << 7)) & 0xFF;
        break;
      case 0x10:  // STOP
        noop;  // Todo: implement
        break;
      case 0x11:  // LD DE, imm
        set_de(read_half_pc());
        break;
      case 0x12:  // LD (DE), A
        memory.write_byte(de(), a);
        break;
      case 0x13:  // INC DE
        set_bc(de() + 1);
        break;
      case 0x14:  // INC D
        d = (d + 1) & 0xFF;
        set_f(d == 0, 0, d == 0x10, null);
        break;
      case 0x15:  // DEC D
        d = (d - 1) & 0xFF;
        set_f(d == 0, 1, d == 0x0F, null);
        break;
      case 0x16:  // LD D, imm
        d = read_byte_pc();
        break;
      case 0x17:  // RLA
        a = (a << 7) | fc();
        set_f(0, 0, 0, a >> 8);
        a = a & 0xFF;
        break;
      case 0x18:  // JR imm
        pc = read_byte_pc() + pc;
        break;
      case 0x19: {  // ADD HL, DE
        auto res = hl() + de();
        set_f(null, 0, (res ^  hl() ^ de()) & 0x1000, res > 0x10000);
        set_de(res);
        break;
      }
      case 0x1A:  // LD A, (DE)
        a = memory.read_half(de());
        break;
      case 0x1B:  // DEC DE
        set_bc(de() - 1);
        break;
      case 0x1C:  // INC E
        e = (e + 1) & 0xFF;
        set_f(e == 0, 0, e == 0x10, null);
        break;
      case 0x1D:  // DEC E
        e = (e - 1) & 0xFF;
        set_f(e == 0, 1, e == 0x0F, null);
        break;
      case 0x1E:  // LD E, imm
        e = read_byte_pc();
        break;
      case 0x1F:  // RRA
        a = a | (fc() << 8);
        set_f(0, 0, 0, a & 0x1);
        a = a >> 1;
        break;
    }
  }
};

struct GameBoy {
  Cpu cpu;
};

int main(int argc, char* argv[]) {
  GameBoy gb;
  return 0;
}
