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
        auto value = hl() + bc();
        set_f(null, 0, (value ^  hl() ^ bc()) & 0x1000, value > 0x10000);
        set_hl(value);
        break;
      }
      case 0x0A:  // LD A, (BC)
        a = memory.read_byte(bc());
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
        break;
      case 0x11:  // LD DE, imm
        set_de(read_half_pc());
        break;
      case 0x12:  // LD (DE), A
        memory.write_byte(de(), a);
        break;
      case 0x13:  // INC DE
        set_de(de() + 1);
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
        auto value = hl() + de();
        set_f(null, 0, (value ^  hl() ^ de()) & 0x1000, value > 0x10000);
        set_de(value);
        break;
      }
      case 0x1A:  // LD A, (DE)
        a = memory.read_byte(de());
        break;
      case 0x1B:  // DEC DE
        set_de(de() - 1);
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
      case 0x20:  // JR NZ, imm
        if (!fz()) {
          pc = read_byte_pc() + pc;
        } else {
          pc = pc + 1;
        }
        break;
      case 0x21:  // LD HL, imm
        set_hl(read_half_pc());
        break;
      case 0x22:  // LD (HL+), A
        memory.write_byte(hl(), a);
        set_hl(hl() + 1);
        break;
      case 0x23:  // INC HL
        set_hl(hl() + 1);
        break;
      case 0x24:  // INC H
        h = (h + 1) & 0xFF;
        set_f(h == 0, 0, h == 0x10, null);
        break;
      case 0x25:  // DEC H
        h = (h - 1) & 0xFF;
        set_f(h == 0, 1, h == 0x0F, null);
        break;
      case 0x26:  // LD H, imm
        h = read_byte_pc();
        break;
      case 0x27:  // DAA
        // Todo: implement https://ehaskins.com/2018-01-30%20Z80%20DAA/
        break;
      case 0x28:  // JR Z, imm
        if (fz()) {
          pc = read_byte_pc() + pc;
        } else {
          pc = pc + 1;
        }
        break;
      case 0x29: {  // ADD HL, HL
        auto value = hl() + hl();
        set_f(null, 0, value & 0x1000, value > 0x10000);
        set_hl(value);
        break;
      }
      case 0x2A:  // LD A, (HL+)
        a = memory.read_byte(hl());
        set_hl(hl() + 1);
        break;
      case 0x2B:  // DEC HL
        set_hl(hl() - 1);
        break;
      case 0x2C:  // INC L
        l = (l + 1) & 0xFF;
        set_f(l == 0, 0, l == 0x10, null);
        break;
      case 0x2D:  // DEC L
        l = (l - 1) & 0xFF;
        set_f(l == 0, 1, l == 0x0F, null);
        break;
      case 0x2E:  // LD L, imm
        l = read_byte_pc();
        break;
      case 0x2F:  // CPL
        a = ~a;
        set_f(null, 1, 1, null);
        break;
      case 0x30:  // JR NC, imm
        if (!fc()) {
          pc = read_byte_pc() + pc;
        } else {
          pc = pc + 1;
        }
        break;
      case 0x31:  // LD SP, imm
        sp = read_half_pc();
        break;
      case 0x32:  // LD (HL-), A
        memory.write_byte(hl(), a);
        set_hl(hl() - 1);
        break;
      case 0x33:  // INC SP
        sp = (sp + 1) & 0xFFFF;
        break;
      case 0x34: {  // INC (HL)
        auto value = (memory.read_byte(hl()) + 1) & 0xFF;
        set_f(value == 0, 0, value == 0x10, null);
        memory.write_byte(hl(), value);
        break;
      }
      case 0x35: {  // DEC (HL)
        auto value = (memory.read_byte(hl()) + 1) & 0xFF;
        set_f(value == 0, 1, value == 0x0F, null);
        memory.write_byte(hl(), value);
        break;
      }
      case 0x36:  // LD (HL), imm
        memory.write_byte(hl(), read_byte_pc());
        break;
      case 0x37:  // SCF
        set_f(null, 0, 0, 1);
        break;
      case 0x38:  // JR C, imm
        if (fc()) {
          pc = read_byte_pc() + pc;
        } else {
          pc = pc + 1;
        }
        break;
      case 0x39: {  // ADD HL, SP
        auto value = hl() + sp;
        set_f(null, 0, (value ^ hl() ^ sp) & 0x1000, value > 0x10000);
        set_hl(value);
        break;
      }
      case 0x3A:  // LD A, (HL-)
        a = memory.read_byte(hl());
        set_hl(hl() - 1);
        break;
      case 0x3B:  // DEC SP
        sp = (sp - 1) & 0xFF;
        break;
      case 0x3C:  // INC A
        a = (a + 1) & 0xFF;
        set_f(a == 0, 0, a == 0x10, null);
        break;
      case 0x3D:  // DEC A
        a = (a - 1) & 0xFF;
        set_f(a == 0, 1, a == 0x0F, null);
        break;
      case 0x3E:  // LD A, imm
        a = read_byte_pc();
        break;
      case 0x3F:  // CCF
        set_f(null, 0, 0, !fc());
        break;
      case 0x40:  // LD B, B
        break;
      case 0x41:  // LD B, C
        b = c;
        break;
      case 0x42:  // LD B, D
        b = d;
        break;
      case 0x43:  // LD B, E
        b = e;
        break;
      case 0x44:  // LD B, H
        b = h;
        break;
      case 0x45:  // LD B, L
        b = l;
        break;
      case 0x46:  // LD B, (HL)
        b = memory.read_byte(hl());
        break;
      case 0x47:  // LD B, A
        b = a;
        break;
      case 0x48:  // LD C, B
        c = b;
        break;
      case 0x49:  // LD C, C
        break;
      case 0x4A:  // LD C, D
        c = d;
        break;
      case 0x4B:  // LD C, E
        c = e;
        break;
      case 0x4C:  // LD C, H
        c = h;
        break;
      case 0x4D:  // LD C, L
        c = l;
        break;
      case 0x4E:  // LD C, (HL)
        c = memory.read_byte(hl());
        break;
      case 0x4F:  // LD C, A
        c = a;
        break;
      case 0x50:  // LD D, B
        d = b;
        break;
      case 0x51:  // LD D, C
        d = c;
        break;
      case 0x52:  // LD D, D
        break;
      case 0x53:  // LD D, E
        d = e;
        break;
      case 0x54:  // LD D, H
        d = h;
        break;
      case 0x55:  // LD D, L
        d = l;
        break;
      case 0x56:  // LD D, (HL)
        d = memory.read_byte(hl());
        break;
      case 0x57:  // LD D, A
        d = a;
        break;
      case 0x58:  // LD E, B
        e = b;
        break;
      case 0x59:  // LD E, C
        e = c;
        break;
      case 0x5A:  // LD E, D
        e = d;
        break;
      case 0x5B:  // LD E, E
        break;
      case 0x5C:  // LD E, H
        e = h;
        break;
      case 0x5D:  // LD E, L
        e = l;
        break;
      case 0x5E:  // LD E, (HL)
        e = memory.read_byte(hl());
        break;
      case 0x5F:  // LD E, A
        e = a;
        break;
      case 0x60:  // LD H, B
        h = b;
        break;
      case 0x61:  // LD H, C
        h = c;
        break;
      case 0x62:  // LD H, D
        h = d;
        break;
      case 0x63:  // LD H, E
        h = e;
        break;
      case 0x64:  // LD H, H
        break;
      case 0x65:  // LD H, L
        h = l;
        break;
      case 0x66:  // LD H, (HL)
        h = memory.read_byte(hl());
        break;
      case 0x67:  // LD H, A
        h = a;
        break;
      case 0x68:  // LD L, B
        l = b;
        break;
      case 0x69:  // LD L, C
        l = c;
        break;
      case 0x6A:  // LD L, D
        l = d;
        break;
      case 0x6B:  // LD L, E
        l = e;
        break;
      case 0x6C:  // LD L, H
        l = h;
        break;
      case 0x6D:  // LD L, L
        break;
      case 0x6E:  // LD L, (HL)
        l = memory.read_byte(hl());
        break;
      case 0x6F:  // LD L, A
        l = a;
        break;
      case 0x70:  // LD (HL), B
        memory.write_byte(hl(), b);
        break;
      case 0x71:  // LD (HL), C
        memory.write_byte(hl(), c);
        break;
      case 0x72:  // LD (HL), D
        memory.write_byte(hl(), d);
        break;
      case 0x73:  // LD (HL), E
        memory.write_byte(hl(), e);
        break;
      case 0x74:  // LD (HL), H
        memory.write_byte(hl(), h);
        break;
      case 0x75:  // LD (HL), L
        memory.write_byte(hl(), l);
        break;
      case 0x76:  // HALT
        // Todo: implement
        break;
      case 0x77:  // LD (HL), A
        memory.write_byte(hl(), a);
        break;
      case 0x78:  // LD A, B
        a = b;
        break;
      case 0x79:  // LD A, C
        a = c;
        break;
      case 0x7A:  // LD A, D
        a = d;
        break;
      case 0x7B:  // LD A, E
        a = e;
        break;
      case 0x7C:  // LD A, H
        a = h;
        break;
      case 0x7D:  // LD A, L
        a = l;
        break;
      case 0x7E:  // LD A, (HL)
        a = memory.read_byte(hl());
        break;
      case 0x7F:  // LD A, A
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
