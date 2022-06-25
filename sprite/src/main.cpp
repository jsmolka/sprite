#include <cstdint>
#include <filesystem>
#include <fstream>
#include <iostream>
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
    rom.resize(0x8000, 0);
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
      return rom[addr];  // Todo: implement, see https://gbdev.io/pandocs/The_Cartridge_Header.html#the-cartridge-header
    } else {
      return rom[addr];
    }
  }

  auto read_half(dzint addr) const -> dzint {
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
      if ((addr - 0xFF00) == 0x01) {
        std::printf("%c", (char)byte);
      }
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
    write_byte(addr, half & 0xFF);
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
  dzint a = 0x01;
  dzint f = 0xB0;
  dzint b = 0x00;
  dzint c = 0x13;
  dzint d = 0x00;
  dzint e = 0xD8;
  dzint h = 0x01;
  dzint l = 0x4D;

  dzint pc = 0x0100;
  dzint sp = 0xFFFE;
  dzbool halt = false;
  dzbool ime = true;
  Memory memory;

  auto af() const -> dzint {
    return f | (a << 8);
  }

  void set_af(dzint value) {
    f = value & 0xF0;
    a = (value >> 8) & 0xFF;
  }

  auto bc() const -> dzint {
    return c | (b << 8);
  }

  void set_bc(dzint value) {
    c = value & 0xFF;
    b = (value >> 8) & 0xFF;
  }

  auto de() const -> dzint {
    return e | (d << 8);
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
    pc = (pc + 1) & 0xFFFF;
    return value;
  }

  auto read_signed_byte_pc() -> dzint {
    return read_byte_pc() << 56 >> 56;
  }

  auto read_half_pc() -> dzint {
    auto value = memory.read_half(pc);
    pc = (pc + 2) & 0xFFFF;
    return value;
  }

  auto inc(dzint value) -> dzint {
    value = (value + 1) & 0xFF;
    set_f(value == 0, 0, (value & 0x0F) == 0x00, null);
    return value;
  }

  auto dec(dzint value) -> dzint {
    value = (value - 1) & 0xFF;
    set_f(value == 0, 1, (value & 0x0F) == 0x0F, null);
    return value;
  }

  void add(dzint b) {
    auto value = a + b;
    set_f((value & 0xFF) == 0, 0, (a ^ b ^ value) & 0x10, value & 0xFF00);
    a = value & 0xFF;
  }

  // Todo: this is always used this HL()
  auto add_half(dzint a, dzint b) -> dzint {
    auto value = a + b;
    set_f(null, 0, (a ^ b ^ value) & 0x1000, value & 0xFFFF0000);
    return value;
  }

  void adc(dzint b) {
    auto value = a + b + fc();
    set_f((value & 0xFF) == 0, 0, (a ^ b ^ value) & 0x10, value & 0xFF00);
    a = value & 0xFF;
  }

  void sub(dzint b) {
    auto value = a - b;
    set_f((value & 0xFF) == 0, 1, (a ^ b ^ value) & 0x10, value & 0xFF00);
    a = value & 0xFF;
  }

  void sbc(dzint b) {
    auto value = a - b - fc();
    set_f((value & 0xFF) == 0, 1, (a ^ b ^ value) & 0x10, value & 0xFF00);
    a = value & 0xFF;
  }

  void cp(dzint b) {
    auto value = a - b;
    set_f((value & 0xFF) == 0, 1, (a ^ b ^ value) & 0x10, value & 0xFF00);
  }

  void and_(dzint b) {
    a = a & b;
    set_f(a == 0, 0, 1, 0);
  }

  void xor_(dzint b) {
    a = a ^ b;
    set_f(a == 0, 0, 0, 0);
  }

  void or_(dzint b) {
    a = a | b;
    set_f(a == 0, 0, 0, 0);
  }

  void jp(dzbool condition) {
    if (condition) {
      pc = read_half_pc();
    } else {
      pc = (pc + 2) & 0xFFFF;
    }
  }

  void jr(dzbool condition) {
    dzint offset = 1;
    if (condition) {
      offset = read_signed_byte_pc();
    }
    pc = (pc + offset) & 0xFFFF;
  }

  void push(dzint half) {
    sp = (sp - 2) & 0xFFFF;
    memory.write_half(sp, half);
  }

  auto pop() -> dzint {
    auto value = memory.read_half(sp);
    sp = (sp + 2) & 0xFFFF;
    return value;
  }

  void call(dzbool condition) {
    if (condition) {
      auto addr = read_half_pc();
      push(pc);
      pc = addr;
    } else {
      pc = (pc + 2) & 0xFFFF;
    }
  }

  void ret(dzbool condition) {
    if (condition) {
      pc = memory.read_half(sp);
      sp = (sp + 2) & 0xFFFF;
    }
  }

  void rst(dzint addr) {
    push(pc);
    pc = addr;
  }

  void step() {
    switch (read_byte_pc()) {
      case 0x00:  // NOP
        break;
      case 0x01:  // LD BC, u16
        c = read_byte_pc();
        b = read_byte_pc();
        break;
      case 0x02:  // LD (BC), A
        memory.write_byte(bc(), a);
        break;
      case 0x03:  // INC BC
        set_bc(bc() + 1);
        break;
      case 0x04:  // INC B
        b = inc(b);
        break;
      case 0x05:  // DEC B
        b = dec(b);
        break;
      case 0x06:  // LD B, u8
        b = read_byte_pc();
        break;
      case 0x07:  // RLCA
        a = ((a << 1) | (a >> 7)) & 0xFF;
        set_f(0, 0, 0, a & 0x1);
        break;
      case 0x08:  // LD (u16), SP
        memory.write_half(read_half_pc(), sp);
        break;
      case 0x09:  // ADD HL, BC
        set_hl(add_half(hl(), bc()));
        break;
      case 0x0A:  // LD A, (BC)
        a = memory.read_byte(bc());
        break;
      case 0x0B:  // DEC BC
        set_bc(bc() - 1);
        break;
      case 0x0C:  // INC C
        c = inc(c);
        break;
      case 0x0D:  // DEC C
        c = dec(c);
        break;
      case 0x0E:  // LD C, u8
        c = read_byte_pc();
        break;
      case 0x0F:  // RRCA
        set_f(0, 0, 0, a & 0x1);
        a = ((a >> 1) | (a << 7)) & 0xFF;
        break;
      case 0x10:  // STOP
        break;
      case 0x11:  // LD DE, u16
        e = read_byte_pc();
        d = read_byte_pc();
        break;
      case 0x12:  // LD (DE), A
        memory.write_byte(de(), a);
        break;
      case 0x13:  // INC DE
        set_de(de() + 1);
        break;
      case 0x14:  // INC D
        d = inc(d);
        break;
      case 0x15:  // DEC D
        d = dec(d);
        break;
      case 0x16:  // LD D, u8
        d = read_byte_pc();
        break;
      case 0x17:  // RLA
        a = (a << 1) | fc();
        set_f(0, 0, 0, a >> 8);
        a = a & 0xFF;
        break;
      case 0x18:  // JR s8
        jr(true);
        break;
      case 0x19:  // ADD HL, DE
        set_hl(add_half(hl(), de()));
        break;
      case 0x1A:  // LD A, (DE)
        a = memory.read_byte(de());
        break;
      case 0x1B:  // DEC DE
        set_de(de() - 1);
        break;
      case 0x1C:  // INC E
        e = inc(e);
        break;
      case 0x1D:  // DEC E
        e = dec(e);
        break;
      case 0x1E:  // LD E, u8
        e = read_byte_pc();
        break;
      case 0x1F:  // RRA
        a = a | (fc() << 8);
        set_f(0, 0, 0, a & 0x1);
        a = a >> 1;
        break;
      case 0x20:  // JR NZ, s8
        jr(!fz());
        break;
      case 0x21:  // LD HL, u16
        l = read_byte_pc();
        h = read_byte_pc();
        break;
      case 0x22:  // LD (HL+), A
        memory.write_byte(hl(), a);
        set_hl(hl() + 1);
        break;
      case 0x23:  // INC HL
        set_hl(hl() + 1);
        break;
      case 0x24:  // INC H
        h = inc(h);
        break;
      case 0x25:  // DEC H
        h = dec(h);
        break;
      case 0x26:  // LD H, u8
        h = read_byte_pc();
        break;
      case 0x27: {  // DAA
        // Todo: this is probably wrong because of signedness
        auto value = a;
        if (fn()) {
          if (fh()) {
            value = (value - 0x06) & 0xFF;
          }
          if (fc()) {
            value = (value - 0x60) & 0xFFFF;
          }
        } else {
          if (fh() || (value & 0x0F) > 9) {
            value = (value + 0x06) & 0xFFFF;
          }
          if (fc() || value > 0x9F) {
            value = (value + 0x60) & 0xFFFF;
          }
        }

        if ((value & 0x100) == 0x100) {
          set_f(null, null, null, 1);
        }

        a = value & 0xFF;
        set_f((value & 0xFF) == 0, null, 0, null);
        break;
      }
      case 0x28:  // JR Z, s8
        jr(fz());
        break;
      case 0x29:  // ADD HL, HL
        set_hl(add_half(hl(), hl()));
        break;
      case 0x2A:  // LD A, (HL+)
        a = memory.read_byte(hl());
        set_hl(hl() + 1);
        break;
      case 0x2B:  // DEC HL
        set_hl(hl() - 1);
        break;
      case 0x2C:  // INC L
        l = inc(l);
        break;
      case 0x2D:  // DEC L
        l = dec(l);
        break;
      case 0x2E:  // LD L, u8
        l = read_byte_pc();
        break;
      case 0x2F:  // CPL
        a = ~a;
        set_f(null, 1, 1, null);
        break;
      case 0x30:  // JR NC, s8
        jr(!fc());
        break;
      case 0x31:  // LD SP, u16
        sp = read_half_pc();
        break;
      case 0x32:  // LD (HL-), A
        memory.write_byte(hl(), a);
        set_hl(hl() - 1);
        break;
      case 0x33:  // INC SP
        sp = (sp + 1) & 0xFFFF;
        break;
      case 0x34:  // INC (HL)
        memory.write_byte(hl(), inc(memory.read_byte(hl())));
        break;
      case 0x35:  // DEC (HL)
        memory.write_byte(hl(), dec(memory.read_byte(hl())));
        break;
      case 0x36:  // LD (HL), u8
        memory.write_byte(hl(), read_byte_pc());
        break;
      case 0x37:  // SCF
        set_f(null, 0, 0, 1);
        break;
      case 0x38:  // JR C, s8
        jr(fc());
        break;
      case 0x39:  // ADD HL, SP
        set_hl(add_half(hl(), sp));
        break;
      case 0x3A:  // LD A, (HL-)
        a = memory.read_byte(hl());
        set_hl(hl() - 1);
        break;
      case 0x3B:  // DEC SP
        sp = (sp - 1) & 0xFFFF;
        break;
      case 0x3C:  // INC A
        a = inc(a);
        break;
      case 0x3D:  // DEC A
        a = dec(a);
        break;
      case 0x3E:  // LD A, u8
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
        halt = true;
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
      case 0x80:  // ADD A, B
        add(b);
        break;
      case 0x81:  // ADD A, C
        add(c);
        break;
      case 0x82:  // ADD A, D
        add(d);
        break;
      case 0x83:  // ADD A, E
        add(e);
        break;
      case 0x84:  // ADD A, H
        add(h);
        break;
      case 0x85:  // ADD A, L
        add(l);
        break;
      case 0x86:  // ADD A, (HL)
        add(memory.read_byte(hl()));
        break;
      case 0x87:  // ADD A, A
        add(a);
        break;
      case 0x88:  // ADC A, B
        adc(b);
        break;
      case 0x89:  // ADC A, C
        adc(c);
        break;
      case 0x8A:  // ADC A, D
        adc(d);
        break;
      case 0x8B:  // ADC A, E
        adc(e);
        break;
      case 0x8C:  // ADC A, H
        adc(h);
        break;
      case 0x8D:  // ADC A, L
        adc(l);
        break;
      case 0x8E:  // ADC A, (HL)
        adc(memory.read_byte(hl()));
        break;
      case 0x8F:  // ADC A, A
        adc(a);
        break;
      case 0x90:  // SUB A, B
        sub(b);
        break;
      case 0x91:  // SUB A, C
        sub(c);
        break;
      case 0x92:  // SUB A, D
        sub(d);
        break;
      case 0x93:  // SUB A, E
        sub(e);
        break;
      case 0x94:  // SUB A, H
        sub(h);
        break;
      case 0x95:  // SUB A, L
        sub(l);
        break;
      case 0x96:  // SUB A, (HL)
        sub(memory.read_byte(hl()));
        break;
      case 0x97:  // SUB A, A
        sub(a);
        break;
      case 0x98:  // SBC A, B
        sbc(b);
        break;
      case 0x99:  // SBC A, C
        sbc(c);
        break;
      case 0x9A:  // SBC A, D
        sbc(d);
        break;
      case 0x9B:  // SBC A, E
        sbc(e);
        break;
      case 0x9C:  // SBC A, H
        sbc(h);
        break;
      case 0x9D:  // SBC A, L
        sbc(l);
        break;
      case 0x9E:  // SBC A, (HL)
        sbc(memory.read_byte(hl()));
        break;
      case 0x9F:  // SBC A, A
        sbc(a);
        break;
      case 0xA0:  // AND A, B
        and_(b);
        break;
      case 0xA1:  // AND A, C
        and_(c);
        break;
      case 0xA2:  // AND A, D
        and_(d);
        break;
      case 0xA3:  // AND A, E
        and_(e);
        break;
      case 0xA4:  // AND A, H
        and_(h);
        break;
      case 0xA5:  // AND A, L
        and_(l);
        break;
      case 0xA6:  // AND A, (HL)
        and_(memory.read_byte(hl()));
        break;
      case 0xA7:  // AND A, A
        and_(a);
        break;
      case 0xA8:  // XOR A, B
        xor_(b);
        break;
      case 0xA9:  // XOR A, C
        xor_(c);
        break;
      case 0xAA:  // XOR A, D
        xor_(d);
        break;
      case 0xAB:  // XOR A, E
        xor_(e);
        break;
      case 0xAC:  // XOR A, H
        xor_(h);
        break;
      case 0xAD:  // XOR A, L
        xor_(l);
        break;
      case 0xAE:  // XOR A, (HL)
        xor_(memory.read_byte(hl()));
        break;
      case 0xAF:  // XOR A, A
        xor_(a);
        break;
      case 0xB0:  // OR A, B
        or_(b);
        break;
      case 0xB1:  // OR A, C
        or_(c);
        break;
      case 0xB2:  // OR A, D
        or_(d);
        break;
      case 0xB3:  // OR A, E
        or_(e);
        break;
      case 0xB4:  // OR A, H
        or_(h);
        break;
      case 0xB5:  // OR A, L
        or_(l);
        break;
      case 0xB6:  // OR A, (HL)
        or_(memory.read_byte(hl()));
        break;
      case 0xB7:  // OR A, A
        or_(a);
        break;
      case 0xB8:  // CP A, B
        cp(b);
        break;
      case 0xB9:  // CP A, C
        cp(c);
        break;
      case 0xBA:  // CP A, D
        cp(d);
        break;
      case 0xBB:  // CP A, E
        cp(e);
        break;
      case 0xBC:  // CP A, H
        cp(h);
        break;
      case 0xBD:  // CP A, L
        cp(l);
        break;
      case 0xBE:  // CP A, (HL)
        cp(memory.read_byte(hl()));
        break;
      case 0xBF:  // CP A, A
        cp(a);
        break;
      case 0xC0:  // RET NZ
        ret(!fz());
        break;
      case 0xC1:  // POP BC
        set_bc(pop());
        break;
      case 0xC2:  // JP NZ, u16
        jp(!fz());
        break;
      case 0xC3:  // JP u16
        jp(true);
        break;
      case 0xC4:  // CALL NZ, u16
        call(!fz());
        break;
      case 0xC5:  // PUSH BC
        push(bc());
        break;
      case 0xC6:  // ADD A, u8
        add(read_byte_pc());
        break;
      case 0xC7:  // RST 0x00
        rst(0x00);
        break;
      case 0xC8:  // RET Z
        ret(fz());
        break;
      case 0xC9:  // RET
        ret(true);
        break;
      case 0xCA:  // JP Z, u16
        jp(fz());
        break;
      case 0xCB:  // PREFIX CB
        prefix();
        break;
      case 0xCC:  // CALL Z, u16
        call(fz());
        break;
      case 0xCD:  // CALL u16
        call(true);
        break;
      case 0xCE:  // ADC A, u8
        adc(read_byte_pc());
        break;
      case 0xCF:  // RST 0x08
        rst(0x08);
        break;
      case 0xD0:  // RET NC
        ret(!fc());
        break;
      case 0xD1:  // POP DE
        set_de(pop());
        break;
      case 0xD2:  // JP NC, u16
        jp(!fc());
        break;
      case 0xD4:  // CALL NC, u16
        call(!fc());
        break;
      case 0xD5:  // PUSH DE
        push(de());
        break;
      case 0xD6:  // SUB A, u8
        sub(read_byte_pc());
        break;
      case 0xD7:  // RST 0x10
        rst(0x10);
        break;
      case 0xD8:  // RET C
        ret(fc());
        break;
      case 0xD9:  // RETI
        pc = pop();
        ime = true;
        break;
      case 0xDA:  // JP C, u16
        jp(fc());
        break;
      case 0xDC:  // CALL C, u16
        call(fc());
        break;
      case 0xDE:  // SBC A, u8
        sbc(read_byte_pc());
        break;
      case 0xDF:  // RST 0x18
        rst(0x18);
        break;
      case 0xE0:  // LDH (u8), A
        memory.write_byte(0xFF00 | read_byte_pc(), a);
        break;
      case 0xE1:  // POP HL
        set_hl(pop());
        break;
      case 0xE2:  // LDH (C), A
        memory.write_byte(0xFF00 | c, a);
        break;
      case 0xE5:  // PUSH HL
        push(hl());
        break;
      case 0xE6:  // AND A, u8
        and_(read_byte_pc());
        break;
      case 0xE7:  // RST 0x20
        rst(0x20);
        break;
      case 0xE8: {  // ADD SP, s8
        auto sbyte = read_signed_byte_pc();
        auto value = sp + sbyte;
        set_f(0, 0, (sp & 0xF) + (sbyte & 0xF) > 0xF, (sp & 0xFF) + (sbyte & 0xFF) > 0xFF);
        sp = value & 0xFFFF;
        break;
      }
      case 0xE9:  // JP (HL)
        pc = hl();
        break;
      case 0xEA:  // LD (u16), A
        memory.write_byte(read_half_pc(), a);
        break;
      case 0xEE:  // XOR A, u8
        xor_(read_byte_pc());
        break;
      case 0xEF:  // RST 0x28
        rst(0x28);
        break;
      case 0xF0:  // LDH A, u8
        a = memory.read_byte(0xFF00 | read_byte_pc());
        break;
      case 0xF1:  // POP AF
        set_af(pop());
        break;
      case 0xF2:  // LDH A, (C)
        a = memory.read_byte(0xFF00 | c);
        break;
      case 0xF3:  // DI
        ime = false;
        break;
      case 0xF5:  // PUSH AF
        push(af());
        break;
      case 0xF6:  // OR A, u8
        or_(read_byte_pc());
        break;
      case 0xF7:  // RST 0x30
        rst(0x30);
        break;
      case 0xF8: {  // LD HL, SP + s8
        auto sbyte = read_signed_byte_pc();
        auto value = sp + sbyte;
        set_f(0, 0, (sp & 0xF) + (sbyte & 0xF) > 0xF, (sp & 0xFF) + (sbyte & 0xFF) > 0xFF);
        set_hl(value);
        break;
      }
      case 0xF9:  // LD SP, HL
        sp = hl();
        break;
      case 0xFA:  // LD A, (u16)
        a = memory.read_byte(read_half_pc());
        break;
      case 0xFB:  // EI
        ime = true;
        break;
      case 0xFE:  // CP A, u8
        cp(read_byte_pc());
        break;
      case 0xFF:  // RST 0x38
        rst(0x38);
        break;
    }
  }

  void prefix() {
    dzint opcode = read_byte_pc();

    dzint operand;
    switch (opcode & 0x7) {
      case 0x0:
        operand = b;
        break;
      case 0x1:
        operand = c;
        break;
      case 0x2:
        operand = d;
        break;
      case 0x3:
        operand = e;
        break;
      case 0x4:
        operand = h;
        break;
      case 0x5:
        operand = l;
        break;
      case 0x6:
        operand = memory.read_byte(hl());
        break;
      default:
        operand = a;
        break;
    }

    dzbool writeback = true;
    if (opcode <= 0x07) {  // RLC operand
      operand = ((operand << 1) | (operand >> 7)) & 0xFF;
      set_f(operand == 0, 0, 0, operand & 0x1);
    } else if (opcode <= 0x0F) {  // RRC operand
      set_f(0, 0, 0, operand & 0x1);
      operand = ((operand >> 1) | (operand << 7)) & 0xFF;
    } else if (opcode <= 0x17) {  // RL operand
      operand = (operand << 1) | fc();
      set_f(0, 0, 0, operand >> 8);
      operand = operand & 0xFF;
    } else if (opcode <= 0x1F) {  // RR operand
      operand = operand | (fc() << 8);
      set_f(0, 0, 0, operand & 0x1);
      operand = operand >> 1;
    } else if (opcode <= 0x27) {  // SLA operand
      auto value = operand;
      operand = (operand << 1) & 0xFF;
      set_f(operand == 0, 0, 0, value >> 7);
    } else if (opcode <= 0x2F) {  // SRA operand
      auto value = operand;
      operand = (operand & 0x80) | (operand >> 1);
      set_f(operand == 0, 0, 0, value & 0x1);
    } else if (opcode <= 0x37) {  // SWAP operand
      operand = ((operand & 0x0F) << 4) | ((operand & 0xF0) >> 4);
      set_f(operand == 0, 0, 0, 0);
    } else if (opcode <= 0x3F) {  // SRL operand
      auto value = operand;
      operand = operand >> 1;
      set_f(operand == 0, 0, 0, value & 0x1);
    } else if (opcode <= 0x7F) {  // BIT n, operand
      auto bit = (opcode - 0x40) >> 3;
      set_f(!(operand & (1LL << bit)), 0, 1, null);
      writeback = false;
    } else if (opcode <= 0xBF) {  // RES n, operand
      auto bit = (opcode - 0x80) >> 3;
      operand = operand & ~(1LL << bit);
    } else if (opcode <= 0xFF) {  // SET n, operand
      auto bit = (opcode - 0xC0) >> 3;
      operand = operand | (1LL << bit);
    }

    if (writeback) {
      switch (opcode & 0x7) {
        case 0x0:
          b = operand;
          break;
        case 0x1:
          c = operand;
          break;
        case 0x2:
          d = operand;
          break;
        case 0x3:
          e = operand;
          break;
        case 0x4:
          h = operand;
          break;
        case 0x5:
          l = operand;
          break;
        case 0x6:
          memory.write_byte(hl(), operand); 
          break;
        default:
          a = operand;
          break;
      }
    }
  }
};

struct GameBoy {
  Cpu cpu;
};

auto read(const std::filesystem::path& file, dzbytes& dst) {
  auto stream = std::ifstream(file, std::ios::binary);
  if (!stream.is_open() || !stream) {
    return false;
  }

  const auto size = std::filesystem::file_size(file);
  dst.resize(size);

  stream.read(reinterpret_cast<char*>(dst.data()), size);
  if (!stream) {
    return false;
  }
  return true;
}

int main(int argc, char* argv[]) {
  GameBoy gb;

  if (argc > 1) {
    if (!read(argv[1], gb.cpu.memory.rom)) {
      std::printf("cannot read '%s'\n", argv[1]);
      return 1;
    }
  }

  while (true) {
    gb.cpu.step();
  }

  return 0;
}
