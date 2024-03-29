#include <iostream>

#include "dz.h"

static constexpr dzint kCycles[] = {
  0x04, 0x0C, 0x08, 0x08, 0x04, 0x04, 0x08, 0x04, 0x14, 0x08, 0x08, 0x08, 0x04, 0x04, 0x08, 0x04,
  0x04, 0x0C, 0x08, 0x08, 0x04, 0x04, 0x08, 0x04, 0x0C, 0x08, 0x08, 0x08, 0x04, 0x04, 0x08, 0x04,
  0x08, 0x0C, 0x08, 0x08, 0x04, 0x04, 0x08, 0x04, 0x08, 0x08, 0x08, 0x08, 0x04, 0x04, 0x08, 0x04,
  0x08, 0x0C, 0x08, 0x08, 0x0C, 0x0C, 0x0C, 0x04, 0x08, 0x08, 0x08, 0x08, 0x04, 0x04, 0x08, 0x04,
  0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x08, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x08, 0x04,
  0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x08, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x08, 0x04,
  0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x08, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x08, 0x04,
  0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x04, 0x08, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x08, 0x04,
  0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x08, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x08, 0x04,
  0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x08, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x08, 0x04,
  0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x08, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x08, 0x04,
  0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x08, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x08, 0x04,
  0x08, 0x0C, 0x0C, 0x10, 0x0C, 0x10, 0x08, 0x10, 0x08, 0x10, 0x0C, 0x08, 0x0C, 0x18, 0x08, 0x10,
  0x08, 0x0C, 0x0C, 0x00, 0x0C, 0x10, 0x08, 0x10, 0x08, 0x10, 0x0C, 0x00, 0x0C, 0x00, 0x08, 0x10,
  0x0C, 0x0C, 0x08, 0x00, 0x00, 0x10, 0x08, 0x10, 0x10, 0x04, 0x10, 0x00, 0x00, 0x00, 0x08, 0x10,
  0x0C, 0x0C, 0x08, 0x04, 0x00, 0x10, 0x08, 0x10, 0x0C, 0x08, 0x10, 0x04, 0x00, 0x00, 0x08, 0x10
};

static constexpr dzint kPalette[] = { 0xFFC6DE8C, 0xFF84A563, 0xFF396139, 0xFF081810 };

inline constexpr dzint kScreenW = 160;
inline constexpr dzint kScreenH = 144;

inline constexpr dzint kStatLine   = 1 << 6;
inline constexpr dzint kStatOam    = 1 << 5;
inline constexpr dzint kStatVBlank = 1 << 4;
inline constexpr dzint kStatHBlank = 1 << 3;

inline constexpr dzint kInterruptVBlank = 1 << 0;
inline constexpr dzint kInterruptStat   = 1 << 1;
inline constexpr dzint kInterruptTimer  = 1 << 2;
inline constexpr dzint kInterruptSerial = 1 << 3;
inline constexpr dzint kInterruptJoypad = 1 << 4;

inline constexpr dzint kModeHBlank = 0;
inline constexpr dzint kModeVBlank = 1;
inline constexpr dzint kModeOam    = 2;
inline constexpr dzint kModeVram   = 3;

auto min(dzint a, dzint b) -> dzint {
  if (a < b) {
    return a;
  } else {
    return b;
  }
}

auto max(dzint a, dzint b) -> dzint {
  if (a > b) {
    return a;
  } else {
    return b;
  }
}

auto sign_extend(dzint value) -> dzint {
  return (value << 56) >> 56;
}

auto color(dzint palette, dzint index) -> dzint {
  return kPalette[(palette >> (2 * index)) & 0x3];
}

class GameBoy {
public:
  GameBoy() {
    vram.resize(0x2000, 0);
    wram.resize(0x2000, 0);
    oram.resize(0x0100, 0);
    hram.resize(0x007F, 0);
  }

  ~GameBoy() {
    delete window;
  }

  dzlist<dzbool> transparent;
  dz::SdlWindow* window = dz::sdl_window("sprite", kScreenW, kScreenH, 2);

  dzint a = 0x01;
  dzint f = 0xB0;
  dzint b = 0x00;
  dzint c = 0x13;
  dzint d = 0x00;
  dzint e = 0xD8;
  dzint h = 0x01;
  dzint l = 0x4D;

  dzint pc     = 0x0100;
  dzint sp     = 0xFFFE;
  dzint halt   = 0;
  dzint ie     = 0;
  dzint ime    = 1;
  dzint cycles = 0;

  dzint mbc        = 0;
  dzint mbc_mode   = 0;
  dzint rom_bank   = 1;
  dzint rom_banks  = 0;
  dzint ram_exists = 0;
  dzint ram_enable = 0;
  dzint ram_bank   = 0;
  dzint ram_banks  = 0;

  dzbytes rom;
  dzbytes vram;
  dzbytes eram;
  dzbytes wram;
  dzbytes oram;
  dzbytes hram;

  dzint joyp        = 0xCF;
  dzint sb          = 0x00;
  dzint sc          = 0x7E;
  dzint div         = 0xAC;
  dzint div_cycles  = 0x00;
  dzint tima        = 0x00;
  dzint tima_cycles = 0x00;
  dzint tma         = 0x00;
  dzint tac         = 0xF8;
  dzint if_         = 0xE1;
  dzint nr10        = 0x80;
  dzint nr11        = 0xBF;
  dzint nr12        = 0xF3;
  dzint nr14        = 0xBF;
  dzint nr21        = 0x3F;
  dzint nr22        = 0x00;
  dzint nr24        = 0xBF;
  dzint nr30        = 0x7F;
  dzint nr31        = 0xFF;
  dzint nr32        = 0x9F;
  dzint nr34        = 0xBF;
  dzint nr41        = 0xFF;
  dzint nr42        = 0x00;
  dzint nr43        = 0x00;
  dzint nr44        = 0xBF;
  dzint nr50        = 0x77;
  dzint nr51        = 0xF3;
  dzint nr52        = 0xF1;
  dzint lcdc        = 0x91;
  dzint stat        = 0x80;
  dzint ppu_mode    = 0x00;
  dzint ppu_cycles  = 0x00;
  dzint scx         = 0x00;
  dzint scy         = 0x00;
  dzint ly          = 0x00;
  dzint lyc         = 0x00;
  dzint dma         = 0x00;
  dzint bgp         = 0xCF;
  dzint obp0        = 0xFF;
  dzint obp1        = 0xFF;
  dzint wx          = 0x00;
  dzint wy          = 0x00;

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

  auto lcd_enabled() const -> dzbool {
    return lcdc & 0x80;
  }

  auto read_byte_io(dzint addr) const -> dzint {
    switch (addr) {
      case 0x00: {
        dzint value = joyp & 0b00110000;
        if ((value & 0x10) == 0) {
          value = value | (dzint(dz::sdl_keystate(SDL_SCANCODE_D)) << 0);
          value = value | (dzint(dz::sdl_keystate(SDL_SCANCODE_A)) << 1);
          value = value | (dzint(dz::sdl_keystate(SDL_SCANCODE_W)) << 2);
          value = value | (dzint(dz::sdl_keystate(SDL_SCANCODE_S)) << 3);
        } else if ((value & 0x20) == 0) {
          value = value | (dzint(dz::sdl_keystate(SDL_SCANCODE_U)) << 0);
          value = value | (dzint(dz::sdl_keystate(SDL_SCANCODE_H)) << 1);
          value = value | (dzint(dz::sdl_keystate(SDL_SCANCODE_F)) << 2);
          value = value | (dzint(dz::sdl_keystate(SDL_SCANCODE_G)) << 3);
        }
        return (value ^ 0b00001111) | 0b11000000;
      }
      case 0x01:
        return sb;
      case 0x02:
        return sc | 0b01111110;
      case 0x04:
        return div;
      case 0x05:
        return tima;
      case 0x06:
        return tma;
      case 0x07:
        return tac | 0b11111000;
      case 0x0F:
        return if_ | 0b11100000;
      case 0x10:
        return nr10 | 0b10000000;
      case 0x11:
        return nr11;
      case 0x12:
        return nr12;
      case 0x14:
        return nr14;
      case 0x16:
        return nr21;
      case 0x17:
        return nr22;
      case 0x19:
        return nr24;
      case 0x1A:
        return nr30 | 0b01111111;
      case 0x1B:
        return nr31;
      case 0x1C:
        return nr32 | 0b10011111;
      case 0x1E:
        return nr34;
      case 0x20:
        return nr41 | 0b11000000;
      case 0x21:
        return nr42;
      case 0x22:
        return nr43;
      case 0x23:
        return nr44 | 0b00111111;
      case 0x24:
        return nr50;
      case 0x25:
        return nr51;
      case 0x26:
        return nr52 | 0b01110000;
      case 0x40:
        return lcdc;
      case 0x41:
        return stat | dzint(ly == lyc) << 2 | ppu_mode | 0b10000000;
      case 0x42:
        return scy;
      case 0x43:
        return scx;
      case 0x44:
        return ly;
      case 0x45:
        return lyc;
      case 0x46:
        return dma;
      case 0x47:
        return bgp;
      case 0x48:
        return obp0;
      case 0x49:
        return obp1;
      case 0x4A:
        return wy;
      case 0x4B:
        return wx;
    }
    return 0xFF;
  }

  auto read_byte(dzint addr) const -> dzint {
    switch (addr >> 12) {
      case 0x0:
      case 0x1:
      case 0x2:
      case 0x3:
        return rom[addr];
      case 0x4:
      case 0x5:
      case 0x6:
      case 0x7:
        if (mbc == 1 || mbc == 3) {
          addr = ((rom_bank & (rom_banks - 1)) << 14) | (addr & 0x3FFF);
        }
        return rom[addr];
      case 0x8:
      case 0x9:
        if (lcd_enabled() && ppu_mode == kModeVram) {
          return 0xFF;
        } else {
          return vram[addr - 0x8000];
        }
      case 0xA:
      case 0xB:
        if (ram_enable) {
          if (mbc == 3 && ram_bank > 7) {
            return 0;
          } else {
            addr = addr & 0x1FFF;
            if (mbc_mode == 1 && ram_bank < ram_banks) {
              addr = addr | (ram_bank << 13);
            }
            return eram[addr];
          }
        }
        break;
      case 0xC:
      case 0xD:
        return wram[addr - 0xC000];
      case 0xE:
        return read_byte(addr - 0x2000);
      case 0xF:
        if (addr < 0xFE00) {
          return read_byte(addr - 0x2000);
        } else if (addr < 0xFEA0) {
          if (lcd_enabled()) {
            switch (ppu_mode) {
              case kModeOam:
              case kModeVram:
                return 0xFF;
            }
          }
          return oram[addr - 0xFE00];
        } else if (addr < 0xFF00) {
          return 0xFF;
        } else if (addr < 0xFF80) {
          return read_byte_io(addr - 0xFF00);
        } else if (addr < 0xFFFF) {
          return hram[addr - 0xFF80];
        } else {
          return ie;
        }
        break;
    }
    return 0xFF;
  }

  auto read_byte_pc() -> dzint {
    auto value = read_byte(pc);
    pc = (pc + 1) & 0xFFFF;
    return value;
  }

  auto read_signed_byte_pc() -> dzint {
    return sign_extend(read_byte_pc());
  }

  auto read_half(dzint addr) const -> dzint {
    return read_byte(addr) | (read_byte(addr + 1) << 8);
  }

  auto read_half_pc() -> dzint {
    auto value = read_half(pc);
    pc = (pc + 2) & 0xFFFF;
    return value;
  }

  void write_byte_io(dzint addr, dzint byte) {
    switch (addr) {
      case 0x00:
        joyp = byte & 0b11110000;
        break;
      case 0x01:
        sb = byte;
        break;
      case 0x02:
        sc = byte;
        break;
      case 0x04:
        div = 0;
        div_cycles = 0;
        tima_cycles = 0;
        break;
      case 0x05:
        tima = byte;
        break;
      case 0x06:
        tma = byte;
        break;
      case 0x07:
        tac = byte;
        break;
      case 0x0F:
        if_ = byte;
        break;
      case 0x10:
        nr10 = byte;
        break;
      case 0x11:
        nr11 = byte;
        break;
      case 0x12:
        nr12 = byte;
        break;
      case 0x14:
        nr14 = byte;
        break;
      case 0x16:
        nr21 = byte;
        break;
      case 0x17:
        nr22 = byte;
        break;
      case 0x19:
        nr24 = byte;
        break;
      case 0x1A:
        nr30 = byte;
        break;
      case 0x1B:
        nr31 = byte;
        break;
      case 0x1C:
        nr32 = byte;
        break;
      case 0x1E:
        nr34 = byte;
        break;
      case 0x20:
        nr41 = byte;
        break;
      case 0x21:
        nr42 = byte;
        break;
      case 0x22:
        nr43 = byte;
        break;
      case 0x23:
        nr44 = byte;
        break;
      case 0x24:
        nr50 = byte;
        break;
      case 0x25:
        nr51 = byte;
        break;
      case 0x26:
        nr52 = byte;
        break;
      case 0x40:
        lcdc = byte;
        break;
      case 0x41:
        stat = byte & 0b11111000;
        break;
      case 0x42:
        scy = byte;
        break;
      case 0x43:
        scx = byte;
        break;
      case 0x44:
        break;
      case 0x45:
        lyc = byte;
        break;
      case 0x46:
        dma = byte;
        oam_dma(byte);
        break;
      case 0x47:
        bgp = byte;
        break;
      case 0x48:
        obp0 = byte;
        break;
      case 0x49:
        obp1 = byte;
        break;
      case 0x4A:
        wy = byte;
        break;
      case 0x4B:
        wx = byte;
        break;
    }
  }

  void write_byte(dzint addr, dzint byte) {
    switch (addr >> 12) {
      case 0x0:
      case 0x1:
        if ((mbc == 1 || mbc == 3) && ram_exists) {
          ram_enable = (byte & 0xF) == 0xA;
        }
        break;
      case 0x2:
      case 0x3:
        if (mbc == 1) {
          rom_bank = max(1, byte & 0x1F);
        } else if (mbc == 3) {
          rom_bank = max(1, byte & 0x7F);
        }
        break;
      case 0x4:
      case 0x5:
        if (ram_exists) {
          if (mbc == 1) {
            ram_bank = byte & 0x3;
          } else if (mbc == 3) {
            ram_bank = byte;
          }
        }
        break;
      case 0x6:
      case 0x7:
        if (mbc == 1) {
          mbc_mode = byte & 0x1;
        }
        break;
      case 0x8:
      case 0x9:
        if (lcd_enabled() && ppu_mode == kModeVram) {
          return;
        }
        vram[addr - 0x8000] = byte;
        break;
      case 0xA:
      case 0xB:
        if (ram_enable) {
          if (mbc == 3 && ram_bank > 7) {
            return;
          } else {
            addr = addr & 0x1FFF;
            if (mbc_mode == 1 && ram_bank < ram_banks) {
              addr = addr | (ram_bank << 13);
            }
            eram[addr] = byte;
          }
        }
        break;
      case 0xC:
      case 0xD:
        wram[addr - 0xC000] = byte;
        break;
      case 0xE:
        write_byte(addr - 0x2000, byte);
        break;
      case 0xF:
        if (addr < 0xFE00) {
          write_byte(addr - 0x2000, byte);
        } else if (addr < 0xFEA0) {
          if (lcd_enabled()) {
            switch (ppu_mode) {
              case kModeOam:
              case kModeVram:
                return;
            }
          }
          oram[addr - 0xFE00] = byte;
        } else if (addr < 0xFF00) {
          noop;
        } else if (addr < 0xFF80) {
          write_byte_io(addr - 0xFF00, byte);
        } else if (addr < 0xFFFF) {
          hram[addr - 0xFF80] = byte;
        } else {
          ie = byte;
        }
        break;
    }
  }

  void write_half(dzint addr, dzint half) {
    write_byte(addr, half & 0xFF);
    write_byte(addr + 1, (half >> 8) & 0xFF);
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

  void add(dzint other) {
    auto value = a + other;
    set_f((value & 0xFF) == 0, 0, (a ^ other ^ value) & 0x10, value & 0xFF00);
    a = value & 0xFF;
  }

  void add_half(dzint other) {
    auto value = hl() + other;
    set_f(null, 0, (hl() ^ other ^ value) & 0x1000, value & 0xFFFF0000);
    set_hl(value);
  }

  auto add_signed_byte(dzint value) -> dzint {
    dzint byte = read_signed_byte_pc();
    set_f(0, 0, (value & 0xF) + (byte & 0xF) > 0xF, (value & 0xFF) + (byte & 0xFF) > 0xFF);
    return value + byte;
  }

  void adc(dzint other) {
    auto value = a + other + fc();
    set_f((value & 0xFF) == 0, 0, (a ^ other ^ value) & 0x10, value & 0xFF00);
    a = value & 0xFF;
  }

  void sub(dzint other) {
    auto value = a - other;
    set_f((value & 0xFF) == 0, 1, (a ^ other ^ value) & 0x10, value & 0xFF00);
    a = value & 0xFF;
  }

  void sbc(dzint other) {
    auto value = a - other - fc();
    set_f((value & 0xFF) == 0, 1, (a ^ other ^ value) & 0x10, value & 0xFF00);
    a = value & 0xFF;
  }

  void cp(dzint other) {
    auto value = a - other;
    set_f((value & 0xFF) == 0, 1, (a ^ other ^ value) & 0x10, value & 0xFF00);
  }

  void and_(dzint other) {
    a = a & other;
    set_f(a == 0, 0, 1, 0);
  }

  void xor_(dzint other) {
    a = a ^ other;
    set_f(a == 0, 0, 0, 0);
  }

  void or_(dzint other) {
    a = a | other;
    set_f(a == 0, 0, 0, 0);
  }

  void jp(std::optional<dzbool> condition) {
    if (condition == null || *condition) {
      pc = read_half_pc();
      if (condition != null) {
        tick(4);
      }
    } else {
      pc = (pc + 2) & 0xFFFF;
    }
  }

  void jr(std::optional<dzbool> condition) {
    auto offset = 1;
    if (condition == null || *condition) {
      offset = read_signed_byte_pc();
      if (condition != null) {
        tick(4);
      }
    }
    pc = (pc + offset) & 0xFFFF;
  }

  void push(dzint value) {
    sp = (sp - 2) & 0xFFFF;
    write_half(sp, value);
  }

  auto pop() -> dzint {
    auto value = read_half(sp);
    sp = (sp + 2) & 0xFFFF;
    return value;
  }

  void call(std::optional<dzbool> condition) {
    if (condition == null || *condition) {
      auto addr = read_half_pc();
      push(pc);
      pc = addr;
      if (condition != null) {
        tick(12);
      }
    } else {
      pc = (pc + 2) & 0xFFFF;
    }
  }

  void ret(std::optional<dzbool> condition) {
    if (condition == null || *condition) {
      pc = pop();
      if (condition != null) {
        tick(12);
      }
    }
  }

  void rst(dzint addr) {
    push(pc);
    pc = addr;
  }

  void daa() {
    if (fn()) {
      if (fh()) a = (a - 0x06) & 0x00FF;
      if (fc()) a = (a - 0x60) & 0xFFFF;
    } else {
      if (fh() || (a & 0x000F) > 0x09) a = (a + 0x06) & 0xFFFF;
      if (fc() || (a & 0xFFFF) > 0x9F) a = (a + 0x60) & 0xFFFF;
    }
    if ((a & 0x100) == 0x100) {
      set_f(null, null, null, 1);
    }
    a = a & 0xFF;
    set_f(a == 0, null, 0, null);
  }

  void cpu() {
    if (halt) {
      tick(kCycles[0]);
      return;
    }

    auto opcode = read_byte_pc();
    switch (opcode) {
      case 0x00:  // NOP
        break;
      case 0x01:  // LD BC, u16
        c = read_byte_pc();
        b = read_byte_pc();
        break;
      case 0x02:  // LD (BC), A
        write_byte(bc(), a);
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
        write_half(read_half_pc(), sp);
        break;
      case 0x09:  // ADD HL, BC
        add_half(bc());
        break;
      case 0x0A:  // LD A, (BC)
        a = read_byte(bc());
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
        write_byte(de(), a);
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
        jr(null);
        break;
      case 0x19:  // ADD HL, DE
        add_half(de());
        break;
      case 0x1A:  // LD A, (DE)
        a = read_byte(de());
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
        write_byte(hl(), a);
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
      case 0x27:  // DAA
        daa();
        break;
      case 0x28:  // JR Z, s8
        jr(fz());
        break;
      case 0x29:  // ADD HL, HL
        add_half(hl());
        break;
      case 0x2A:  // LD A, (HL+)
        a = read_byte(hl());
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
        a = (~a & 0xFF);
        set_f(null, 1, 1, null);
        break;
      case 0x30:  // JR NC, s8
        jr(!fc());
        break;
      case 0x31:  // LD SP, u16
        sp = read_half_pc();
        break;
      case 0x32:  // LD (HL-), A
        write_byte(hl(), a);
        set_hl(hl() - 1);
        break;
      case 0x33:  // INC SP
        sp = (sp + 1) & 0xFFFF;
        break;
      case 0x34:  // INC (HL)
        write_byte(hl(), inc(read_byte(hl())));
        break;
      case 0x35:  // DEC (HL)
        write_byte(hl(), dec(read_byte(hl())));
        break;
      case 0x36:  // LD (HL), u8
        write_byte(hl(), read_byte_pc());
        break;
      case 0x37:  // SCF
        set_f(null, 0, 0, 1);
        break;
      case 0x38:  // JR C, s8
        jr(fc());
        break;
      case 0x39:  // ADD HL, SP
        add_half(sp);
        break;
      case 0x3A:  // LD A, (HL-)
        a = read_byte(hl());
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
        b = read_byte(hl());
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
        c = read_byte(hl());
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
        d = read_byte(hl());
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
        e = read_byte(hl());
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
        h = read_byte(hl());
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
        l = read_byte(hl());
        break;
      case 0x6F:  // LD L, A
        l = a;
        break;
      case 0x70:  // LD (HL), B
        write_byte(hl(), b);
        break;
      case 0x71:  // LD (HL), C
        write_byte(hl(), c);
        break;
      case 0x72:  // LD (HL), D
        write_byte(hl(), d);
        break;
      case 0x73:  // LD (HL), E
        write_byte(hl(), e);
        break;
      case 0x74:  // LD (HL), H
        write_byte(hl(), h);
        break;
      case 0x75:  // LD (HL), L
        write_byte(hl(), l);
        break;
      case 0x76:  // HALT
        halt = true;
        break;
      case 0x77:  // LD (HL), A
        write_byte(hl(), a);
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
        a = read_byte(hl());
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
        add(read_byte(hl()));
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
        adc(read_byte(hl()));
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
        sub(read_byte(hl()));
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
        sbc(read_byte(hl()));
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
        and_(read_byte(hl()));
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
        xor_(read_byte(hl()));
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
        or_(read_byte(hl()));
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
        cp(read_byte(hl()));
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
        jp(null);
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
        ret(null);
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
        call(null);
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
        ime = 1;
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
        write_byte(0xFF00 | read_byte_pc(), a);
        break;
      case 0xE1:  // POP HL
        set_hl(pop());
        break;
      case 0xE2:  // LDH (C), A
        write_byte(0xFF00 | c, a);
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
      case 0xE8:  // ADD SP, s8
        sp = add_signed_byte(sp) & 0xFFFF;
        break;
      case 0xE9:  // JP (HL)
        pc = hl();
        break;
      case 0xEA:  // LD (u16), A
        write_byte(read_half_pc(), a);
        break;
      case 0xEE:  // XOR A, u8
        xor_(read_byte_pc());
        break;
      case 0xEF:  // RST 0x28
        rst(0x28);
        break;
      case 0xF0:  // LDH A, u8
        a = read_byte(0xFF00 | read_byte_pc());
        break;
      case 0xF1:  // POP AF
        set_af(pop());
        break;
      case 0xF2:  // LDH A, (C)
        a = read_byte(0xFF00 | c);
        break;
      case 0xF3:  // DI
        ime = 0;
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
      case 0xF8:  // LD HL, SP + s8
        set_hl(add_signed_byte(sp));
        break;
      case 0xF9:  // LD SP, HL
        sp = hl();
        break;
      case 0xFA:  // LD A, (u16)
        a = read_byte(read_half_pc());
        break;
      case 0xFB:  // EI
        ime = 1;
        break;
      case 0xFE:  // CP A, u8
        cp(read_byte_pc());
        break;
      case 0xFF:  // RST 0x38
        rst(0x38);
        break;
    }
    tick(kCycles[opcode]);
  }

  void prefix() {
    auto opcode = read_byte_pc();

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
        operand = read_byte(hl());
        break;
      default:
        operand = a;
        break;
    }

    switch (opcode & 0xC7) {
      case 0x06:
      case 0x86:
      case 0xC6:
        tick(8);
        break;
      case 0x46:
        tick(4);
        break;
    }

    auto writeback = true;
    if (opcode <= 0x07) {  // RLC
      operand = ((operand << 1) | (operand >> 7)) & 0xFF;
      set_f(operand == 0, 0, 0, operand & 0x1);
    } else if (opcode <= 0x0F) {  // RRC
      set_f(operand == 0, 0, 0, operand & 0x1);
      operand = ((operand >> 1) | (operand << 7)) & 0xFF;
    } else if (opcode <= 0x17) {  // RL
      auto carry = operand >> 7;
      operand = ((operand << 1) | fc()) & 0xFF;
      set_f(operand == 0, 0, 0, carry);
    } else if (opcode <= 0x1F) {  // RR
      auto carry = operand & 0x1;
      operand = (operand | (fc() << 8)) >> 1;
      set_f(operand == 0, 0, 0, carry);
    } else if (opcode <= 0x27) {  // SLA
      auto carry = operand >> 7;
      operand = (operand << 1) & 0xFF;
      set_f(operand == 0, 0, 0, carry);
    } else if (opcode <= 0x2F) {  // SRA
      auto carry = operand & 0x1;
      operand = (operand & 0x80) | (operand >> 1);
      set_f(operand == 0, 0, 0, carry);
    } else if (opcode <= 0x37) {  // SWAP
      operand = ((operand & 0x0F) << 4) | ((operand & 0xF0) >> 4);
      set_f(operand == 0, 0, 0, 0);
    } else if (opcode <= 0x3F) {  // SRL
      auto carry = operand & 0x1;
      operand = operand >> 1;
      set_f(operand == 0, 0, 0, carry);
    } else if (opcode <= 0x7F) {  // BIT n
      auto bit = (opcode - 0x40) >> 3;
      set_f(!(operand & (1LL << bit)), 0, 1, null);
      writeback = false;
    } else if (opcode <= 0xBF) {  // RES n
      auto bit = (opcode - 0x80) >> 3;
      operand = operand & ~(1LL << bit);
    } else if (opcode <= 0xFF) {  // SET n
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
          write_byte(hl(), operand);
          break;
        default:
          a = operand;
          break;
      }
    }
  }

  void oam_dma(dzint src) {
    src = 0x100 * min(src, 0xF1);
    for (dzint dst = 0xFE00; dst < 0xFEA0; ++dst) {
      write_byte(dst, read_byte(src));
      src = src + 1;
    }
  }

  void irq() {
    dzint servable = ie & if_;
    if (servable) {
      halt = 0;
      if (ime) {
        for (dzint bit = 0; bit < 5; ++bit) {
          dzint mask = 1 << bit;
          if (servable & mask) {
            ime = 0;
            if_ = if_ & ~mask;
            rst(0x40 + 8 * bit);
            break;
          }
        }
      }
    }
  }

  void interrupt(dzint mask) {
    if_ = if_ | mask;
  }

  void interrupt_stat(dzint mask) {
    if (stat & mask) {
      interrupt(kInterruptStat);
    }
  }

  void set_mode(dzint mode) {
    switch (mode) {
      case kModeOam:
        interrupt_stat(kStatOam);
        break;
      case kModeHBlank:
        interrupt_stat(kStatHBlank);
        break;
      case kModeVBlank:
        interrupt(kInterruptVBlank);
        interrupt_stat(kStatVBlank);
        break;
    }
    ppu_mode = mode;
  }

  void increment_line() {
    ly = ly + 1;
    if (ly == lyc) {
      interrupt_stat(kStatLine);
    }
  }

  void tick(dzint cycles) {
    this->cycles = this->cycles + cycles;

    if (tac & 0b100) {
      dzint freq = 1;
      switch (tac & 0b11) {
        case 0b00: freq = 1024; break;
        case 0b01: freq =   16; break;
        case 0b10: freq =   64; break;
        case 0b11: freq =  256; break;
      }

      tima_cycles = tima_cycles + cycles;
      while (tima_cycles >= freq) {
        tima = tima + 1;
        tima_cycles = tima_cycles - freq;

        if (tima == 0x100) {
          tima = tma;
          interrupt(kInterruptTimer);
        }
      }
    }

    div_cycles = div_cycles + cycles;
    while (div_cycles >= 256) {
      div = (div + 1) & 0xFF;
      div_cycles = div_cycles - 256;
    }

    if (lcd_enabled()) {
      ppu_cycles = ppu_cycles + cycles;
      switch (ppu_mode) {
        case kModeOam: {
          if (ppu_cycles >= 80) {
            ppu_cycles = ppu_cycles - 80;
            set_mode(kModeVram);
          }
          break;
        }
        case kModeVram: {
          if (ppu_cycles >= 172) {
            ppu_cycles = ppu_cycles - 172;
            set_mode(kModeHBlank);
            scanline();
          }
          break;
        }
        case kModeHBlank: {
          if (ppu_cycles >= 204) {
            ppu_cycles = ppu_cycles - 204;

            increment_line();
            if (ly == kScreenH) {
              set_mode(kModeVBlank);
              window->render();
            } else {
              set_mode(kModeOam);
            }
          }
          break;
        }
        case kModeVBlank: {
          if (ppu_cycles >= 456) {
            ppu_cycles = ppu_cycles - 456;

            increment_line();
            if (ly == kScreenH + 10) {
              ly = 0;
              set_mode(kModeOam);
            }
          }
          break;
        }
      }
    }
  }

  void scanline() {
    if (lcd_enabled()) {
      transparent.clear();
      transparent.resize(kScreenW, true);

      if (lcdc & 0x01) {
        draw_background();
      }
      if (lcdc & 0x20) {
        draw_window();
      }
      if (lcdc & 0x02) {
        draw_sprites();
      }
    } else {
      window->clear(kPalette[0]);
    }
  }

  auto read_tile(dzint base, dzint tile, dzint x, dzint y) -> dzint {
    dzint addr = 16 * tile + 2 * y + base;
    dzint lsbc = vram[addr + 0] >> (x ^ 0x7);
    dzint msbc = vram[addr + 1] >> (x ^ 0x7);

    return (lsbc & 0x1) | (msbc & 0x1) << 1;
  }

  void draw_background() {
    dzint map_base = 0x1800;
    if (lcdc & 0x08) {
      map_base = map_base + 0x0400;
    }

    dzint tile_base = 0x1000;
    if (lcdc & 0x10) {
      tile_base = tile_base - 0x1000;
    }

    dzint y = ly;
    for (dzint x = 0; x < kScreenW; ++x) {
      dzint texel_x = (x + scx) & 0xFF;
      dzint texel_y = (y + scy) & 0xFF;

      dzint tile_x = texel_x >> 3;
      dzint tile_y = texel_y >> 3;
      dzint tile = vram[32 * tile_y + tile_x + map_base];

      if ((lcdc & 0x10) == 0) {
        tile = sign_extend(tile);
      }

      dzint pixel_x = texel_x & 0x7;
      dzint pixel_y = texel_y & 0x7;

      dzint index = read_tile(tile_base, tile, pixel_x, pixel_y);
      window->set_pixel(x, y, color(bgp, index));
      transparent[x] = index == 0;
    }
  }

  void draw_window() {
    dzint wx = this->wx - 7;
    if (wx >= kScreenW) {
      return;
    }

    dzint wy = this->wy;
    if (wy >= kScreenH) {
      return;
    }

    dzint y = ly - wy;
    if (y < 0) {
      return;
    }

    dzint map_base = 0x1800;
    if (lcdc & 0x40) {
      map_base = map_base + 0x0400;
    }

    dzint tile_base = 0x1000;
    if (lcdc & 0x10) {
      tile_base = tile_base - 0x1000;
    }

    for (dzint x = max(0, -wx); x < min(kScreenW, kScreenW - wx); ++x) {
      dzint tile_x = x >> 3;
      dzint tile_y = y >> 3;
      dzint tile = vram[32 * tile_y + tile_x + map_base];

      if ((lcdc & 0x10) == 0) {
        tile = sign_extend(tile);
      }

      dzint pixel_x = x & 0x7;
      dzint pixel_y = y & 0x7;

      dzint index = read_tile(tile_base, tile, pixel_x, pixel_y);
      window->set_pixel(wx + x, ly, color(bgp, index));
      transparent[wx + x] = index == 0;
    }
  }

  void draw_sprites() {
    dzint height = 8;
    if (lcdc & 0x04) {
      height = 16;
    }

    for (dzint entry = 0x9C; entry >= 0; entry -= 4) {
      dzint sy   = oram[entry + 0] - 16;
      dzint sx   = oram[entry + 1] - 8;
      dzint tile = oram[entry + 2];
      dzint data = oram[entry + 3];

      dzint line = ly - sy;
      if (line < 0 || line >= height) {
        continue;
      }

      if (sx <= -8 || sx >= kScreenW) {
        continue;
      }

      if (data & 0x40) {
        line = line ^ (height - 1);
      }

      if (lcdc & 0x04) {
        if (line < 8) {
          tile = tile & 0xFE;
        } else {
          tile = tile | 0x01;
          line = line - 8;
        }
      }

      dzint palette = obp0;
      if (data & 0x10) {
        palette = obp1;
      }

      dzint flip_x = 0;
      if (data & 0x20) {
        flip_x = 0x7;
      }

      dzint pixel_y = line;
      for (dzint pixel_x = max(0, -sx); pixel_x < min(8, kScreenW - sx); ++pixel_x) {
        dzint index = read_tile(0, tile, pixel_x ^ flip_x, pixel_y);
        if (index == 0) {
          continue;
        }

        dzint x = sx + pixel_x;
        if ((data & 0x80) == 0 || transparent[x]) {
          window->set_pixel(x, ly, color(palette, index));
        }
      }
    }
  }

  auto boot(const dzbytes& rom) -> dzbool {
    this->rom = rom;
    if (rom.size() < 0x8000) {
      std::printf("rom too small\n");
      return false;
    }

    switch (rom[0x147]) {
      case 0x00:
      case 0x08:
      case 0x09:
        mbc = 0;
        break;
      case 0x01:
      case 0x02:
      case 0x03:
        mbc = 1;
        break;
      case 0x0F:
      case 0x10:
      case 0x11:
      case 0x12:
      case 0x13:
        mbc = 3;
        break;
      default:
        std::printf("unsupported cartridge type %d\n", int(rom[0x147]));
        return false;
    }

    switch (rom[0x148]) {
      case 0x00: rom_banks =   2; break;
      case 0x01: rom_banks =   4; break;
      case 0x02: rom_banks =   8; break;
      case 0x03: rom_banks =  16; break;
      case 0x04: rom_banks =  32; break;
      case 0x05: rom_banks =  64; break;
      case 0x06: rom_banks = 128; break;
      case 0x07: rom_banks = 256; break;
      case 0x08: rom_banks = 512; break;
      default:
        std::printf("unsupported rom size %d\n", int(rom[0x148]));
        return false;
    }

    if (0x4000 * rom_banks != rom.size()) {
      std::printf("expected rom size %d but got %d\n", int(0x4000 * rom_banks), int(rom.size()));
      return false;
    }

    switch (rom[0x147]) {
      case 0x02:
      case 0x03:
      case 0x08:
      case 0x09:
      case 0x0C:
      case 0x0D:
      case 0x10:
      case 0x12:
      case 0x13:
        ram_exists = 1;
        break;
    }

    if (ram_exists) {
      switch (rom[0x149]) {
        case 0x00: ram_banks =  0; break;
        case 0x02: ram_banks =  1; break;
        case 0x03: ram_banks =  4; break;
        case 0x04: ram_banks = 16; break;
        case 0x05: ram_banks =  8; break;
        default:
          std::printf("unsupported ram size %d\n", int(rom[0x149]));
          return false;
      }
      eram.resize(0x2000 * ram_banks, 0xFF);
    }
    return true;
  }

  void frame() {
    while (true) {
      cpu();
      irq();

      if (cycles >= 70224) {
        cycles = cycles - 70224;
        break;
      }
    }
  }

  auto run(const dzbytes& rom) -> dzint {
    if (!boot(rom)) {
      return 1;
    }

    while (dz::sdl_events()) {
      dzint time = dz::time();
      frame();
      dzint wait = 16 - dz::time() + time;
      if (wait > 0) {
        dz::sleep(wait);
      }
    }
    return 0;
  }
};

auto main(int argc, char* argv[]) -> int {
  std::atexit(SDL_Quit);

  if (argc < 2) {
    std::printf("cannot run without rom\n");
    return 1;
  }

  auto rom = dz::read_bin(argv[1]);
  if (!rom) {
    std::printf("cannot read '%s'\n", argv[1]);
    return 1;
  }

  return GameBoy().run(*rom);
}
