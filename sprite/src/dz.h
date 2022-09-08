#pragma once

#include <algorithm>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <optional>
#include <stdexcept>
#include <vector>

#if _MSC_VER
#  include <SDL2/SDL.h>
#else
#  include "SDL.h"
#endif

using dzbool  = bool;
using dzint   = std::int64_t;
using dzfloat = double;

template<typename T>
using dzlist  = std::vector<T>;
using dzbytes = dzlist<std::uint8_t>;

inline constexpr auto noop = 0;
inline constexpr auto null = std::nullopt;

inline auto read_bin(const std::filesystem::path& file) -> std::optional<dzbytes> {
  auto stream = std::ifstream(file, std::ios::binary);
  if (!stream.is_open() || !stream) {
    return std::nullopt;
  }

  dzbytes bytes;
  const auto size = std::filesystem::file_size(file);
  bytes.resize(size);

  stream.read(reinterpret_cast<char*>(bytes.data()), size);
  if (!stream) {
    return std::nullopt;
  }
  return bytes;
}

class SdlWindow {
public:
  SdlWindow(const char* title, dzint w, dzint h, dzint scale) {
    if (!SDL_WasInit(SDL_INIT_VIDEO)) {
      if (SDL_InitSubSystem(SDL_INIT_VIDEO) < 0) {
        return;
      }
    }

    if (w < 0 || h < 0 || scale < 1) {
      return;
    }

    window = SDL_CreateWindow(
      title,
      SDL_WINDOWPOS_CENTERED,
      SDL_WINDOWPOS_CENTERED,
      w * scale,
      h * scale,
      SDL_WINDOW_RESIZABLE);

    if (!window) {
      return;
    }

    renderer = SDL_CreateRenderer(
      window, -1,
      SDL_RENDERER_ACCELERATED | SDL_RENDERER_TARGETTEXTURE);

    if (!renderer) {
      return;
    }

    SDL_RenderSetLogicalSize(renderer, w, h);

    texture = SDL_CreateTexture(
      renderer,
      SDL_PIXELFORMAT_ARGB8888,
      SDL_TEXTUREACCESS_STREAMING,
      w, h);

    buffer.resize(w * h, 0xFFFFFFFF);
  }

  ~SdlWindow() {
    if (texture) {
      SDL_DestroyTexture(texture);
      texture = nullptr;
    }
    if (renderer) {
      SDL_DestroyRenderer(renderer);
      renderer = nullptr;
    }
    if (window) {
      SDL_DestroyWindow(window);
      window = nullptr;
    }
  }

  operator bool() const {
    return window && renderer && texture;
  }

  void set_pixel(dzint x, dzint y, dzint color) {
    int w;
    int h;
    SDL_RenderGetLogicalSize(renderer, &w, &h);

    if (x < 0 || x >= w || y < 0 || y >= h) {
      throw std::runtime_error("pixel index out of range");
    }
    buffer[w * y + x] = static_cast<std::uint32_t>(color);
  }

  auto width() const -> dzint {
    int w;
    int h;
    SDL_RenderGetLogicalSize(renderer, &w, &h);
    return w;
  }

  auto height() const -> dzint {
    int w;
    int h;
    SDL_RenderGetLogicalSize(renderer, &w, &h);
    return h;
  }

  void clear(dzint color) {
    std::fill(buffer.begin(), buffer.end(), static_cast<std::uint32_t>(color));
  }

  void render() {
    int w;
    int h;
    SDL_RenderGetLogicalSize(renderer, &w, &h);

    SDL_UpdateTexture(texture, NULL, buffer.data(), sizeof(std::uint32_t) * w);
    SDL_RenderCopy(renderer, texture, NULL, NULL);
    SDL_RenderPresent(renderer);
  }

private:
  SDL_Window* window = nullptr;
  SDL_Renderer* renderer = nullptr;
  SDL_Texture* texture = nullptr;
  std::vector<std::uint32_t> buffer;
};

inline auto sdl_events() -> dzbool {
  SDL_Event event;
  while (SDL_PollEvent(&event)) {
    if (event.type == SDL_QUIT) {
      return false;
    }
  }
  return true;
}

inline auto sdl_keystate(dzint key) -> dzbool {
  if (key >= 0 && key < SDL_NUM_SCANCODES) {
    return SDL_GetKeyboardState(NULL)[key];
  } else {
    return false;
  }
}

inline auto sdl_window(const char* title, dzint w, dzint h, dzint scale) -> SdlWindow* {
  return new SdlWindow(title, w, h, scale);
}
