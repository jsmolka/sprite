# sprite
A Game Boy emulator written in [drizzle](https://github.com/jsmolka/drizzle).

## Why
This emulator serves as the ultimate stress test for the language. It implements most components of the system with reasonable accuracy, but there is no sound or saves, and the cartridge types are limited to MBC0, MBC1, and MBC3. Performance is quite poor. The emulator runs at around 45% of the console. That could be increased to 65% by using free functions and variables instead of a class.

## Usage
Download the latest version of [drizzle-sdl](https://github.com/jsmolka/drizzle/releases) for your system and run `drizzle sprite.dz <rom>`.

## Binaries
Binaries for Windows, Linux and macOS are available as [nightly](https://nightly.link/jsmolka/sprite/workflows/build/master) and [release](https://github.com/jsmolka/sprite/releases) builds.

## Build
Detailed build instructions can be found [here](BUILD.md).

![screenshot](screenshot.png)
