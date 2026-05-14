# Alacritty

Alacritty is the preferred terminal emulator for this repo on both Fedora/Sway and macOS.

## Package layout

This directory uses nested stow packages:

- `alacritty/` — shared config, stowed on all platforms.
- `alacritty-darwin/` — macOS overlay, stowed only on Darwin.
- `alacritty-linux/` — Linux overlay, stowed only on Linux.

The shared config imports `~/.config/alacritty/os.toml`; exactly one OS overlay owns that path on a given machine.

## macOS policy

macOS uses Alacritty first and Ghostty only as a compatibility fallback for machines that still differ. Hammerspoon's terminal binds (`Hyper+T`, `Hyper+Return`, `Hyper+PadEnter`) look for Alacritty first, then fall back to Ghostty.

Keep terminal-emulator-specific behavior narrow: rendering, input translation, clipboard, and current-Space window creation. Shared shell/editor/multiplexer workflow belongs in zsh, nvim, and tmux.
