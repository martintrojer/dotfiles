# Ghostty

Ghostty is the macOS terminal. The most macOS-native option available: native
AppKit window controls, real app-bundle launch model, single-process tab/split
model that aligns with Mission Control.

Policy:

- Ghostty is the only macOS terminal in this repo. No fallback chain.
- Shared terminal behavior lives in `tmux/`, `zsh/`, `nvim/`. Keep this
  package limited to rendering, input translation, clipboard, and
  current-Space window creation.

Hammerspoon's terminal binds (`Hyper+T`, `Hyper+Return`, `Hyper+PadEnter`)
all use `open -na "Ghostty"` to spawn current-Space windows without
swooshing macOS to existing windows on other Spaces.

See [`docs/DECISIONS.md`](../docs/DECISIONS.md#each-os-gets-its-native-terminal-foot-on-linux-ghostty-on-macos-accepted-2026-05-15)
for the per-OS terminal rationale.
