# Hammerspoon

macOS window/app automation lives in `hammerspoon/.hammerspoon/init.lua`.

## Interactive guide

For a walkthrough with quizzes, see
[`../guides/HAMMERSPOON.md`](../guides/HAMMERSPOON.md). Run `make serve-guides`
from the repo root to open it in a browser.

## What it covers

- Hyper-key window size cycles centered on `Q/R/W` (with `E/X` for top/bottom)
- H/J/K/L directional focus
- A trimmed app launcher/focus set aligned with sway where it makes sense (`B/T/I/Y/M`)
- Finder and Ghostty focus and window creation on the current Space for
  `T`/`Return`/`PadEnter`
- The `F1` help overlay, which mirrors sway's `mod+F1`. The `/` key is not used
  because macOS routes `Hyper+/` (`Cmd+Shift+/`) to the Help menu

## macOS terminal policy

Ghostty is the macOS terminal. The bindings use `open -na "Ghostty"` to open a
window on the current Space without switching to an existing Ghostty window on
another Space.

- `Hyper+T`: focus/cycle a Ghostty window on the current Space. No-op if Ghostty isn't running or has no window here (won't launch Ghostty or swoosh Spaces).
- `Hyper+Return` / `Hyper+PadEnter`: always create a new Ghostty window on the current Space (and launch Ghostty if needed).
- Use `open -na` instead of synthetic focus commands, which can switch Spaces.

## Model

- The current-space-aware window cycling model
