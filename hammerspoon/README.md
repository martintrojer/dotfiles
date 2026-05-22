# Hammerspoon

macOS window/app automation lives in `hammerspoon/.hammerspoon/init.lua`.

## Interactive Guide

For a browser-friendly walkthrough with quizzes, see [`../guides/HAMMERSPOON.md`](../guides/HAMMERSPOON.md) and run `make serve-guides` from the repo root.

## What it covers

- Hyper-key window size cycles centered on `Q/R/W` (with `E/X` for top/bottom)
- H/J/K/L directional focus
- A trimmed app launcher/focus set aligned with sway where it makes sense (`B/T/I/Y/M`)
- Finder and Ghostty current-Space-aware focus/new-window behavior for `T`/`Return`/`PadEnter`, and the `F1` help overlay (mirrors sway's `mod+F1`; `/` would have been the obvious choice but `Hyper+/` is `Cmd+Shift+/` which macOS routes to the Help menu)

## macOS terminal policy

Ghostty is the macOS terminal. The bindings use `open -na "Ghostty"` to spawn windows on the current Space without activating an existing Ghostty window on another Space (which would make macOS swoosh there).

- `Hyper+T`: focus/cycle a Ghostty window on the current Space. No-op if Ghostty isn't running or has no window here (won't launch Ghostty or swoosh Spaces).
- `Hyper+Return` / `Hyper+PadEnter`: always create a new Ghostty window on the current Space (and launch Ghostty if needed).
- Avoid generic synthetic focus hacks that jump Spaces; the `open -na` primitive is the load-bearing detail.

## Model

- The current-space-aware window cycling model
