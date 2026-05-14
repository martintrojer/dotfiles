# Hammerspoon

macOS window/app automation lives in `hammerspoon/.hammerspoon/init.lua`.

## Interactive Guide

For a browser-friendly walkthrough with quizzes, see [`../guides/HAMMERSPOON.md`](../guides/HAMMERSPOON.md) and run `make serve-guides` from the repo root.

## What it covers

- Hyper-key window size cycles centered on `Q/R/W` (with `E/X` for top/bottom)
- H/J/K/L directional focus
- A trimmed app launcher/focus set aligned with sway where it makes sense (`B/T/I/Y/M`)
- Finder's current-Space-aware focus/new-window behavior, terminal fallback behavior (`Alacritty` first, then `Ghostty`) for `T`/`Return`/`PadEnter`, and the `/` help overlay

## macOS terminal policy

Alacritty is the preferred macOS terminal. Ghostty stays stowed on Darwin as a compatibility fallback for machines that do not have Alacritty installed yet. Hammerspoon's terminal binds follow that policy:

- `Hyper+T`: focus/cycle a terminal on the current Space, preferring Alacritty and falling back to Ghostty.
- `Hyper+Return` / `Hyper+PadEnter`: create a new terminal window on the current Space using the same preference order.
- App-specific launch primitives are intentional: Alacritty uses `alacritty msg ... create-window`; Ghostty uses the best available macOS app launch/new-window fallback. Avoid generic synthetic focus hacks that jump Spaces.

## Model

- The current-space-aware window cycling model
