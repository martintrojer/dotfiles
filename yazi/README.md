# Yazi

Terminal file manager config lives under `yazi/.config/yazi/`.

## Interactive Guide

For a browser-friendly walkthrough with quizzes, see [`../guides/YAZI.md`](../guides/YAZI.md) and run `make serve-guides` from the repo root.

## What it covers

- Basic navigation and help
- Visual mode and bulk operations
- Tabs and the task manager / background workers
- `fd` / `ripgrep` search
- `fzf` / `zoxide` jumps
- Repo-local custom keymaps (`c i`, `g s`, `g t`, `g l`)

## Shell overlap

- `y` launches Yazi and adopts the directory you exit from.
- `z` means zoxide in both zsh and Yazi; `Z` is the fzf jump inside Yazi.
- `g l` reuses the shared pager-backed preview command (`m`) instead of shelling out to raw `glow`.
- `c i` and `g t` reuse the shared `clipf` and `trashd` commands from `local-bin/.local/bin/`.
