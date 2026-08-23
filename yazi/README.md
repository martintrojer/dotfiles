# Yazi

Terminal file manager config lives under `yazi/.config/yazi/`.

## Interactive guide

For a walkthrough with quizzes, see
[`../guides/YAZI.md`](../guides/YAZI.md). Run `make serve-guides` from the repo
root to open it in a browser.

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
- `c i` and `g t` reuse the shared `clipf` and `trash` commands from `local-bin/.local/bin/`.
