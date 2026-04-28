# Zsh

Slim, framework-free zsh config. No oh-my-zsh, no plugin manager. Native compinit, native keybindings, two third-party plugins sourced directly. Mirrors the philosophy of `nvim/` (no framework, every line understood).

## Interactive Guide

For a browser-friendly walkthrough with quizzes, open [`../docs/ZSH_LEARNING_GUIDE.html`](../docs/ZSH_LEARNING_GUIDE.html) locally.

## Setup

`./dotfiles-sync --apply` from the repo root does everything:

1. Stows `zsh/.zshrc` and `zsh/.zsh/` to `~/`.
2. Clones `zsh-autosuggestions` and `zsh-syntax-highlighting` into `~/.local/share/zsh-plugins/<name>/` at pinned versions (see `_dotfiles_sync/pins.py`).

If you want the `tm` tmux helper, install `tmux`, `fzf`, `zoxide`, `fd`, and `eza` (the Fedora `base-packages.sh` covers these).

## Layout

```
.zshrc                 — slim interactive shell config (~110 lines)
.zsh/
  exports.zsh          — PATH, EDITOR, HISTCONTROL, PAGER/LESS, TERM fallbacks
  tools.zsh            — mise activation + hand-rolled prompt (Catppuccin)
  aliases.zsh          — eza-backed ls/tree families (ls, la, ll, lla, lt, llt, lt5, llt5, l), global pipes (F, H, T, G, L), serve, td, gvd, jvd, ff/ffd
  git-aliases.zsh      — 12 cherry-picked git aliases (g, ga, gst, glg, gp, …)
  jj-aliases.zsh       — 10 cherry-picked jj aliases (jjla, jjst, jjgp, jjsq, …)
  functions.zsh        — zknew, nv, mvln, y, rmhist, tm
  homebrew.zsh         — macOS Homebrew PATH bootstrap (sourced first on Darwin)
  os-darwin.zsh        — macOS-only shell tweaks
```

## Highlights

### Aliases (cherry-picked from oh-my-zsh git/jj plugins)

The full lists live in `git-aliases.zsh` and `jj-aliases.zsh`. The keepers:

- **git**: `g`, `ga`, `gb`, `gc`, `gco`, `gd`, `gf`, `gl`, `glg`, `gp`, `grb`, `gst`
- **jj**: `jjb`, `jjd`, `jjdmsg`, `jje`, `jjgp`, `jjla`, `jjn`, `jjrb`, `jjsq`, `jjst`

Selection criterion: ≥5 hits in `~/.zsh_history`. Everything else from the OMZ git (~190) and jj (~25) plugins was dropped.

### Diff review wrappers (in `aliases.zsh`)

- `gvd` → `git difftool --dir-diff --no-prompt --extcmd=nvdiff`
- `jvd` → `jj --no-pager diff --tool nvdiff`
- `nvdiff` is a `fedora/bin/` script that opens Neovim's `:DiffTool`

### Plugins sourced directly

- **zsh-autosuggestions** — ghost-text history suggestions; Right Arrow accepts via the default `forward-char` widget.
- **zsh-syntax-highlighting** — live coloring of the command line (sourced last per upstream docs).
- **fzf** — `eval "$(fzf --zsh)"` provides:
  - `Ctrl-T` file picker
  - `Ctrl-R` history search
  - `Esc-c` cd picker
  - `**` completion trigger: `kill **<TAB>`, `ssh **<TAB>`, `cd **<TAB>`, `vim **<TAB>`, etc.
- **zoxide** — `eval "$(zoxide init zsh)"` provides `z <pat>` (jump) and `zi` (interactive).

### Keybindings (native)

- `↑` / `↓` — `up-line-or-beginning-search` / `down-line-or-beginning-search` (type a prefix then ↑ to history-search by that prefix)
- `^E` — `end-of-line`
- `Ctrl-X Ctrl-E` — `edit-command-line` (open `$EDITOR` to compose the current line)

### Dir aliases

- `...` → `cd ../..`
- `....` → `cd ../../..`
- `-` → `cd -`
- `md` → `mkdir -p`
- `lla` → `ll` plus dotfiles (`eza -la --group-directories-first`)
- `la` → `ls` plus dotfiles (`eza -a --group-directories-first`)
- `lt` / `llt` → tree view at depth 3, with / without long format
- `lt5` / `llt5` → deeper tree view at depth 5, with / without long format
- `td` → `cd "$(trashd)"`
- `ff <pattern>` → `fd --hidden --exclude .git --exclude .jj --type f <pattern>`
- `ffd <pattern>` → `fd --hidden --exclude .git --exclude .jj --type d <pattern>`

### `tm` tmux helper

In `functions.zsh`. Runs `$HOME/.config/tmux/scripts/tms pick-and-connect` — the picker shows the selection UI; on `<Enter>` `tms` attaches or creates the tmux session in one step.

## Plugin update story

Bump the version string in `_dotfiles_sync/pins.py`'s `ZSH_PLUGINS` constant, then run `./dotfiles-sync --apply`. The script:

- Fetches if the pinned ref isn't in the local clone
- Checks out the new ref only if HEAD doesn't already match
- Logs `CLONING` / `FETCHING` / `PINNED` for visible operations; silent if everything's already correct
- `./dotfiles-sync --check` reports `MISSING`, `UNKNOWN-REF`, or `DRIFT` issues

Same model as `nvim/lua/plugins.lua` + `nvim-pack-lock.json`.

## Boundary

Cross-platform behavior in shared files. OS checks for narrow differences (`os-darwin.zsh`, `homebrew.zsh`). Larger Linux-specific behavior would live in `os-linux.zsh` (deleted as unused 2026-04-25; revive if needed).
