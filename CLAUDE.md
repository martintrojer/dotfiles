# Dotfiles Repository

This repository uses **GNU Stow** for dotfile management. Each top-level directory is a stow package, and most config lives under hidden paths (`.config/`, `.ssh/`, `.claude/`).

## Layout

- `nvim/.config/nvim`: Neovim config (Lua + `lazy-lock.json`)
- `zsh/.zshrc`, `tmux/.tmux.conf`: shell/prompt/tmux setup
- `git/.gitconfig*`, `ssh/.ssh/config`: Git and SSH config
- `sway/.config/sway`, `waybar/.config/waybar`, `fuzzel/.config/fuzzel`, `kanshi/.config/kanshi`, `mako/.config/mako`, `swaylock/.config/swaylock`: Wayland/WM and related tooling (wallpapers are not stowed; managed per machine by the `wallpaper` helper under `~/.local/share/wallpapers/`)
- `eza/.config/eza`: eza theme (Catppuccin Mocha)
- `vscode/settings.json`, `vale/.vale.ini`, `bat/.config/bat`, `btop/.config/btop`, `yazi/.config/yazi`: app configs
- `ghostty/.config/ghostty`: macOS terminal config
- `local-bin/.local/bin`: shared cross-platform user commands on `$PATH`
- `fedora/`: setup scripts plus `containers/` and `systemd/` configs
- `fedora/bin/.local/bin`: Fedora-only user commands on `$PATH`
- `claude/`: Claude-related configs and a local plugin (`claude/mtrojer-plugin`)
- `pi/extensions/`: Pi coding agent extensions
- `skills/`: shared agent skills (symlinked into `~/.agents/skills/`)

## Search Tips

Many nested folders start with `.`. Most default searches skip these, so use hidden-aware search:
- Find files: `rg --files --hidden -g '!.git' -g '!.jj'`
- Grep text: `rg --hidden -g '!.git' -g '!.jj' <pattern>`

## Editing Notes

- Keep config-specific documentation in the relevant package folder (e.g. `tmux/README.md`), not in the root `README.md`
- README.md / LICENSE at a package root are ignored by stow's built-in defaults; no `.stow-local-ignore` needed for the common case. Only add a local ignore file when a package has *nested* files that should not be stowed (current example: `yazi/.stow-local-ignore` for the Catppuccin flavor tree). Note: a local ignore file fully **replaces** the built-in defaults rather than augmenting them, so include the root-level rules when you add nested ones.
- Preserve path shapes that mirror `$HOME` (e.g. `.config/...`, `.ssh/...`); these are intended for stow
- Avoid adding secrets or private keys. `ssh/.ssh/config` should stay non-sensitive
- Keep changes minimal and consistent with existing formats (Lua, Python, shell, TOML, JSON, INI)
- Keep Lua files formatted with `stylua` and lint-clean with `luacheck`
- See `THEME.md` for shared theme and palette references across tools

## Script Locations

Scripts are stored in tool-specific directories under `.config/*/scripts/`:

### Fuzzel Scripts
- `fuzzel/.config/fuzzel/scripts/*` - Python 3 picker scripts (extensionless executable names)
- Shared helpers: `fuzzel/.config/fuzzel/scripts/_common.py`

### Sway Scripts
- `sway/.config/sway/scripts/lock-screen` - Python `swaylock` wrapper: dims the wallpaper, stamps a `LOCKED user@host` banner, and plucks a wallpaper-derived accent color for the ring (cached under `$XDG_CACHE_HOME/lock-screen/`). `--help` for knobs; unknown flags pass through to swaylock. See `sway/README.md` § Lock Screen.

### Tmux Scripts
- `tmux/.config/tmux/scripts/agent-attention` - AI agent attention tracker (Python 3, supports claude/codex/opencode/pi)
- `tmux/.config/tmux/scripts/status-ai` - Renders the boxed `AI <count>` segment for the right-hand status
- `tmux/.config/tmux/scripts/status-ram` - Cross-platform RAM usage percentage for the status bar
- `tmux/.config/tmux/scripts/status-uptime` - Cross-platform short uptime string for the status bar
- `tmux/.config/tmux/scripts/_status_common.py` - Shared helpers (throttled error log + `EXPECTED_ERRORS` tuple) consumed by every `status-*` script
- `tmux/.config/tmux/scripts/status-window-label` - Active-pane derived window label, prefers the real running agent over stale titles
- `tmux/.config/tmux/scripts/tms` - local Python tmux session launcher (pinned sessions + tmux sessions + zoxide + finder)
- `tmux/.config/tmux/tms.toml` - pinned session config plus finder/preview knobs and optional per-session split mode (`vertical` / `horizontal`)
- `tms pick-and-connect` - opens the picker and attaches to the selection (bound to `prefix + s` and the `tm` zsh function)
- `tmux/.config/tmux/scripts/move-pane-picker` - `choose-tree` driven helper for `prefix + M` (move pane into another window/pane)
- `tmux/.config/tmux/scripts/pi-extensions/agent-attention.ts` - Pi Agent extension for agent-attention
- `tmux/.config/tmux/scripts/opencode-plugin/notify.ts` - OpenCode plugin for agent-attention
- `tmux/.config/tmux/scripts/cheatsheet` - fzf cheatsheet popup
- `tmux/.config/tmux/scripts/test-status-tools` - Smoke tests for `agent-attention`, `status-window-label`, and `cheatsheet` against an isolated tmux server

#### Running the tmux status tool tests

Runs the helpers against an isolated tmux server (`-L test-status-tools`) and a throwaway `XDG_STATE_HOME`, so it never touches your real tmux session, sockets, or pending events:

```bash
tmux/.config/tmux/scripts/test-status-tools           # run all assertions, exit 0 on success
tmux/.config/tmux/scripts/test-status-tools -v        # verbose subprocess output
tmux/.config/tmux/scripts/test-status-tools --keep    # leave the test tmux server + tmpdir up for inspection
```

When `--keep` is set, attach to the test server with:

```bash
tmux -L test-status-tools attach
```

Both `agent-attention` and `cheatsheet` honor `TMUX_SOCKET_NAME` / `TMUX_SOCKET_PATH` so the test harness can pin them to a non-default tmux server, which is what makes the tests possible. Real tmux subprocesses inherit `TMUX` from their pane and ignore these env vars.

## Package Docs

- Fuzzel-specific scripts, conventions, and config notes now live in `fuzzel/README.md`

## Fedora Setup

- Shared Fedora package lists live in `fedora/base-packages.sh` and `fedora/sway-packages.sh`
- `fedora/setup-base.sh`, `fedora/setup-sway.sh`, and `fedora/setup-toolbox.sh` are thin wrappers around those shared lists (rpm-ostree only; mutable workstation variants are no longer supported)
- `fedora/setup-toolbox-ubuntu.sh` and `fedora/setup-toolbox-arch.sh` follow the same package intent, but keep distro-specific package names and bootstrap steps

## Pi Extensions

Pi coding agent extensions live in `pi/extensions/` and are symlinked into `~/.pi/agent/extensions/`.

- `answer` — Extract questions from assistant responses into interactive Q&A TUI
- `btw` — Side-chat popover for tangential questions

See `pi/README.md` for details.

## Skills

Generic agent skills live in `skills/` and are symlinked into `~/.agents/skills/`. See `skills/README.md` for full reference.

- `brainstorm` — Refine ideas into technical specs via dialogue
- `write-plan` / `execute-plan` — Create and execute implementation plans
- `code-reviewer` / `test-reviewer` — Review code and tests for quality issues
- `council` — Multi-agent collaborative debate
- `changelog` — Generate changelogs from git history
- `caveman` — Token-efficient communication mode
- `summarize` — `summarize.sh` workflow helper for URL/PDF/DOCX → Markdown via `markitdown`
- `tmux` — Remote-control tmux sessions for interactive CLIs

## Window Manager

Uses **sway** (Wayland compositor). Window operations use `swaymsg` commands.
