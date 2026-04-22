# Dotfiles Repository

This repository uses **GNU Stow** for dotfile management. Each top-level directory is a stow package, and most config lives under hidden paths (`.config/`, `.ssh/`, `.claude/`).

## Layout

- `nvim/.config/nvim`: Neovim config (Lua + `lazy-lock.json`)
- `zsh/.zshrc`, `tmux/.tmux.conf`: shell/prompt/tmux setup
- `git/.gitconfig*`, `ssh/.ssh/config`: Git and SSH config
- `niri/.config/niri`, `waybar/.config/waybar`, `fuzzel/.config/fuzzel`, `kanshi/.config/kanshi`, `mako/.config/mako`, `swaylock/.config/swaylock`, `wallpapers/.config/wallpapers`: Wayland/WM and related tooling
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
- Add or update `.stow-local-ignore` in each package to keep `README.md` from being stowed into `$HOME`
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

### Tmux Scripts
- `tmux/.config/tmux/scripts/agent-attention` - AI agent attention tracker (Python 3, supports claude/codex/opencode/pi)
- `tmux/.config/tmux/scripts/status-ai` - Renders the boxed `AI <count>` segment for the right-hand status
- `tmux/.config/tmux/scripts/status-ram` - Cross-platform RAM usage percentage for the status bar
- `tmux/.config/tmux/scripts/status-uptime` - Cross-platform short uptime string for the status bar
- `tmux/.config/tmux/scripts/status-window-label` - Active-pane derived window label, prefers the real running agent over stale titles
- `tmux/.config/tmux/scripts/tms` - local Python tmux session launcher (pinned sessions + tmux sessions + zoxide + finder)
- `tmux/.config/tmux/tms.toml` - pinned session config plus finder/preview knobs and optional per-session split mode (`vertical` / `horizontal`)
- `tmux/.config/tmux/scripts/session-picker` - thin wrapper that opens the `tms` picker (bound to `prefix + s`)
- `tmux/.config/tmux/scripts/move-pane-picker` - `choose-tree` driven helper for `prefix + M` (move pane into another window/pane)
- `tmux/.config/tmux/scripts/pi-extensions/agent-attention.ts` - Pi Agent extension for agent-attention
- `tmux/.config/tmux/scripts/opencode-plugin/notify.ts` - OpenCode plugin for agent-attention
- `tmux/.config/tmux/scripts/cheatsheet` - fzf cheatsheet popup
- `tmux/.config/tmux/scripts/test-status-tools` - Smoke tests for `agent-attention` and `status-window-label` against an isolated tmux server

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

The script honors `TMUX_SOCKET_NAME` / `TMUX_SOCKET_PATH` to pin `agent-attention` to a non-default tmux server, which is what makes the tests possible. Real tmux subprocesses inherit `TMUX` from their pane and ignore these env vars.

## Package Docs

- Fuzzel-specific scripts, conventions, and config notes now live in `fuzzel/README.md`

## Fedora Setup

- Shared Fedora package lists live in `fedora/base-packages.sh` and `fedora/niri-packages.sh`
- `fedora/setup-base*.sh`, `fedora/setup-niri*.sh`, and `fedora/setup-toolbox.sh` are thin wrappers around those shared lists
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

Uses **niri** (Wayland compositor). Window operations use `niri msg` commands.
