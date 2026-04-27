# Dotfiles Repository

This repository uses **GNU Stow** for dotfile management. Most top-level directories are stow packages mirroring `$HOME`, but the repo also contains a few intentional non-package top-levels: Fedora bootstrap code, agent source trees, Claude marketplace assets, and the `dotfiles-sync` control plane under `_dotfiles_sync/`.

## Layout

- `nvim/.config/nvim`: Neovim config (Lua + `lazy-lock.json`)
- `zsh/.zshrc`, `tmux/.tmux.conf`: shell/prompt/tmux setup
- `git/.gitconfig*`, `ssh/.ssh/config`: Git and SSH config
- `sway/.config/sway`, `waybar/.config/waybar`, `fuzzel/.config/fuzzel`, `kanshi/.config/kanshi`, `mako/.config/mako`, `swaylock/.config/swaylock`: Wayland/WM and related tooling (wallpapers are not stowed; managed per machine by the `wallpaper` helper under `~/.local/share/wallpapers/`)
- `eza/.config/eza`: eza theme (Catppuccin Mocha)
- `docs/VSCODE.md`, `vale/.vale.ini`, `bat/.config/bat`, `btop/.config/btop`, `yazi/.config/yazi`: app config notes / configs
- `ghostty/.config/ghostty`: macOS terminal config
- `local-bin/.local/bin`: shared cross-platform user commands on `$PATH`
- `fedora/`: setup scripts plus `containers/` and `systemd/` configs
- `fedora/bin/.local/bin`: Fedora-only user commands on `$PATH`
- `.claude-plugin/`, `agents/`, `hooks/`: Claude Code plugin assets kept at repo root because the marketplace integration consumes that published surface directly
- `pi/`: Pi coding agent extensions plus package docs; `dotfiles-sync --apply` symlinks `pi/extensions/*.ts` into `~/.pi/agent/extensions/` (pi auto-discovers there)
- `skills/`: shared agent skills following the Agent Skills standard; `dotfiles-sync --apply` symlinks each into `~/.agents/skills/<name>` (universal path read by Codex, OpenCode, Pi, Cursor, Amp, Cline, Warp, OpenClaw, Claude Code, ...)
- `dotfiles-sync`, `_dotfiles_sync/`: repo control plane for Stow apply/check, pinned clones, and symlink fan-out

## Search Tips

Many nested folders start with `.`. Most default searches skip these, so use hidden-aware search:
- Find files: `rg --files --hidden -g '!.git' -g '!.jj'`
- Grep text: `rg --hidden -g '!.git' -g '!.jj' <pattern>`

## Editing Notes

- Keep config-specific documentation in the relevant package folder (e.g. `tmux/README.md`), not in the root `README.md`
- README.md / LICENSE at a package root are ignored by stow's built-in defaults; repo-wide extra ignore rules live in `.stowrc` (preferred over package-local ignore files). Note: a package-local `.stow-local-ignore` fully **replaces** the built-in defaults rather than augmenting them, so avoid it unless a package truly needs package-specific rules that should not apply repo-wide.
- Preserve path shapes that mirror `$HOME` (e.g. `.config/...`, `.ssh/...`); these are intended for stow
- Avoid adding secrets or private keys. `ssh/.ssh/config` should stay non-sensitive
- Keep changes minimal and consistent with existing formats (Lua, Python, shell, TOML, JSON, INI)
- Keep Lua files formatted with `stylua` and lint-clean with `luacheck`
- **Shell vs Python rule:** if a shell script starts to smell non-trivial, rewrite it in Python. Smell signals: needing `declare -g` or `set -u`, multi-level local/global scope juggling, tab-separated `mktemp` templates being parsed by `read`, more than ~50 lines of logic, anything that wants real data structures, or workarounds for portability between bash/zsh/sh. The repo already has good Python references: `local-bin/.local/bin/m`, `local-bin/.local/bin/solo`, `tmux/.config/tmux/scripts/tms`, `tmux/.config/tmux/scripts/agent-attention`, `fuzzel/.config/fuzzel/scripts/*`. Bash stays for thin wrappers (a single `exec`, an OS dispatch, sourcing env), not for logic.
- **No two aliases (or scripts) doing the same thing.** Before adding a new alias / wrapper / helper, check `grep -hE '^alias ' zsh/.zsh/*.zsh | sed -E 's/^alias[ ]+([^=]+)=//' | sort | uniq -c | sort -rn` (or the moral equivalent for the file type). Two aliases expanding to the same string is dead weight.
- **Audit usage with shell history before keeping/cherry-picking.** When deciding whether something earns its place — OMZ aliases to keep, plugins to vendor, etc. — grep `~/.zsh_history` (or the macOS equivalent) for actual call counts. "I might use it someday" is not evidence; "used 5+ times in 1660 commands" is.
- **OS conditionals at boundaries, not in shared config.** Keep OS detection at explicit boundaries: package selection (`fedora/base-packages.sh` etc.), bootstrap scripts (`dotfiles-sync` package gating), backend helpers (`status-ram` reading `/proc/meminfo` vs `vm_stat`). Avoid embedding `if [[ Linux ]]` branches directly in otherwise-shared user-facing config (zsh aliases, tmux.conf, nvim Lua); prefer a wrapper script or a sourced OS-specific file (e.g. `zsh/.zsh/os-darwin.zsh`) that the shared config calls into.
- See `docs/THEME.md` for shared theme and palette references across tools

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
- `pi/extensions/agent-attention.ts` - Pi Agent extension for agent-attention (distributed via the dotfiles pi package)
- `opencode/.config/opencode/plugin/notify.ts` - OpenCode plugin for agent-attention (stowed)
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
- `fedora/setup-base.sh`, `fedora/setup-sway.sh`, and `fedora/setup-toolbox.sh` are thin wrappers around those shared lists. The host target is **Fedora Sway Atomic (Sericea)**; `sway-packages.sh` only lists what gets layered on top of the Sericea base.
- `fedora/setup-toolbox-ubuntu.sh` and `fedora/setup-toolbox-arch.sh` follow the same package intent, but keep distro-specific package names and bootstrap steps

## Pi Extensions

Pi coding agent extensions live in `pi/extensions/`. `dotfiles-sync --apply` symlinks each `*.ts` into `~/.pi/agent/extensions/`, where pi auto-discovers them (with `/reload` support). Edits propagate live.

- `answer` — Extract questions from assistant responses into interactive Q&A TUI
- `btw` — Side-chat popover for tangential questions

See `pi/README.md` for details.

## Skills

Generic agent skills live in `skills/`. `dotfiles-sync --apply` symlinks each `<name>` into `~/.agents/skills/<name>`. All supported agents (Codex, OpenCode, Pi, Cursor, Amp, Cline, Warp, OpenClaw, Claude Code via the plugin) read this universal path. Edits propagate live. The same `skills/` tree is also bundled into the Claude plugin (`claude plugin install mtrojer@dotfiles`). See `skills/README.md` for full reference.

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
