# Dotfiles
Intended to be used with [GNU Stow](https://www.gnu.org/software/stow/)

## Zen Of This Setup

The pillars below are the same ones that already govern `nvim/`, `tmux/`, and `fedora/`. They exist so future-me can resist the urge to chase the next shiny compositor release, plugin framework, or distro flavor and instead keep editing actual files.

When tempted, re-read this section before touching anything.

1. **Boring infra is good infra.** Fedora rpm-ostree, Sway, tmux, Neovim, zsh. None of these are exciting in 2026. That is the point. The desktop should be the substrate, not the hobby.
2. **Builtins first, plugins last.** Neovim 0.12 builtins (LSP, completion, comments, snippets, `vim.pack`) before any plugin. Native tmux status formatting, `choose-tree`, popups, and menus before TPM plugins. Standard Fedora repos before COPRs. A plugin or external tool only lands when a builtin genuinely cannot do the job.
3. **Every line is understood.** No framework magic, no hidden keymaps, no "distro" config layers. If a line of config is here, future-me can explain why. If it cannot be explained, it gets deleted.
4. **Each piece earns its place.** Plugins, packages, scripts, services — they all justify themselves with a one-line "why not builtin?" answer. The `nvim/README.md` plugin table is the template for how to think about every other tool too.
5. **Local scripts over upstream plugins.** `tms`, `agent-attention`, `status-*`, `cheatsheet`, `lock-screen`, the fuzzel pickers, `stow-all.py` — small Python scripts in this repo are preferred over adding a third-party dependency. They are easy to read, easy to fix, and they do not break on upgrade.
6. **Recreate, do not restore.** No tmux resurrect. No session snapshots. No magic state restoration. The workflow is rebuilt on demand: `tms` recreates project sessions in two keystrokes, `mini.starter` and `<leader>fo` re-enter files, agents keep their own state. Disposable sessions force the setup to stay cheap to spin up.
7. **Thin wrappers around shared lists.** `fedora/setup-*.sh` are wrappers around `base-packages.sh` / `sway-packages.sh`. `stow-all.py` is a wrapper around stow + skill links + Claude plugin bundle. Decisions live in data, not in scripts.
8. **Opinionated, not agnostic.** Linux is Fedora + Wayland + Sway. macOS is Hammerspoon + Ghostty. The shared layer is the CLI/editor baseline; the desktop stack is allowed to diverge per platform. No effort is spent making the WM/compositor portable.
9. **One palette, everywhere.** Catppuccin Mocha across nvim, tmux, eza, bat, waybar, fuzzel, mako, swaylock. See `THEME.md`. New tools adopt the palette or do not get added.
10. **Config lives next to the thing it configures.** Tool-specific docs go in the package folder (`nvim/README.md`, `tmux/README.md`, `fedora/README.md`, `fuzzel/README.md`, ...). This root README only describes the repo shape and the rules above.

If a new toy violates more than one of these, it does not belong here — no matter how cool the blur effect is.

## Repository Shape

- Portable core: shell, editor, git, terminal tooling
- Linux desktop stack: Fedora + Wayland + `sway`
- macOS desktop stack: Hammerspoon + Ghostty

The shared layer is intentionally the CLI/editor baseline. Desktop behavior is allowed to diverge by platform.

## Stow

Use `./stow-all.py` to stow the packages that match the current OS and distro.

Not everything at the repo root is a stow package. In particular, [`skills/`](./skills) is the shared source of truth for agent skills and is managed directly by `stow-all.py`, not by GNU Stow.

## Agent Skills

Repo-root [`skills/`](./skills) is the source of truth.

`./stow-all.py --apply` does best-effort setup for that shared skill tree:

- Creates or repairs per-skill links in `~/.codex/skills/`
- Creates or repairs per-skill links in `~/.agents/skills/` for OpenCode and pi-agent
- Generates [`claude/mtrojer-plugin/skills/`](./claude/mtrojer-plugin) by copying the shared `skills/` tree into the Claude plugin bundle

Claude then installs that generated plugin bundle into its own cache under `~/.claude/plugins/cache/...`.
That installed Claude copy is a snapshot. If skills are edited or new skills are added, it becomes stale until you reinstall the local plugin.

`./stow-all.py --check` verifies all of these locations:

- `~/.codex/skills/`
- `~/.agents/skills/`
- generated plugin bundle at `~/dotfiles/claude/mtrojer-plugin/skills/`
- installed Claude plugin bundle under `~/.claude/plugins/cache/local/mtrojer/...`

If the Claude plugin is missing, `--check` prints the manual install commands.

## Agent Skill Flow

Run the steps in this order:

1. Populate links and generate the Claude plugin bundle:
   ```bash
   ./stow-all.py --apply
   ```
2. Install or update the Claude Code plugin from that generated bundle:
   ```text
   /plugin marketplace add ~/dotfiles/claude/mtrojer-plugin
   /plugin install mtrojer@local
   ```
3. Verify everything:
   ```bash
   ./stow-all.py --check
   ```

When shared skills change, repeat the same order:

1. `./stow-all.py --apply`
2. `/plugin install mtrojer@local`
3. `./stow-all.py --check`

`stow-all.py` does not run Claude’s `/plugin ...` commands for you. It only prepares the bundle and verifies the installed copy.
If Claude's installed copy is stale, `./stow-all.py --check` tells you to run `/plugin install mtrojer@local`.

Key Linux packages now include:
- `sway`
- `waybar`
- `fuzzel`
- `kanshi`
- `mako`
- `swaylock`

Wallpapers themselves are not stowed; the `wallpaper` helper in `fedora/bin/.local/bin/` manages them per machine under `~/.local/share/wallpapers/`.

Fedora-specific setup lives under `fedora/`, with shared package definitions in:
- `fedora/base-packages.sh`
- `fedora/sway-packages.sh`

Other notable shared packages:
- `tmux/` for tmux config, local session picker/launcher scripts, and status helpers
- `zsh/` for shell config and the `tm` helper that launches the local tmux session picker
- `local-bin/` for shared `~/.local/bin` commands across macOS and Linux

Fedora-only command wrappers live in `fedora/bin/`.

## Platform Stance

Linux in this repo is intentionally opinionated: Fedora + Wayland + `sway`.
The goal is not to keep the desktop stack compositor-agnostic; the goal is to keep the Sway workflow cohesive and maintainable.

## OS Conditionals

Keep OS detection at explicit boundaries such as package selection, bootstrap scripts, and backend helpers.
Avoid embedding platform branches directly in otherwise shared user-facing config when a wrapper script or sourced OS-specific file would be clearer.

## Notes
Project-specific setup details should live in the corresponding package folder
(e.g. `tmux/README.md`, `nvim/README.md`, etc.), not in this root README.
