# Dotfiles
Intended to be used with [GNU Stow](https://www.gnu.org/software/stow/)

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
- `wallpapers`

Fedora-specific setup lives under `fedora/`, with shared package definitions in:
- `fedora/base-packages.sh`
- `fedora/sway-packages.sh`

Other notable shared packages:
- `tmux/` for tmux config, local picker scripts, and status helpers
- `zsh/` for shell config and the `tm` helper that launches the local tmux session picker
- `tmux/` for tmux config, local session picker/launcher scripts, and status helpers
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
