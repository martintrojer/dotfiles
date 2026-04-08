# Dotfiles
Intended to be used with [GNU Stow](https://www.gnu.org/software/stow/)

## Repository Shape

- Portable core: shell, editor, git, terminal tooling
- Linux desktop stack: Fedora + Wayland + `niri`
- macOS desktop stack: Hammerspoon + Ghostty

The shared layer is intentionally the CLI/editor baseline. Desktop behavior is allowed to diverge by platform.

## Stow

Use `./stow-all.py` to stow the packages that match the current OS and distro.

Key Linux packages now include:
- `niri`
- `waybar`
- `fuzzel`
- `kanshi`
- `mako`
- `swaylock`
- `wallpapers`

Fedora-specific setup lives under `fedora/`, with shared package definitions in:
- `fedora/base-packages.sh`
- `fedora/niri-packages.sh`

## Platform Stance

Linux in this repo is intentionally opinionated: Fedora + Wayland + `niri`.
The goal is not to keep the desktop stack compositor-agnostic; the goal is to keep the `niri` workflow cohesive and maintainable.

## OS Conditionals

Keep OS detection at explicit boundaries such as package selection, bootstrap scripts, and backend helpers.
Avoid embedding platform branches directly in otherwise shared user-facing config when a wrapper script or sourced OS-specific file would be clearer.

## Notes
Project-specific setup details should live in the corresponding package folder
(e.g. `tmux/README.md`, `nvim/README.md`, etc.), not in this root README.
