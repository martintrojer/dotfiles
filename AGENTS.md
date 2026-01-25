# AGENTS

## Overview
This repo is a GNU Stow-based dotfiles collection. Each top-level directory is a stow package, and most config lives under hidden paths (for example `.config/`, `.ssh/`, `.claude/`).

## Layout (high level)
- `nvim/.config/nvim`: Neovim config (Lua + `lazy-lock.json`).
- `zsh/.zshrc`, `tmux/.tmux.conf`, `starship/.config/starship.toml`: shell/prompt/tmux setup.
- `git/.gitconfig*`, `ssh/.ssh/config`: Git and SSH config.
- `hypr/.config/hypr`, `niri/.config/niri`, `waybar/.config/waybar`, `wofi/.config/wofi`, `kanshi/.config/kanshi`, `mako/.config/mako`: Wayland/WM and related tooling.
- `vscode/settings.json`, `vale/.vale.ini`, `bat/.config/bat`, `btop/.config/btop`, `yazi/.config/yazi`, `ghostty/.config/ghostty`: app configs.
- `fedora/`: setup scripts plus `containers/` and `systemd/` configs.
- `claude/`: Claude-related configs and a local plugin (`claude/mtrojer-plugin`).

## Search tips (hidden directories)
There are many nested folders starting with `.`. Most default searches skip these, so use hidden-aware search:
- Find files: `rg --files --hidden -g '!.git' -g '!.jj'`
- Grep text: `rg --hidden -g '!.git' -g '!.jj' <pattern>`

## Editing notes
- Keep config-specific documentation in the relevant package folder (e.g. `tmux/README.md`), not in the root `README.md`.
- Add or update `.stow-local-ignore` in each package to keep `README.md` from being stowed into `$HOME`.
- Preserve path shapes that mirror `$HOME` (e.g. `.config/...`, `.ssh/...`); these are intended for stow.
- Avoid adding secrets or private keys. `ssh/.ssh/config` should stay non-sensitive.
- Keep changes minimal and consistent with existing formats (Lua, shell, TOML, JSON, INI).
