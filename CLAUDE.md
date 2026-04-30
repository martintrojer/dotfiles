# Dotfiles Repository

This repo uses **GNU Stow**. Most top-level directories are stow packages mirroring `$HOME`. The main intentional non-package top-levels are:

- `fedora/` — Fedora bootstrap/setup namespace
- `skills/`, `pi/` — shared agent source trees
- `.claude-plugin/`, `agents/`, `hooks/` — Claude marketplace surface
- `dotfiles-sync`, `_dotfiles_sync/` — repo control plane

## Search Tips

Many important paths are hidden, so use hidden-aware search:

- Find files: `rg --files --hidden -g '!.git' -g '!.jj'`
- Grep text: `rg --hidden -g '!.git' -g '!.jj' <pattern>`

## Editing Rules

- Keep tool-specific docs next to the tool (for example `tmux/README.md`), not in the root `README.md`.
- Prefer the repo `Makefile` for validation/formatting: `make check-all`, `make check-python`, `make check-lua`, `make check-prettier`, `make format-all`, etc.
- Prettier here is for `*.ts`, `*.json`, `*.jsonc`, and `*.css` only. **Do not auto-format Markdown.** Keep Markdown source compact—especially tables—so raw files stay readable in Neovim/glow and cosmetic diffs stay small.
- Preserve path shapes that mirror `$HOME` (for example `.config/...`, `.ssh/...`); these are meant for stow.
- Avoid adding secrets or private keys. `ssh/.ssh/config` must stay non-sensitive.
- Keep changes minimal and consistent with existing file formats (Lua, Python, shell, TOML, JSON, INI). Lua should stay `stylua`-formatted and `luacheck`-clean.
- For stow ignore rules, prefer repo-wide `.stowrc`. Avoid package-local `.stow-local-ignore` unless a package truly needs special behavior; it replaces stow's built-in defaults rather than augmenting them.

## Repo Policies

- **Shell vs Python:** if a shell script becomes non-trivial, rewrite it in Python. Smell signs: `declare -g`, `set -u`, multi-level scope juggling, parsing tempfiles with `read`, more than ~50 lines of logic, real data structures, or shell portability hacks. Bash is for thin wrappers and dispatch, not logic.
- **No duplicate helpers:** before adding aliases/wrappers/scripts, check whether something equivalent already exists.
- **Audit with history:** keep aliases/plugins/helpers because they are used, not because they might be useful someday. Check shell history for evidence.
- **OS conditionals at boundaries:** keep OS branching in package selection, bootstrap scripts, or backend helpers—not in otherwise-shared user-facing config. Prefer wrappers or sourced OS-specific files.
- Shared palette guidance lives in `docs/THEME.md`.

## Useful Pointers

- Tool-specific details live in package docs such as `tmux/README.md`, `sway/README.md`, `fuzzel/README.md`, `pi/README.md`, and `skills/README.md`.
- `dotfiles-sync --apply` symlinks `skills/*` into `~/.agents/skills/` and `pi/extensions/*.ts` into `~/.pi/agent/extensions/`.
- Tmux helper smoke tests live at `tmux/.config/tmux/scripts/test-status-tools`.
- Sway window operations use `swaymsg`.
