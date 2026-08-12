# Dotfiles Repository

This repo deploys with its own symlink planner (`_dotfiles_sync/link.py`); GNU Stow is no longer a dependency. Most top-level directories are packages mirroring `$HOME`. The main intentional non-package top-levels are:

- `fedora/` — Fedora bootstrap/setup namespace
- `skills/`, `pi/` — shared agent source trees
- `dotfiles-sync`, `_dotfiles_sync/` — repo control plane

## Search Tips

Many important paths are hidden, so use hidden-aware search:

- Find files: `rg --files --hidden -g '!.git' -g '!.jj'`
- Grep text: `rg --hidden -g '!.git' -g '!.jj' <pattern>`

## Editing Rules

- Keep tool-specific docs next to the tool (for example `tmux/README.md`), not in the root `README.md`.
- Prefer the repo `Makefile` for validation/formatting: `make check-all`, `make check-python`, `make check-lua`, `make check-prettier`, `make format-all`, etc.
- The repo Makefile runs Prettier on `*.ts`, `*.json`, `*.jsonc`, and `*.css` only. Markdown is not part of `make format-all`. The Neovim config (see `nvim/.config/nvim/lua/format_on_save.lua`) does run Prettier on Markdown on save — that's an editor-level convenience, not a repo-level convention. Keep Markdown source readable in raw form (compact tables, no over-wrapping) so glow and a plain `nvim` view both stay pleasant.
- Preserve path shapes that mirror `$HOME` (for example `.config/...`, `.ssh/...`); the planner maps them one-to-one.
- Avoid adding secrets or private keys. `ssh/.ssh/config` must stay non-sensitive.
- Keep changes minimal and consistent with existing file formats (Lua, Python, shell, TOML, JSON, INI). Lua should stay `stylua`-formatted and `luacheck`-clean.
- **Run the formatter on the files you touched before running any checks:** `ruff format <paths>` for Python, `stylua <paths>` for Lua, `prettier --write <paths>` for ts/json/jsonc/css. The gate pins its own versions (`make tool-versions`) and runs them via `uvx`/`npx`, so a system binary can disagree with `make check-*`; when it does, format through `make format-python` / `make format-lua` / `make format-prettier`. Don't hand-wrap code guessing at what the formatter wants, and don't let `make check-all` be what discovers it — `check-python` fails on `ruff format --check`, so one unformatted edit costs a full gate cycle. Format first, then check once.
- Paths that must never be linked into `$HOME` go in `NAME_PATTERNS` in `_dotfiles_sync/ignore.py` (one rule set, shared by the planner and the backlink audit). A package needing a whole directory linked as one bundle sets `bundle_dirs` in `inventory.py`.

## Repo Policies

- **Shell vs Python:** if a shell script becomes non-trivial, rewrite it in Python. Smell signs: `declare -g`, `set -u`, multi-level scope juggling, parsing tempfiles with `read`, more than ~50 lines of logic, real data structures, or shell portability hacks. Bash is for thin wrappers and dispatch, not logic.
- **No duplicate helpers:** before adding aliases/wrappers/scripts, check whether something equivalent already exists.
- **Audit with history:** keep aliases/plugins/helpers because they are used, not because they might be useful someday. Check shell history for evidence.
- **OS conditionals at boundaries:** keep OS branching in package selection, bootstrap scripts, or backend helpers—not in otherwise-shared user-facing config. Prefer wrappers or sourced OS-specific files.
- **Tests are bug-driven:** a script earns one when it can fail *without you noticing*. Loud failure is not a reason. Write it when the bug lands, and break the code to confirm the test fails before committing. See [`docs/DECISIONS.md`](docs/DECISIONS.md).
- Shared palette guidance lives in `docs/THEME.md`.

## Useful Pointers

- Tool-specific details live in package docs such as `tmux/README.md`, `sway/README.md`, `fuzzel/README.md`, `pi/README.md`, and `skills/README.md`.
- `dotfiles-sync --apply` symlinks `skills/.agents/skills/*` into `~/.agents/skills/` and `pi/.pi/agent/extensions/*.ts` into `~/.pi/agent/extensions/`. All agents read those paths natively.
- Before adding, syncing, or removing a skill, read *Zen Of These Skills* and *Sync procedure* in [`skills/README.md`](skills/README.md). Multi-agent orchestration is `mu`'s job, not a skill's.
- Test suites: `_dotfiles_sync/tests/`, `fedora{,/gaming}/tests/`, `guides/`, `tmux/.config/tmux/scripts/test-status-tools`, and beside linked scripts in `{fuzzel,sway,waybar}/.config/*/scripts/tests/`. All run under `make check-all`.
- Sway window operations use `swaymsg`.
