# _dotfiles_sync

Repo-specific control plane behind [`../dotfiles-sync`](../dotfiles-sync).

This is a repo-specific tool, not a generic dotfiles framework. It keeps the `dotfiles-sync` bootstrap logic in plain Python modules under `_dotfiles_sync/` without claiming that the code applies to arbitrary repos. [`../docs/DECISIONS.md`](../docs/DECISIONS.md) explains why `_dotfiles_sync/` uses this module split and avoids a DSL.

## Scope

The control plane owns:

- Symlink planning, apply and check
- OS/distro scope selection
- Conflict handling and backlink checks
- Pinned third-party clones (zsh plugins, TPM)
- Per-package link mode (`skills/` links each skill as one bundle; everything else per-leaf)
- Post-apply hints for the manual Codex notify step

It does not own:

- Top-level packages such as `zsh/`, `nvim/`, `tmux/`, `skills/`, and `pi/`
- The `fedora/` namespace, which stays a special-case package subtree plus setup wrappers

## Module map

- `cli.py`: argument parsing, top-level flow, task dispatch, and post-apply hints
- `config.py`: shared constants and check/apply task policy
- `inventory.py`: package inventory, grouping, and selection
- `pins.py`: pinned clone refs and destinations
- `system.py`: OS and distro detection, plus active scope selection
- `link.py`: link states, per-leaf and bundle walks, and apply logic
- `ignore.py`: the shared rules for paths that never link into `$HOME`
- `sync.py`: conflict, `--ignore`, backup, check, and apply policy
- `repo_checks.py`: package coverage, private-environment checks, cleanup, backlink checks, and systemd unit-target resolution
- `integration_checks.py`: external drift checks for zsh plugins, TPM, and Codex notify
- `external.py`: pinned third-party clone management for zsh plugins and TPM
- `model.py`: typed dataclasses and shared aliases

## Design rules

- Keep abstractions boring and explicit.
- Prefer plain functions and typed data over framework-y indirection.
- Keep repo-specific facts in `inventory.py`, `pins.py`, or `config.py`; do not hide them in control flow.
- If a concern only exists because of one external integration (Codex notify, TPM, Pi auto-discovery), say so plainly in code and docs.
- If a split would only create a fake reusable layer, do not do it.

## Operator commands

From the repo root:

```bash
./dotfiles-sync --check                  # check everything for the active OS/distro
./dotfiles-sync --apply                  # apply everything for the active OS/distro
./dotfiles-sync --check zsh tmux         # check just these packages
./dotfiles-sync --apply nvim             # apply just this package
```

## Quality bar

From the repo root, `make check-python` runs `ruff`, `ty`, and `py_compile` on every Python file in the repo. Run it before sending a change. The Makefile pins the versions and fetches them through `uv`; see *Pinned toolchain* in [`../docs/SETUP.md`](../docs/SETUP.md).
