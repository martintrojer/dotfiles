# _dotfiles_sync

Repo-specific control plane behind [`../dotfiles-sync`](../dotfiles-sync).

This is intentionally **not** a generic dotfiles framework. It exists to keep
the repo bootstrap code reading like real software without pretending the logic
is reusable across arbitrary repos.

## Scope

Owns:

- Stow apply/check orchestration
- OS/distro scope selection
- Conflict handling and backlink checks
- Pinned third-party clones (zsh plugins, TPM)
- Symlink fan-out for shared skills and Pi extensions
- Post-apply hints for the manual Claude/Codex steps

Does **not** own:

- Top-level stow packages (`zsh/`, `nvim/`, `tmux/`, ...)
- The `fedora/` namespace, which stays a special-case package subtree plus setup wrappers
- The universal agent source trees (`skills/`, `pi/`)
- The Claude marketplace surface (`agents/`, `hooks/`, `.claude-plugin/`), which stays at repo root because that published layout is the integration contract Claude consumes

## Module map

- `cli.py` — argument parsing, top-level flow, task dispatch, post-apply hints
- `config.py` — shared constants plus check/apply task policy
- `inventory.py` — package inventory, package grouping, selection logic
- `pins.py` — pinned clone refs and destinations
- `system.py` — OS/distro detection and active scope selection
- `stow.py` — Stow command execution, conflict parsing, apply/check helpers
- `repo_checks.py` — package coverage, ignore-driven cleanup, backlink checks
- `integration_checks.py` — external drift checks (zsh plugins, TPM, Codex notify)
- `external.py` — pinned clone management plus skills / Pi symlink fan-out
- `model.py` — typed dataclasses and shared aliases

## Design rules

- Keep abstractions boring and explicit.
- Prefer plain functions and typed data over framework-y indirection.
- Keep repo-specific facts in `inventory.py`, `pins.py`, or `config.py`; do not hide them in control flow.
- If a concern only exists because of one external integration (Claude marketplace, TPM, Pi auto-discovery), say so plainly in code and docs.
- If a split would only create a fake reusable layer, do not do it.

## Operator commands

From the repo root:

```bash
./dotfiles-sync --check
./dotfiles-sync --apply
./dotfiles-sync --check zsh tmux
./dotfiles-sync --apply nvim
python3 -m _dotfiles_sync --check
```

## Quality bar

```bash
ty check _dotfiles_sync dotfiles-sync
ruff check _dotfiles_sync dotfiles-sync
ruff format --check _dotfiles_sync dotfiles-sync
```
