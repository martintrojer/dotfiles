# Cursor

Cursor Agent CLI settings and hooks. Nothing in this package is symlinked;
`dotfiles-sync` manages the two shared JSON files under `~/.cursor/`.

`~/.cursor/cli-config.json` stays the CLI's file. It holds auth, the model
picker, and caches, so a tracked copy would commit account data and churn on
every launch. This repo owns keys in `_dotfiles_sync/cursor_settings.py`:

- `approvalMode: "unrestricted"` — Run Everything (same as `--force` / `--yolo`)
- `attribution.attributeCommitsToAgent` / `attributePRsToAgent`: `false` — no
  Co-authored-by / agent PR attribution injection

`--check` reports drift; `--apply` merges those keys in and leaves the rest
alone. A machine with no `cli-config.json` yet gets a file containing only the
owned keys; the CLI fills its defaults on the next launch.

`_dotfiles_sync/osc7501_hooks.py` merges
`$HOME/.local/share/mu-crew-dotfiles/cursor/osc7501` into `~/.cursor/hooks.json`
without touching hooks registered by `murmur`, `mu`, `muzzle`, or `museum`. The
bridge itself (and its tests) lives in `mu-crew/dotfiles` (`cursor/osc7501`),
cloned by `dotfiles-sync` at the pin in `_dotfiles_sync/pins.py`. It translates
Cursor hook payloads into OSC 7501 program-status reports and, when
`$OSC7577_TOKEN` is set, signed OSC 7577 resume records. This is a temporary
bridge until Cursor Agent ships native OSC 7501 support (Claude Code, Pi, and
Codex already emit it natively); remove `_dotfiles_sync/osc7501_hooks.py` once
native support lands.
