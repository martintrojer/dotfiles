# Cursor

Cursor Agent CLI settings and hooks. Only `.cursor/hooks/osc7501` is linked
into `~/.cursor/hooks/osc7501`.

`~/.cursor/cli-config.json` stays the CLI's file. It holds auth, the model
picker, and caches, so a tracked copy would commit account data and churn on
every launch. This repo owns keys in `_dotfiles_sync/cursor_settings.py`:

- `approvalMode: "unrestricted"` — Run Everything (same as `--force` / `--yolo`)
- `attribution.attributeCommitsToAgent` / `attributePRsToAgent`: `false` — no
  Co-authored-by / agent PR attribution injection

`--check` reports drift; `--apply` merges those keys in and leaves the rest
alone. A machine with no `cli-config.json` yet gets a file containing only the
owned keys; the CLI fills its defaults on the next launch.

`_dotfiles_sync/osc7501_hooks.py` merges `$HOME/.cursor/hooks/osc7501` into
`~/.cursor/hooks.json` without touching hooks registered by `murmur`, `mu`,
`muzzle`, or `museum`. This is a temporary bridge until Cursor Agent ships
native OSC 7501 support (Claude Code, Pi, and Codex already emit it natively);
watch upstream and remove it along with `cursor/.cursor/hooks/osc7501` once
native support lands.

## OSC 7577 resume record

With `$OSC7577_TOKEN` set, the hook also signs an OSC 7577 record
(`argv=["cursor-agent","--resume",<conversation id>]`, cwd from
`workspace_roots`) on `sessionStart`/`beforeSubmitPrompt` and `clear=1` on
`sessionEnd`, reported for the hook's parent pid (the long-lived cursor-agent).
Tests: `_dotfiles_sync/tests/test_osc7577_cursor.py`.
