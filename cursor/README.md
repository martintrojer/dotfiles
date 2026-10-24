# Cursor

Cursor Agent CLI settings. Nothing here is linked.

`~/.cursor/cli-config.json` stays the CLI's file. It holds auth, the model
picker, and caches, so a tracked copy would commit account data and churn on
every launch. This repo owns one key, in `_dotfiles_sync/cursor_settings.py`:
`approvalMode: "unrestricted"` (Run Everything, the same as `--force` /
`--yolo`). `--check` reports drift; `--apply` merges that key in and leaves the
rest alone. A machine with no `cli-config.json` yet gets a file containing only
that key; the CLI fills its defaults on the next launch.
