# Cursor

User-level Cursor Agent hooks for [murmur](https://github.com/martintrojer/murmur)
attention.

## What this package links

| Path | Role |
| --- | --- |
| `~/.cursor/hooks.json` | `stop` → `murmur notify --source cursor` |

Cursor writes the stop payload on stdin (`hook_event_name` + `status`). murmur
maps `completed` → `done` and `aborted`/`error` → `blocked`. This is attention
only — no pane ownership or crash detection (see murmur's README).

## Requirements

- murmur on PATH for the process that launches `agent` (usually your terminal)
- agent running inside tmux (`$TMUX_PANE`)
- interactive `agent` sessions; non-interactive `agent -p` may omit `stop`

## Verify

```bash
echo '{"hook_event_name":"stop","status":"completed"}' | murmur notify --source cursor
murmur status
murmur clear --pane "$TMUX_PANE"
```

`./dotfiles-sync --check` fails if `~/.cursor/hooks.json` exists and its `stop`
hooks do not call murmur.
