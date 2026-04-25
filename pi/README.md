# Pi Extensions

Extensions for [Pi coding agent](https://buildwithpi.ai/).

## Install

`./stow-all.py --apply` symlinks each `*.ts` from this directory into `~/.pi/agent/extensions/`. Pi auto-discovers extensions there (with `/reload` support) — no `pi install`, no `package.json` manifest. Edits propagate live.

Skills (in the repo-root `skills/` directory) are similarly symlinked into `~/.agents/skills/` and read by pi from there.

Source: cherry-picked from [mitsuhiko/agent-stuff](https://github.com/mitsuhiko/agent-stuff).

## Extensions

### `/answer` — Interactive Q&A

Extracts questions from the last assistant message and presents an interactive TUI to answer them one by one.

1. Agent asks multiple questions in a response
2. Run `/answer`
3. Navigate questions with ↑/↓, type answers in an editor per question
4. Submit all answers at once when done

### `/btw` — Side-chat popover

Quick side-thread for tangential questions without polluting the main conversation.

- `/btw <text>` — ask something immediately in a side-chat
- `/btw` — open an empty side-thread
- Optionally injects a summary back into the main chat on close
