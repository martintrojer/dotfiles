# Pi Extensions

Extensions for [Pi coding agent](https://buildwithpi.ai/). Managed by `stow-all.py` which symlinks `pi/extensions/*.ts` into `~/.pi/agent/extensions/`.

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

### `/context` — Context overview

Shows what's currently loaded in the session:

- Active extensions and slash commands
- Loaded skills
- Project context files (AGENTS.md / CLAUDE.md)
- Context window usage (tokens) and session totals (cost)

### `/loop` — Iterative loop

Keeps re-prompting the agent until a condition is met. Three modes:

- **Until tests pass** — runs until the test suite is green
- **Until custom condition** — you describe when to stop
- **Self driven** — agent decides when it's done

The agent gets a `signal_loop_success` tool to break out of the loop.
