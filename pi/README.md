# Pi Extensions

Extensions for [Pi coding agent](https://buildwithpi.ai/). Distributed as part of the dotfiles pi package via the `pi` manifest in the repo-root `package.json`.

## Install

```bash
pi install git:github.com/martintrojer/dotfiles
# or for development:
pi install /path/to/dotfiles
```

Pi reads `pi.extensions: ["./pi/extensions"]` and `pi.skills: ["./skills"]` from the package's `package.json` and loads both at runtime. No copy or symlink — pi reads from the package source path. `pi config` (TUI) lets you toggle individual extensions and skills on/off.

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
