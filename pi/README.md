# Pi Extensions

Extensions for [Pi coding agent](https://buildwithpi.ai/).

## Install

`./dotfiles-sync --apply` symlinks each `*.ts` from this directory into `~/.pi/agent/extensions/`. Pi auto-discovers extensions there (with `/reload` support) — no `pi install`, no `package.json` manifest. Edits propagate live.

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

### `brave_search` — Brave LLM Context tool

Adds model-callable Brave tools:

- `brave_search` — LLM Context API (`/res/v1/llm/context`) for extracted page snippets and source metadata ready for LLM grounding
- `brave_news_search` — News Search API for recent articles/events
- `brave_image_search` — Image Search API for thumbnails and source pages
- `brave_video_search` — Video Search API for tutorials, clips, and source pages

`brave_search` supports context-budget controls (`maxTokens`, `maxUrls`, snippet/token per-URL limits), `contextThresholdMode`, Goggles, freshness, and explicit local-recall/location parameters.

Set the `BRAVE_SEARCH_API_KEY` environment variable before starting pi (e.g. via your shell profile or a secrets manager):

```bash
export BRAVE_SEARCH_API_KEY="your-brave-search-api-key"
```

Then ask for web/current information normally; the model can call `brave_search` and cite returned URLs. Ask specifically for news, images, or videos when you want the model to use those specialized Brave tools.
