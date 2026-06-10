# Pi Extensions

Extensions for [Pi coding agent](https://buildwithpi.ai/).

## Install

`./dotfiles-sync --apply` symlinks each top-level `*.ts` from this directory into `~/.pi/agent/extensions/`. Pi auto-discovers extensions there (with `/reload` support) — no `pi install`, no `package.json` manifest. Edits propagate live. Helper modules should also be top-level `*.ts` files with a safe no-op default export (for example `_lib.ts`), because pi/pi-meta may auto-load every top-level file while other extensions import it.

Skills (in the repo-root `skills/` directory) are similarly symlinked into `~/.agents/skills/` and read by pi from there.

Some extensions originated from [mitsuhiko/agent-stuff](https://github.com/mitsuhiko/agent-stuff); the local command clones are maintained here.

## Extensions

### `agent-attention` — tmux status signaling

Reflects the agent's run state into the surrounding tmux window so a glance at the status bar shows which panes need attention.

- On `agent_start`: marks the window `working` (records the pid via `~/.config/tmux/scripts/agent-attention` for crash reaping).
- On `agent_end`: clears the state if the pane is focused, otherwise marks it `blocked` (waiting on you).
- Under `mu` (`MU_MANAGED_AGENT=1`) it uses lightweight direct tmux calls that survive container reaping.

No-op when not running inside tmux.

### `/loop` — recurring prompt scheduler

Clone of Claude Code's `/loop`. Re-sends a prompt on an interval within the current session, as if you typed it each time.

- `/loop 5m check if the deploy on staging finished` — run every 5 minutes
- `/loop 30s /some-command` — the prompt can be another slash command
- `/loop summarize new errors` — no interval defaults to 10m
- `/loop list` — show active loops
- `/loop stop` — cancel all loops; `/loop stop 2` cancels loop #2

Intervals accept `s`/`m`/`h` suffixes (bare numbers are minutes), with a 5s floor. Ticks are skipped while the agent is busy or has queued work, so loops never stack up. Loops are session-scoped — they stop on session switch (`/new`, `/resume`, `/fork`) and on quit.

### `/goal` — autonomous work toward a verifiable condition

Clone of Claude Code's `/goal`. You state a verifiable end condition; pi keeps taking turns toward it without you prompting each step. After every turn, a small/fast checker model checks the recent transcript and answers one yes/no: is the condition met? "no" feeds the checker's reason (including checker model name) back as the next instruction; "yes" clears the goal and returns control to you.

- `/goal until \`npm test\` exits 0 and tsc --noEmit is clean, max 20 turns`
- `/goal` — show the active goal, turns spent, and the last checker reason
- `/goal clear` — stop the goal (aliases: `stop`, `off`, `reset`, `cancel`)

The checker has no tools, so it can only judge what the agent surfaced in the conversation — make conditions verifiable and have the agent print the evidence (test output, file counts, grep results). A trailing `max N turns` / `stop after N turns` is parsed out as a safety net (default 25). The checker prefers a small/fast model and falls back through ranked candidates (including the current session model). Goals are session-scoped.

**`/goal` vs `/loop`:** `/loop` is timer-driven and re-sends a fixed prompt on an interval; `/goal` is turn-driven and continues until an evaluator confirms a condition. They're kept as separate commands, mirroring Claude Code.

### `/btw` — quick side question, no history pollution

Clone of Claude Code's `/btw` ("by the way"). Opens an interactive, multi-turn, no-tools side thread that sees the current session context, and never writes to the main session history. The inverse of a subagent: full context, no tools.

- `/btw what does calculate_metrics return here?`
- `/btw` — open an empty side-thread and type the first question in the modal

Context handed to the side agent: structured main-session messages from pi's session context (preserving roles/tool-results better than a flattened transcript) **and** the project context the main agent sees — cwd, any custom/appended system prompt, and loaded context files (AGENTS.md / CLAUDE.md). It cannot see the half-written in-flight turn (not yet finalized into the session) and long project context is trimmed.

In the overlay:

- **Enter** with text — ask a follow-up (the side thread keeps its own history for continuity)
- **Enter** on an empty input — paste the **last answer** into the main editor and close
- **Esc** — close without pasting (cancels any in-flight turn)

An animated `⠹ Answering…` indicator shows while a turn runs. The overlay is modal (captures input), so there's only ever one `/btw` at a time. The main agent task keeps running in the background regardless, and the side thread has its own abort signal: pressing Esc to stop the main turn won't cancel `/btw`, and vice versa. Outside the TUI it falls back to a single-shot answer via notification.

Uses the current session model for full context awareness, falling back to any model with configured auth.

### `/answer` — answer the agent's questions interactively

When the agent ends a turn by asking you several questions, `/answer` (or `Ctrl+.`) extracts them and walks you through answering them in a modal, then sends the compiled answers back as one message. A small/fast model extracts the questions as JSON (chosen model shown in loader/notification, with serial fallback on failure); the modal (same look/feel as `/btw` — framing rules, title bar, progress dots) shows one question at a time with a multi-line editor.

- **Enter** — next question (or submit on the last)
- **Shift+Enter** — newline in the answer
- **Tab / ↑↓** — move between questions (↑↓ only when the answer is empty)
- **Esc** — cancel

Extraction prefers a small/fast model, falls back through ranked candidates (including the current session model), and reports the chosen model. TUI-only.

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
