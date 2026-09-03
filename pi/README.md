# Pi Extensions

Extensions for [Pi coding agent](https://buildwithpi.ai/).

## Install

Extensions live at `.pi/agent/extensions/*.ts`, which mirrors their path under
`$HOME`. `./dotfiles-sync --apply` links each `*.ts` file into
`~/.pi/agent/extensions/`. Pi discovers the extensions there and supports
`/reload`, so the package needs no `pi install` step or `package.json` manifest.
Edits propagate through the links. Keep top-level `*.ts` helper files with a
safe no-op default export, such as `_lib.ts`, because pi or pi-meta
may load every top-level file while other extensions import it.

Skills (the repo-root `skills/` package) are similarly linked into `~/.agents/skills/` and read by pi from there.

Some extensions originated from [mitsuhiko/agent-stuff](https://github.com/mitsuhiko/agent-stuff); the local command clones are maintained here.

## Extensions

### `murmur` — agent state, installed not linked

Agent state signalling moved to [murmur](https://github.com/martintrojer/murmur),
which aggregates across machines rather than just this one. Its extension is
**not** in this package: `murmur link pi` writes it to
`~/.pi/agent/extensions/murmur.ts`, pinning an absolute store path as it goes,
so `dotfiles-sync` cannot symlink it and only checks that it is there.

- On `agent_start`: marks the window `working` and records the pid, which is
  what makes crash detection possible.
- On `agent_end`: clears if the pane is focused, otherwise `done`.
- On `session_shutdown`: clears, awaited — without it the last stored row is a
  `working` row whose pid is about to die, and a clean exit reads as a crash.
- Under `mu` (`MU_MANAGED_AGENT=1`) it always clears, and records the agent as
  `driver = orchestrated` so surfaces can hide the crew by default.

Sets the tmux option first and appends to the log second, so the status bar
never waits on a fold. No-ops when murmur is absent or uninitialised.

See [`tmux/README.md` § AI Agent Attention](../tmux/README.md#ai-agent-attention).

### `/loop` — recurring prompt scheduler

This command repeats a prompt on an interval within the current session. It is
based on Claude Code's `/loop`.

- `/loop 5m check if the deploy on staging finished` — run every 5 minutes
- `/loop 30s /some-command` — the prompt can be another slash command
- `/loop summarize new errors` — no interval defaults to 10m
- `/loop list` — show active loops
- `/loop stop` — cancel all loops; `/loop stop 2` cancels loop #2

Intervals accept `s`/`m`/`h` suffixes (bare numbers are minutes), with a 5s floor. Ticks are skipped while the agent is busy or has queued work, so loops never stack up. Loops are session-scoped — they stop on session switch (`/new`, `/resume`, `/fork`) and on quit.

### `/goal` — autonomous work toward a verifiable condition

This command works toward a verifiable end condition without requiring a new
prompt for each turn. It is based on Claude Code's `/goal`. After every turn,
a small, fast model checks the recent transcript and returns yes or no. A no
answer and its reason become the next instruction. A yes answer clears the goal
and returns control to you.

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

### `/namesession` — generated `/resume` title

Uses a small/fast model to generate a short friendly title for the current session, then persists it with pi's built-in session-name mechanism so `/resume` shows it instead of the first prompt. Unnamed persisted sessions are also auto-named just before `/new`, `/resume`, `/fork`, or quit. `/reload` is skipped.

- `/namesession` — generate/replace a title from the current transcript
- `/namesession auth cleanup` — optional hint to steer the title

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
### `refusal` — bypassing a refused check gets nudged

No command, no model. When a gate rejects a bash command and the next attempt at
the **same action** carries a flag that disables that gate, this injects one
fixed note before the following LLM call. It never blocks.

```
$ git commit -m fix          # pre-commit hook rejects
$ git commit -m fix --no-verify
  ⚠ Bypassing a refused check: git commit --no-verify
```

This lives in an extension rather than in the
`verification-before-completion` skill because it has to fire whether or not
the agent cooperates — the agent about to skip the check is the one who would
otherwise have to remember the rule.

Precision is the design, since a false positive teaches you to ignore it:

- A **bare** `--force` is never flagged. Force-pushing your own branch after a
  rebase is normal. Only a bypass flag that is *new since* a refusal of the same
  action fires.
- Flags split by how much context they need. `--no-verify`, `--skip-hooks` and
  friends exist for nothing but skipping a check, so any prior failure of the
  same action arms them — which is what catches a hook that is just `exit 1`
  and prints nothing. `--force`, `-f` and `--ignore-working-copy` have real
  everyday uses, so they need the earlier failure to have actually *read* like a
  gate declining (`pre-commit hook`, `rejected`, `is immutable`, `would be
  reformatted`).
- Gate markers are phrases a gate **emits**, never words a gate is named by. pi
  throws the tool's whole output as the error message, so a bare `hook` would
  match `git help hooks` and arm state nothing refused.
- pi's own `Command aborted` (Esc) and `Command timed out` arrive as errors too.
  Both are ignored.
- Actions are keyed on program + subcommand, so a refused `git push` does not
  arm a nudge for `git commit --no-verify`.
- One nudge per refusal, then the state is cleared — repeats stack and stop
  being read.

Deliberately no fast model in the path: the trigger is two observable facts, not
a judgment. Muse Code routes the equivalent rule through an LLM judge and it
failed open in production — a `git push` recorded `allow:policy` with no judge
invocation at all.

## Tests

`.pi/agent/extensions/tests/lib.test.ts` covers the pure helpers in `_lib.ts` (`parseInterval`, `formatInterval`, `blockText`, `conversationTranscript`, `expandProbe`). `tests/refusal.test.ts` covers `refusal.ts`'s four matchers, weighted toward the negative cases — a legitimate `--force`, `find -name`, and a plain `command not found` must all stay silent. Run them with `make check-ts-tests`, which is part of `make check-all`.

No runner dependency and no `package.json`: node strips the TS types itself, so `node --test` runs the files directly. The `tests/` directory is never linked into `$HOME` — `tests` is in `NAME_PATTERNS` (`_dotfiles_sync/ignore.py`), so pi never sees it as an extension.

Agent state is murmur's, so the tmux side of it is tested there, not here. `tmux/.config/tmux/scripts/test-status-tools` keeps only what this repo still owns: the window label, the `status-ai` render, and the tmux-facing CLIs.
