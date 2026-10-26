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

Agent state comes from [murmur](https://github.com/mu-crew/murmur), which
aggregates across machines. Its extension is
**not** in this package: `murmur link pi` writes it to
`~/.pi/agent/extensions/murmur.ts`, pinning an absolute store path as it goes,
so `dotfiles-sync` cannot symlink it and only checks that it is there.

The `mu` pi extension (control socket for `mu` panes, plus the `mu_delegate`
tool in unmanaged pi) is installed the same way, by `mu link pi`;
`dotfiles-sync` does not link it, and `mu doctor` checks it.

- On `agent_start`: marks the window `working` and records the pid, which is
  what makes crash detection possible.
- On `agent_end`: clears if the pane is focused, otherwise `done`.
- On `session_shutdown`: clears, awaited — without it the last stored row is a
  `working` row whose pid is about to die, and a clean exit reads as a crash.
- Under `mu` (`MU_MANAGED_AGENT=1`) it always clears, and records the agent as
  `driver = orchestrated` so surfaces can hide the crew by default.

Sets the tmux option first and appends to the log second, so the status bar
never waits on a fold. No-ops when murmur is absent or uninitialised.

See [`tmux/README.md` § AI Agent Attention](../tmux/README.md#ai-agent-attention)
and [`docs/DECISIONS.md`](../docs/DECISIONS.md) § Agent state awareness (tmux +
murmur + mu + mule).

### `/loop` — recurring prompt scheduler

This command repeats a prompt on an interval within the current session. It is
based on Claude Code's `/loop`.

- `/loop 5m check if the deploy on staging finished` — run every 5 minutes
- `/loop 30s /some-command` — the prompt can be another slash command
- `/loop summarize new errors` — no interval defaults to 10m
- `/loop list` — show active loops
- `/loop stop` — cancel all loops; `/loop stop 2` cancels loop #2

Intervals accept `s`/`m`/`h` suffixes (bare numbers are minutes), with a 5s floor. Ticks are skipped while the agent is busy or has queued work, so loops never stack up. Loops are session-scoped — they stop on session switch (`/new`, `/resume`, `/fork`) and on quit.

### `/watch` — `/loop`, but only when a probe's output changes

Same scheduler as `/loop` (interval, busy-skip, session scope, `list`/`stop`). The prompt fires only when a backtick-quoted probe's output changes. The first successful tick records a baseline and does not fire, so arming a watch never spuriously wakes the agent.

- `/watch 3m \`gh-pr 1234\` address new CI failures`
- `/watch 1m \`git-remote\` pull and summarize what landed`
- `/watch 30s \`gh run list -L 5\` check the latest Actions run` — any shell command works; named probes are optional
- `/watch summarize new errors` — no interval defaults to 10m (probe still required)
- `/watch list` — show active watches
- `/watch stop` — cancel all watches; `/watch stop 2` cancels watch #2

Named probes expand to deterministic commands (no relative timestamps or color, which would churn on their own):

| Name | Command | Meaning |
| --- | --- | --- |
| `gh-pr` | `gh pr checks` + optional args (`gh-pr 1234`) | CI checks on a PR (no arg = current branch's PR) |
| `gh-ci` | `gh run list -L 5` | recent GitHub Actions workflow runs |
| `git-remote` | `git ls-remote origin HEAD` | remote HEAD moved |
| `git-status` | `git status --porcelain` | working tree / index changed |
| `git-log` | `git log -1 --format=%H origin/HEAD` | upstream HEAD commit moved |

Non-zero probe exits still count as output (`gh pr checks` is often 1 while CI is pending). Spawn failures and timeouts warn once and keep the watch alive. The injected message appends `--- watch probe output ---` plus the new stdout.

**`/watch` vs `/loop`:** `/loop` re-sends a fixed prompt on every tick. `/watch` is change-driven — use it when the interesting event is "this command's output moved."

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

**`/goal` vs `/loop` vs `/watch`:** `/loop` is timer-driven and re-sends a fixed prompt on an interval; `/watch` is the same timer but only injects when a probe's output changes; `/goal` is turn-driven and continues until an evaluator confirms a condition. `/loop` and `/watch` are kept as separate commands, mirroring Claude Code's `/loop` plus a local change-detecting variant.

### `/answer` — answer the agent's questions interactively

When the agent ends a turn by asking you several questions, `/answer` (or `Ctrl+.`) extracts them and walks you through answering them in a modal, then sends the compiled answers back as one message. A small/fast model extracts the questions as JSON (chosen model shown in loader/notification, with serial fallback on failure); the modal (framing rules, title bar, progress dots) shows one question at a time with a multi-line editor.

- **Enter** — next question (or submit on the last)
- **Shift+Enter** — newline in the answer
- **Tab / ↑↓** — move between questions (↑↓ only when the answer is empty)
- **Esc** — cancel

Extraction prefers a small/fast model, falls back through ranked candidates (including the current session model), and reports the chosen model. TUI-only.

### `/namesession` — generated `/resume` title

Uses a small/fast model to generate a short friendly title for the current session, then persists it with pi's built-in session-name mechanism so `/resume` shows it instead of the first prompt. Unnamed persisted sessions are also auto-named just before `/new`, `/resume`, `/fork`, or quit. `/reload` is skipped.

- `/namesession` — generate/replace a title from the current transcript
- `/namesession auth cleanup` — optional hint to steer the title

### `web_search` — Brave / Tavily / Exa, muxed

Adds one model-callable `web_search` tool (`query`, `count`, `maxTokens`, `domains`, `freshness`) over three vendors, so one vendor's monthly cap no longer kills search. Each call picks a random vendor with a key, tries the others one at a time only if it fails, and never fans out. A 200 with zero results counts as success.

Set any subset of the keys before starting pi; vendors without a key are skipped, and with none the tool is not registered:

```bash
export BRAVE_SEARCH_API_KEY=...   # Brave LLM Context
export TAVILY_API_KEY=...         # 1,000 free searches/month
export EXA_API_KEY=...            # $10 free credit/month
```

Quota errors (Brave/Exa 402, Tavily 432/433) mark the vendor out for 24 h; 429 for 60 s. Marked vendors are tried last, not never. Markers live in `~/.pi/agent/extension-state/web-search.json`, shared by every pi session; delete the file (or one entry) to reset. Each attempt has a 30 s deadline.

Results are treated as untrusted: invisible characters (zero-width, bidi controls, Unicode tag characters) are stripped, and the vendor text sits inside an `<untrusted_web_content>` fence with a "data, not instructions" note; any copy of the marker inside a page is defused so it can't close the fence early. No vendor filters prompt injection (their safe-search flags are adult-content filters), so this is friction, not a barrier.

`domains` takes hostnames or host/path prefixes. Tavily and Exa honour the path; Brave uses `site:` operators, which match the host only.

Usage and failure trends come from the session logs: each result's `details` records `vendor` (absent when every vendor failed) and `attempts`.

```bash
# answering vendor (or all-failed), then each failed attempt as vendor:class
cat ~/.pi/agent/sessions/*/*.jsonl | jq -r 'select(.message.toolName? == "web_search") | .message.details // empty
  | (.vendor // "all-failed"), (.attempts[] | select(.class) | "\(.vendor):\(.class)")' | sort | uniq -c | sort -rn
```

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

`.pi/agent/extensions/tests/lib.test.ts` covers the pure helpers in `_lib.ts` (`parseInterval`, `formatInterval`, `blockText`, `conversationTranscript`, `expandProbe`). `tests/web-search.test.ts` covers `web-search.ts`'s vendor selection, error classification and quota markers, and `tests/web-search-wiring.test.ts` drives the real tool with `fetch` mocked to pin marker persistence and that a broken state directory never aborts failover — the failures another vendor would silently paper over. `tests/refusal.test.ts` covers `refusal.ts`'s four matchers, weighted toward the negative cases — a legitimate `--force`, `find -name`, and a plain `command not found` must all stay silent. Run them with `make check-ts-tests`, which is part of `make check-all`.

No runner dependency and no `package.json`: node strips the TS types itself, so `node --test` runs the files directly. The `tests/` directory is never linked into `$HOME` — `tests` is in `NAME_PATTERNS` (`_dotfiles_sync/ignore.py`), so pi never sees it as an extension.

Agent state is murmur's, so the tmux side of it is tested there, not here. The tmux formats are mu-crew/dotfiles' and tested there. `tmux/.config/tmux/scripts/test-status-tools` keeps only what this repo still owns: the window label, the format-injection audit, the 0% probe segment, and the glyph overrides. The cheatsheet is untested: it fails loudly.
