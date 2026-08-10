# Herdr

[herdr](https://herdr.dev) is a terminal workspace manager built for coding
agents: workspaces / tabs / panes like tmux, plus per-pane agent detection with
`idle / working / blocked / done` states, an agent-priority sidebar, and a
socket API (`herdr agent`, `herdr pane`) for driving other agents.

**This package is an evaluation, not a migration.** tmux is still the
substrate ([`../docs/DECISIONS.md`](../docs/DECISIONS.md) § Agent state
awareness). The point of running herdr with real dotfiles is to test that
decision honestly instead of from the outside.

**Verdict so far (2026-08-10): tried it for a day, went back to tmux.** The
config stays here and stays working, because the interesting test has not
been run yet — see [Eval Notes](#eval-notes). Re-entering costs `herdr`, not
a rebuild.

This is a **common-scope package**: it links on both Linux and macOS, so
nothing in it may depend on sway, fuzzel, mako, or Homebrew. On Fedora the
binary comes from mise (`fedora/mise/.config/mise/config.toml`); on macOS
install it yourself (`mise use -g herdr` or herdr's installer). Either way
herdr's own updater is off in the config — two updaters fighting over one
`$PATH` entry is worse than a slightly stale binary.

## Config

`.config/herdr/config.toml` only records settings that differ from herdr's
defaults. Run `herdr --default-config` for the full annotated set, and
`herdr config check` to validate an edit — it reports unknown keys and
unparseable keybindings by name and exits non-zero.

Theming is herdr's own: `[theme] name = "catppuccin"` matches the rest of the
repo, so this file carries no `THEME BEGIN/END` block and `make theme` skips
it. If the built-in drifts from `docs/palette.toml`, override individual
tokens under `[theme.custom]` rather than templating the file.

## Muscle Memory

Same out of the box, nothing to configure: prefix `ctrl+b`, `prefix+c` new tab,
`prefix+n`/`prefix+p` next/prev, `prefix+1..9`, `prefix+x` close pane,
`prefix+z` zoom, `prefix+r` resize mode, `prefix+shift+r` reload config,
`prefix+b` toggle sidebar.

Rebound to match [`../tmux/.tmux.conf`](../tmux/.tmux.conf):

| tmux | herdr default | here |
| ---- | ------------- | ---- |
| `prefix+d` detach | `prefix+q` | `prefix+d` |
| `prefix+%` split right | `prefix+v` | `prefix+%` (and `prefix+v`) |
| `prefix+"` split down | `prefix+minus` | `prefix+"` (and `prefix+minus`) |
| `prefix+,` rename window | `prefix+shift+t` | `prefix+,` |
| `prefix+&` kill window | `prefix+shift+x` | `prefix+&` |
| `prefix+s` session picker | `settings` | `tms` picker (settings → `prefix+shift+s`) |
| `prefix+a` agent picker | — | `next_agent` (see below) |
| `prefix+arrows` focus pane | `prefix+h/j/k/l` | arrows (hjkl still works in navigate mode) |
| `prefix+!` break pane out | — | `prefix+shift+b` |

Two naming traps worth knowing before editing `[keys]`:

- `split_vertical` puts the new pane **to the right** and `split_horizontal`
  puts it **below**. The names describe the divider, not the motion — the
  opposite of the tmux mnemonic. Verified with `herdr pane split --direction`.
- Keys take a string *or* an array, which is how `%` and `v` both reach the
  same action.

The key parser accepts `%` and `"` directly, but has no name for `!` — and
`prefix+shift+1` is not a substitute, because terminals send `!` for that
chord. It parses and then never fires, which `config check` cannot catch. So
break-pane sits on `prefix+shift+b`.

## Selecting Agents

Herdr has no agent picker surface, because the sidebar's **agent panel** is
that view. So `prefix+a` selects *within the panel* rather than opening
anything:

| key | does |
| --- | ---- |
| `prefix+a` | cycle forward through the agent panel |
| `prefix+A` | cycle back |
| `prefix+alt+1..9` | jump straight to agent row 1–9 |

`agent_panel_sort = "priority"` makes row 1 the most urgent agent, so
`prefix+alt+1` is "go to whatever most wants me" — which is what `prefix+a`
meant in tmux.

`focus_agent` is indexed-only. It rejects a bare key (`indexed keybinding must
use 1..9`) and takes a *modifier*, not a leader, so `prefix+a+1..9` does not
parse.

## The Sidebar

`prefix+b` toggles it. It starts collapsed to the narrow `compact` rail,
expanded on demand.

That is this repo's argument from `docs/DECISIONS.md` — a permanent column is
too much rent for information you need a few times an hour — wired up as a
default you can flip in one keystroke. If the always-open sidebar turns out to
earn its width, set `ui.sidebar_start_collapsed = false` and say so in
DECISIONS.md.

Two open questions from living with it:

- **Whether the `compact` rail earns its column.** It is the middle ground —
  agent state stays glanceable without the full panel — but it still costs
  width while carrying little you can act on. `sidebar_collapsed_mode =
  "hidden"` takes that width back to zero and makes `prefix+b` the only way
  in; worth trying if the rail keeps not paying for itself.
- **The panel cannot move.** There is no way to put it on top as a horizontal
  strip; herdr exposes `sidebar_width` / `min` / `max`, `sidebar_start_collapsed`
  and `sidebar_collapsed_mode`, and nothing else. (`sidebar_position`,
  `_placement`, `_side`, `_orientation`, `_location` are all rejected as
  unknown keys, and the
  [config reference](https://herdr.dev/docs/config-reference/) lists no such
  setting.) `tab_bar_position` moves the *tab row* top/bottom, which is a
  different thing. So you cannot trade the column for a row — you can only
  choose whether to pay it.

`agent_panel_sort = "priority"` orders the panel by attention rather than by
space, matching what `prefix+a` does in the tmux setup.

## Not Ported

- **`tmux-fingers-rs`** (`prefix+Tab` hint picking) — no equivalent, no
  workaround.
- **`vim-tmux-navigator`** (`ctrl+h/j/k/l` across nvim splits and panes) — no
  herdr↔nvim protocol, and binding bare ctrl-letters would steal keys from
  pane apps.
- **Status bar scripts** (`status-ai`, `status-ram`, `status-uptime`,
  `status-hostname`, `status-window-label`) — herdr's sidebar covers agent
  state natively and there is no status line to render the rest into.
- **`agent-attention`** — this is the thing herdr replaces. Both are live at
  once right now: our `pi/.pi/agent/extensions/agent-attention.ts` and herdr's
  own `herdr-agent-state.ts` (installed by `herdr integration install pi`).
  Different filenames, no collision, but see below.
- **`prefix+v` clipboard history** — the desktop already owns a clipboard
  shortcut on both platforms, and the picker behind it is OS-specific
  (`clipman`+fuzzel on sway, something else entirely on macOS). A
  common-scope config cannot bind one command for both.
- **Notification sound** — same reason: it would need a per-OS audio backend
  to behave identically. Toasts only (`ui.toast.delivery = "herdr"`).

## Sessions (`tms`)

`prefix+s` opens the same picker as tmux, backed by the same
`~/.config/tmux/tms.toml`: pinned sessions with `startup` and `split`, live
sessions, zoxide, and the `fd` fallback, with the same `^a/^c/^t/^x/^f` mode
keys.

The script is one file at `local-bin/.local/bin/tms` (common scope, on
`$PATH`) with a backend seam: a tms "session" is a tmux session under `$TMUX`
and a herdr **workspace** under `$HERDR_ENV`. `$TMUX` is checked first, so a
tmux running inside a herdr pane still means tmux. `TMS_BACKEND=tmux|herdr`
forces it.

| tms | tmux | herdr |
| --- | ---- | ----- |
| session | session | workspace (label) |
| create + `startup` | `new-session` + `send-keys` | `workspace create --cwd` + `pane run` |
| `split = vertical` | `split-window -h` | `pane split --direction right` |
| `split = horizontal` | `split-window -v` | `pane split --direction down` |
| switch | `switch-client` | `workspace focus` |
| preview | `capture-pane` | `pane read` |

Bindings: `prefix+s` picker, `prefix+g` last session, `prefix+shift+t` session
at the current pane's cwd — the same three as `.tmux.conf`.

Two deliberate differences under herdr:

- **No agent glyphs in picker rows.** Those come from tmux `@agent_state`
  window options; herdr's sidebar owns that signal instead. The scan is
  skipped rather than stapling one multiplexer's state onto the other's rows.
- **`prefix+g` has only one pointer.** tmux offers `client_last_session`;
  herdr has no equivalent, so `last` relies solely on the `LAST_FILE` that
  `tms` writes on every switch.

herdr's own workspace picker is still there on `prefix+shift+w`, and `goto`
moves to `prefix+w` (its `prefix+g` default collides with `tms last`, a
collision `config check` does not report).

## Agent Integrations

`herdr integration install <agent>` writes hook files into each agent's own
config dir — herdr owns those paths, `dotfiles-sync` does not:

| agent | path |
| ----- | ---- |
| pi | `~/.pi/agent/extensions/herdr-agent-state.ts` |
| codex | `~/.codex/herdr-agent-state.sh` |
| opencode | `~/.config/opencode/plugins/herdr-agent-state.js` |

`herdr integration status` lists all supported agents and flags outdated
hooks. These run alongside this repo's own tmux `agent-attention` hooks; each
no-ops when its multiplexer is absent.

## Eval Notes

**Where it landed.** Back on tmux after a day. The sidebar is the reason, but
not for the reason originally given: the compact rail is 4 columns, ~1.5% of
width, so the cost is not the cells. It is that the panel is fixed to the left
edge and cannot become a horizontal strip, while tmux's pills share a status
row that was already being spent. Overhead you have already paid has zero
marginal cost; a new column does not.

**What has not been tested, and is the whole question.** Everything that
justifies an always-present agent panel is a fleet feature — priority sort,
cross-workspace blocked detection, `herdr agent prompt --wait`. At one or two
agents it reports what you already know. The real trial is 4–6 agents across
3+ workspaces, which is also the `mu`-on-tmux vs herdr-agent-CLI comparison.
Until that runs, "unclear what it buys" is accurate, not premature.

Things to decide if the experiment resumes:

- **`mu` is tmux-native.** The orchestration skill spawns tmux panes, and
  `experimental.allow_nested = false` means it cannot run a tmux inside a herdr
  pane. The real comparison is *mu-on-tmux* vs *`herdr agent start/prompt/wait`*,
  not multiplexer vs multiplexer.
- **Two agent-state systems.** Confirm `agent-attention.ts` no-ops cleanly
  outside tmux before crediting or blaming herdr's sidebar for anything.
- **v0.x, one maintainer, AGPL, active churn.** The caveat from DECISIONS.md
  has not expired.

## Agent Skill

`herdr --skill` prints a skill file describing the CLI for agents driving
herdr. It is not installed into `~/.agents/skills/` — read
[`../skills/README.md`](../skills/README.md) § Zen Of These Skills first if
that starts looking tempting.
