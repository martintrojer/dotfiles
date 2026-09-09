# Herdr

[herdr](https://herdr.dev) is a terminal workspace manager for coding agents.
It provides workspaces, tabs, and panes like tmux, plus per-pane detection with
`idle / working / blocked / done` states, an agent-priority sidebar, and a
socket API (`herdr agent`, `herdr pane`) for driving other agents.

**This package is an evaluation, not a migration.** tmux remains the terminal
multiplexer ([`../docs/DECISIONS.md`](../docs/DECISIONS.md) § Agent state
awareness). Running herdr with the working dotfiles tests it under normal use.

**Verdict so far (2026-08-10): tried it for a day, went back to tmux.** The
config remains available because the longer test has not run. See
[Eval notes](#eval-notes). Starting another evaluation requires only the
`herdr` command.

This is a **common-scope package**: it links on both Linux and macOS, so
nothing in it may depend on sway, fuzzel, mako, or Homebrew. On Fedora the
binary comes from mise (`fedora/mise/.config/mise/config.toml`); on macOS
install it yourself (`mise use -g herdr` or herdr's installer). Either way
herdr's updater is disabled so that it cannot conflict with the package manager
that owns the `$PATH` entry.

## Config

`.config/herdr/config.toml` only records settings that differ from herdr's
defaults. Run `herdr --default-config` for the full annotated set, and
`herdr config check` to validate an edit — it reports unknown keys and
unparseable keybindings by name and exits non-zero.

What stays here, and why: updater disabled (mise owns the binary), sidebar
starts collapsed, agent panel sorted by priority, agent labels on pane
borders, no pane gaps/scrollbars, toasts on / sound off, and the three
additive agent-focus keys below. Everything else — including theme
(`catppuccin` is already stock) — is left at upstream default so the eval
tests herdr's own model rather than a tmux dialect.

If the built-in catppuccin drifts from `docs/palette.toml`, override tokens
under `[theme.custom]` rather than templating the file. There is no
`THEME BEGIN/END` block; `make theme` skips this package.

## Keys

Stock herdr, learned with `prefix+?`. The deltas that bite a tmux hand:

| Action | Stock herdr | tmux habit |
| ------ | ----------- | ---------- |
| Detach | `prefix+q` | `prefix+d` |
| Split right / down | `prefix+v` / `prefix+minus` | `prefix+%` / `prefix+"` |
| Focus panes | `prefix+h/j/k/l` | `prefix+arrows` |
| Rename / close tab | `prefix+shift+t` / `prefix+shift+x` | `prefix+,` / `prefix+&` |
| Goto / workspace picker | `prefix+g` / `prefix+w` | (tms / choose-tree) |

`split_vertical` puts the new pane **to the right** and `split_horizontal`
puts it **below** — the names describe the divider, not the motion.

## Select agents

The only deliberate key additions. Stock leaves these unset; the sidebar's
**agent panel** is the picker surface:

| key | does |
| --- | ---- |
| `prefix+a` | cycle forward through the agent panel |
| `prefix+A` | cycle back |
| `prefix+alt+1..9` | jump straight to agent row 1–9 |

`agent_panel_sort = "priority"` makes row 1 the most urgent agent, so
`prefix+alt+1` is "go to whatever most wants me."

`focus_agent` is indexed-only. It rejects a bare key (`indexed keybinding must
use 1..9`) and takes a *modifier*, not a leader, so `prefix+a+1..9` does not
parse.

## The sidebar

`prefix+b` toggles it. It starts collapsed to the narrow `compact` rail,
expanded on demand.

The default follows `docs/DECISIONS.md`: information needed only a few times an
hour does not keep a permanent column. If the sidebar proves useful enough to
stay open, set `ui.sidebar_start_collapsed = false` and record the change in
`DECISIONS.md`.

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
space.

## Missing integrations

- **`tmux-fingers-rs`** (`prefix+Tab` hint picking) — no equivalent, no
  workaround.
- **`vim-tmux-navigator`** (`ctrl+h/j/k/l` across nvim splits and panes) — no
  herdr↔nvim protocol, and binding bare ctrl-letters would steal keys from
  pane apps.
- **Status bar scripts** (`status-ai`, `status-ram`, `status-uptime`,
  `status-hostname`, `status-window-label`) — herdr's sidebar covers agent
  state natively and there is no status line to render the rest into.
- **Agent state** — this is the thing herdr replaces. Both are live at once
  right now: [murmur](https://github.com/martintrojer/murmur)'s pi extension
  (installed by `murmur link pi`) and herdr's own `herdr-agent-state.ts`
  (installed by `herdr integration install pi`). Different filenames, no
  collision, but see below.
- **Clipboard history / notification sound** — OS-specific backends; this
  package is common scope. Toasts only (`ui.toast.delivery = "herdr"`).

## Sessions (`tms`)

`tms` still understands herdr (a tms "session" is a herdr **workspace** under
`$HERDR_ENV`), but it is **not** bound in this config — stock `prefix+s` is
settings and `prefix+g` is goto. Drive it from the shell
(`tms pick-and-connect`, `TMS_BACKEND=herdr` to force) if the eval needs
pinned recipes; otherwise use herdr's own workspace picker (`prefix+w`) and
goto (`prefix+g`).

The script lives at `local-bin/.local/bin/tms`. `$TMUX` is checked before
`$HERDR_ENV`, so a tmux nested somehow still means tmux.

| tms | tmux | herdr |
| --- | ---- | ----- |
| session | session | workspace (label) |
| create + `startup` | `new-session` + `send-keys` | `workspace create --cwd` + `pane run` |
| `split = vertical` | `split-window -h` | `pane split --direction right` |
| `split = horizontal` | `split-window -v` | `pane split --direction down` |
| switch | `switch-client` | `workspace focus` |
| preview | `capture-pane` | `pane read` |

Under herdr there are no agent glyphs in picker rows (sidebar owns that
signal), and `tms last` relies only on the `LAST_FILE` tms writes — herdr has
no `client_last_session` equivalent.

## Agent Integrations

`herdr integration install <agent>` writes hook files into each agent's own
config dir — herdr owns those paths, `dotfiles-sync` does not:

| agent | path |
| ----- | ---- |
| pi | `~/.pi/agent/extensions/herdr-agent-state.ts` |
| codex | `~/.codex/herdr-agent-state.sh` |
| opencode | `~/.config/opencode/plugins/herdr-agent-state.js` |

`herdr integration status` lists all supported agents and flags outdated
hooks. These run alongside murmur's tmux-facing hooks; each no-ops when its
multiplexer is absent.

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
- **Two agent-state systems.** Confirm murmur's pi extension no-ops cleanly
  outside tmux before crediting or blaming herdr's sidebar for anything.
- **v0.x, one maintainer, AGPL, active churn.** The caveat from DECISIONS.md
  has not expired. This eval spanned a release, and pre-1.0 protocol bumps
  mean an old client cannot attach to a new server without restarting it.
- **It fails pillar #11 (human-made, human-owned, heavily tested).** This is
  the larger objection, and the one that took longer to name than the screen
  real estate did. Every other tool this setup rests on — tmux, zsh, sway,
  sqlite — is old, multi-maintainer, distro-packaged and exhaustively tested;
  herdr is none of those yet. The pillar is about maturity and accountability,
  not a claim about how the code was written. A 1.0 with a real test suite and
  more than one maintainer would be a different proposition.

## Agent Skill

`herdr --skill` prints a skill file describing the CLI for agents driving
herdr. It is not installed into `~/.agents/skills/` — read
[`../skills/README.md`](../skills/README.md) § Zen Of These Skills first if
that starts looking tempting.
