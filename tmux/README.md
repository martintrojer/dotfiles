# Tmux

TPM (the tmux plugin manager) is cloned automatically by
`./dotfiles-sync --apply` into `$HOME/.tmux/plugins/tpm` at a pinned
ref (currently `v3.1.0`; see `_dotfiles_sync/pins.py`). After
the first apply on a fresh machine, install the @plugin entries
listed in `.tmux.conf`:

```
[inside tmux]  prefix + I    # install all @plugin entries
prefix + U    # update them later
prefix + alt + u   # uninstall plugins removed from .tmux.conf
```

`dotfiles-sync` only bootstraps TPM itself; TPM owns the @plugin
lifecycle from there. The trade-off is documented in
[`docs/DECISIONS.md` § Vendoring tmux plugins](../docs/DECISIONS.md).

This setup uses a local Python session launcher (`$HOME/.local/bin/tms`) for the repo-defined session flows in `tmux/.tmux.conf`, such as `prefix + s`, `prefix + g`, and `prefix + T`.

`tms` lives in the [`local-bin/`](../local-bin) package rather than beside the other tmux scripts because the [`herdr/`](../herdr) package drives the same picker against herdr workspaces. It picks a backend from the environment (`$TMUX` wins over `$HERDR_ENV`) and reads the same `~/.config/tmux/tms.toml` either way; see [`herdr/README.md`](../herdr/README.md#sessions-tms). Nothing about the tmux behavior changed.

## Interactive guide

For a walkthrough with quizzes, see
[`../guides/TMUX.md`](../guides/TMUX.md). Run `make serve-guides` from the repo
root to open it in a browser.

## Plugin inventory

This configuration uses TPM to manage navigation, clipboard, picker, and
status-line plugins.

- `tmux-plugins/tpm`: tmux plugin manager. It installs, updates, and loads the rest of the plugins from `.tmux.conf`.
- `tmux-plugins/tmux-yank`: copies from tmux into the system clipboard. Most useful in copy mode and for pushing text out of tmux into the desktop clipboard.
- `tmux-plugins/tmux-cpu`: provides the `#{cpu_percentage}` format used by the native status bar's CPU segment.
- `martintrojer/tmux-fingers-rs`: hint-based picking inside visible pane content, similar to Vimium-style jump labels for paths, URLs, SHAs, numbers, and other matches. This is a Rust port of `Morantron/tmux-fingers`; configuration is the same (`@fingers-*` options), the binary is `tmux-fingers-rs`.
- `sainnhe/tmux-fzf`: fzf-powered tmux management for sessions, windows, panes, bindings, clipboard history, and process actions.
- `christoomey/vim-tmux-navigator`: moves between Neovim splits and tmux panes with the same control-key motions, no prefix.

## Active integrations

From your current `tmux/.tmux.conf`:

- The visible status bar is native tmux formatting using named Catppuccin Mocha palette variables defined near the top of `.tmux.conf`.
- Built-in tmux UI surfaces such as `choose-tree`, menus, popups, and prompts are also styled directly with Catppuccin Mocha hex values instead of the stock tmux colors.
- Vim split to tmux pane movement comes from `christoomey/vim-tmux-navigator`.
- The right side CPU segment comes from `tmux-cpu`.
- Cross-platform RAM usage is provided by `$HOME/.config/tmux/scripts/status-ram`.
- The `agent-attention` integration is not a plugin. It is a local script in this repo that tracks per-window agent state (`working / done / blocked / crashed`) via push events from the pi extension, with a pid-liveness reaper for crash detection. See [`docs/DECISIONS.md` § Agent state awareness](../docs/DECISIONS.md).
- Cross-platform uptime is provided by `$HOME/.config/tmux/scripts/status-uptime`.
- Window labels are derived from the active pane by `$HOME/.config/tmux/scripts/status-window-label`, so vertical-split workflows can switch between labels like `nvim`, `codex`, `π - ...`, or a cwd basename.
- `$HOME/.config/tmux/scripts/status-ai` renders the agent segment and sets the
  `@ai_status` option. It starts with the `nf-md-robot` icon, followed by agents
  grouped into color-coded state runs in urgency order. For up to three agents,
  the run shows one glyph per agent. Larger runs use `<N><glyph>`, such as
  `8▶ 5·` (for example, `!! 8▶ 5·` with higher-priority states first). The
  robot icon replaces a separate `AI` label. `.tmux.conf` reads the value
  through `#{E:@ai_status}` and supplies the box background. The script controls
  the foreground colors because a tmux format conditional cannot change color
  within one value.

## Status bar layout

The current bar keeps the same useful information as before, but without the pill-style Catppuccin theme chrome. It follows the shared language in [`docs/LAYOUT.md`](../docs/LAYOUT.md): filled cells are affordances for place, focus, modal state, or attention.

- Left: a filled session block.
- Center: merged window labels (`number + active-pane label`) with a filled active window, flat inactive windows, and inline agent state (`✗` crashed, `!` blocked, `✓` done, `▶` working, `·` idle agent) / zoom markers.
- Pane, window, and session switches trigger an immediate `refresh-client -S`, so label changes show up right away instead of waiting for the status timer.
- Right: a boxed `PREFIX` segment and a boxed agent segment (robot icon + one glyph per agent, urgency-ordered runs, `<N><glyph>` past three in a state), followed by flatter glyph-based `CPU`, `RAM`, `host`, and `uptime` segments. The agent segment disappears entirely when no agents are running.

## Built-in tmux UI

Native tmux pickers and overlays use the same palette as the status bar instead of the default yellow-accent tmux theme.

Repo-defined bindings in the current `tmux/.tmux.conf`:

- `prefix` + `s`: local `tms` picker popup
- `prefix` + `S`: tmux `choose-tree` session picker, sorted by name
- `prefix` + `g`: switch to last session via `tms`
- `prefix` + `T`: create or switch to a session rooted at the current pane path
- `prefix` + `R`: reload `~/.tmux.conf` (mirrors sway `mod+Shift+r`)
- `prefix` + `r`: cycle active pane width 1/3 → 1/2 → 2/3 (mirrors sway `mod+r`)
- `prefix` + `v`: clipboard history picker (mirrors sway `mod+v`)
- `prefix` + `Tab`: tmux-fingers-rs pick visible matches
- `prefix` + `!`: break the current pane out into a new window
- `prefix` + `M`: move the current pane into the selected window or pane as a split
- `prefix` + `w`: built-in tmux session-window tree picker
- `prefix` + `a`: agent state picker — `◆` marks the current window, the right pane previews event history, `ctrl-a/b/w/d/x` filter by state (see [AI Agent Attention](#ai-agent-attention))
- `prefix` + `Ctrl-g`: cheatsheet popup
- mouse click on the left status session block: opens the tmux session picker
- built-in menus, prompts, and popups use Mocha background/foreground colors with a sky selection highlight

Mental model for pane moving:

- `prefix` + `!`: split current pane away from its window. Current pane becomes a new one-pane window.
- `prefix` + `M`: pick destination in tmux tree, then insert current pane there as a split. Source window loses that pane.
- In short: `!` means "pull this pane out"; `M` means "move this pane into there".

## Session persistence

This config does not save or restore tmux state across reboots. The workflow is intentionally on-the-fly:

- `tms` recreates any project session in two keystrokes (`prefix` + `s`), with pinned sessions from `~/.config/tmux/tms.toml` and optional startup commands.
- `detach-on-destroy off` keeps sessions sticky within a running tmux server, so accidental window closes don't kick you out.
- Neovim's `shada` restores oldfiles, registers, global marks, and command/search history across restarts. Buffer lists and window layouts are **not** persisted — use `<leader>fo` (recent files) or `mini.starter` to re-enter.
- Shell history is global via zsh.
- Agent CLIs (`codex`, `opencode`, `pi`) keep their conversation state in their own session stores, not in tmux pane state.

### `tms` config

Pinned sessions live in `~/.config/tmux/tms.toml`.

Example:

```toml
find_max_depth = 2
preview_command = "eza --all --git --icons --color=always {path}"
live_session_threshold = 5
blacklist = [".cache", ".codex", ".config", ".local", "Library", "tmp"]
noisy_basenames = ["node_modules", "dependencies", "docker", "examples", "m4", "opam", "scripts", "website", "target", "dist", "build", ".git"]

[[sessions]]
name = "dotfiles"
path = "~/dotfiles"
startup = "yazi"
split = "vertical"

[[sessions]]
name = "docs"
path = "~/docs"
startup = "nvim"
split = "vertical"
```

Notes:

- `find_max_depth` and `preview_command` are required.
- `blacklist`, `noisy_basenames`, and `sessions` default to empty when omitted.
- `sessions` are shown first in the picker with a `★` marker.
- sessions with pending `agent-attention` are highlighted with the same subtle yellow-on-surface treatment used elsewhere in the tmux UI.
- per-session `split` is optional.
- valid `split` values are `vertical` and `horizontal`.
- if `split` is omitted, that session starts with a single pane.
- `startup` runs in the original first pane. If that session has a split configured, the extra pane is created afterwards and starts empty.
- `Ctrl-c` filters the picker down to configured sessions only.
- `Ctrl-t` shows live tmux sessions, `Ctrl-x` shows `zoxide`, and `Ctrl-f` runs the fallback `fd` scan.
- `live_session_threshold` (optional, default `0` = off): when the picker is launched in the default `all` view, auto-switch the *initial* view to live tmux sessions if at least this many are running. The `^a/^c/^t/^x/^f` reload binds are unchanged, so `^a` still pulls up the merged view.
- `fzf_exact` (optional bool, default `true`): pass `--exact` to fzf so query tokens match as literal substrings instead of fzf's default scattered-character fuzzy. Prefix a token with `'` to opt back into fuzzy for that one token (e.g. `'dotf`). Set to `false` for classic fuzzy.

## Using tmux-fingers-rs

`tmux-fingers-rs` is a fast hint picker for useful text visible in the current tmux pane, such as URLs, paths, SHAs, numbers, and other tokens. It is a Rust port of [Morantron/tmux-fingers](https://github.com/Morantron/tmux-fingers); behavior and `@fingers-*` configuration options are unchanged, the binary is named `tmux-fingers-rs` so it can coexist with the upstream Crystal `tmux-fingers`.

Configured flow:

- `prefix` + `Tab`: start `tmux-fingers-rs`
- type the shown hint to copy the match to the clipboard and tmux buffer
- type `Shift` + the final hint character to copy and paste immediately into the active pane

First-time install: after TPM clones the plugin (`prefix` + `I`), the wizard pops up. Pick one of:

- **Download prebuilt binary** — fastest, no Rust toolchain needed (Linux x86_64 and Apple Silicon macOS).
- **Install from crates.io** — `cargo install tmux-fingers-rs`.
- **Build locally into `./bin`** — builds in place, the plugin script picks it up.
- **Install from this checkout** — `cargo install --path .`.

If you upgrade the plugin and the installed binary's version no longer matches `Cargo.toml`, the wizard pops up again. Set `@fingers-skip-wizard 1` to suppress this.

Custom patterns carried over from the previous setup:

- email addresses
- `host:port`
- semantic versions
- `D123`-style identifiers
- `T123`-style identifiers

If you copy without the shift-paste action, paste it with normal tmux buffer commands:

- `prefix` + `]`: paste the most recent tmux buffer
- `prefix` + `=`: open the tmux buffer list and choose one to paste

## Using tmux-fzf

`tmux-fzf` is a general fuzzy finder for tmux objects rather than visible pane text.

Default flow:

- `prefix` + `F`: open `tmux-fzf`
- use it to search sessions, windows, panes, key bindings, clipboard buffers, and other tmux actions

Notes:

- `prefix` + `s` opens the local popup-backed `tms` picker script.
- `tms` merges pinned sessions, live tmux sessions, and `zoxide` directories, with a fallback `find` scan on `Ctrl-f`.
- `prefix` + `g` keeps last-session switching on an easy key without colliding with your existing tmux binds.
- `prefix` + `w` remains tmux's standard session-window tree picker.
- `prefix` + `Ctrl-g` moves the cheatsheet off a prime lowercase key.
- This is complementary to `tmux-fingers-rs`: `tmux-fzf` is for tmux state and management, while `tmux-fingers-rs` is for picking text from pane content.
- Your config makes the popup larger than the plugin default with `TMUX_FZF_OPTIONS="-p -w 80% -h 75% -m"`.
- The `-m` flag enables multi-select in pickers that support it.

## AI Agent Attention

Agent state is owned by [murmur](https://github.com/martintrojer/murmur), an
installed tool rather than a script in this package. It replaced the local
`agent-attention` script, which was single-machine by construction: window ids
are machine-local, so it could never answer "is anything blocked on me right
now" across more than one box.

What this package still owns is appearance and keys:

- window glyphs (`✗` crashed, `!` blocked, `✓` done, `▶` working, `·` idle) via
  `@agent_glyphs`, themed from `docs/palette.toml`
- `status-ai`, which renders one glyph per agent in `status-right` as
  urgency-ordered colored runs behind a robot icon, rolling up past three per
  state so a large fleet stays narrow
- the `prefix + a` bind, and the three focus-clear hooks

What murmur owns is behaviour: the event log, the fold, crash detection, and
the picker. The boundary is *tool owns behaviour, dotfiles own appearance and
keys*.

`@agent_state` is the seam, and it has a second consumer: the `tms` session
picker colours its rows from it via `_tmux_common.scan_agent_states`, so a
badge murmur writes shows up as a glyph next to the session name in
`prefix + s` as well as in the status bar. That is also why `murmur clear`
clears the tmux option even for a pane murmur has no event for — a badge left
by anything else would otherwise sit in the picker forever.

Because murmur aggregates across machines, the status bar now paints the whole
fleet. A blocked agent on another host shows up here.

| command | what it does |
| --- | --- |
| `murmur status` | `<state>\t<count>` lines, most urgent first. What `status-ai` parses |
| `murmur pick` | the `prefix + a` popup: filter, glance at the pane, jump local or remote |
| `murmur clear --pane <id>` | clears attention for one pane. What the focus hooks call |

Runtime state lives in murmur's own state dir, not
`~/.local/state/tmux-agent-attention/`.

All three commands resolve on PATH — the *tmux server's* PATH, which is frozen
at the moment the server started. Install murmur while a server is running and
nothing picks it up until `tmux kill-server` (or `tmux setenv -g PATH "$PATH"`),
even though it works fine in your shell. `dotfiles-sync` checks both PATHs and
reports that gap as `UNREACHABLE`.

### Harness support

**pi only.** murmur's extension runs in-process and pushes state directly, so
`working`, `done` and `cleared` are real events rather than inferences. A pid
carried on the `working` event is what makes crash detection possible.

codex and opencode are **not supported**. They had no way to report from inside
themselves, so the old script gave them a `notify` verb that set `blocked` from
outside; murmur has no equivalent and their hooks have been removed. Adding
them back means giving murmur a `notify` path for harnesses that cannot report
their own state.

mu-managed pi panes (`MU_MANAGED_AGENT=1`) clear on `agent_end` rather than
showing `done`, and murmur records them as `driver = orchestrated` so the
picker can hide the crew by default. mu consumes those completions itself, so a
sticky "finished, unseen" badge is noise nobody is expected to acknowledge.

### Setup

```bash
murmur init      # once per machine
murmur link pi   # installs the extension into ~/.pi/agent/extensions/
```

`dotfiles-sync --apply` does not install murmur; it is an npm package, not a
symlink.

## Cheatsheet

`prefix + Ctrl-g` opens an fzf cheatsheet popup inside tmux. The script auto-derives entries from `tmux list-keys -T prefix -N`, so adding `bind -N "label" key cmd` in `.tmux.conf` is enough to make it appear in the picker — no separate cheatsheet edit required. Stock tmux defaults show up with their own one-liners.

The script is Python (`tmux/.config/tmux/scripts/cheatsheet`). Section membership lives in the `SECTION_KEYS` dict near the top; add a key there to put it in a section. The picker shows a curated subset by default — keys mapped into the `Sessions / Windows / Panes / Resize / Copy & Pick / Tools` sections. Useful flags:

- `--all` dumps every binding in the prefix table; uncategorised keys go to a trailing `Other` section.
- `--no-picker` prints the rendered cheatsheet to stdout instead of opening fzf, used by `tmux/.config/tmux/scripts/test-status-tools` to assert formatting.

A small static block in the script lists plugin binds (tmux-fingers-rs, tmux-fzf, TPM) and no-prefix keys (`C-q`, `C-h/j/k/l` via vim-tmux-navigator) since those don't show up as annotated `bind -N` entries. Update `PLUGIN_EXTRAS` / `NO_PREFIX_EXTRAS` when adding or removing plugins.
