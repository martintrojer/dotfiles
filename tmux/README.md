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

Two pieces live in their own repos so they can move forward with the tools they serve, and this package pins or installs them:

| Repo | What it gives this config | How it arrives |
| --- | --- | --- |
| [mu-crew/dotfiles](https://github.com/mu-crew/dotfiles) | murmur focus hooks; `prefix` + `a` / `C-m` / `G` / `u`; pane-border format; agent window, pane and pill formats | cloned by `dotfiles-sync --apply` to `~/.local/share/mu-crew-dotfiles` at the ref in [`_dotfiles_sync/pins.py`](../_dotfiles_sync/pins.py); `.tmux.conf` sources `tmux/mu-crew.conf` from it |
| [mu-crew/tmux-session-picker](https://github.com/mu-crew/tmux-session-picker) | `tsesh`: `prefix` + `s` / `g` / `T` | TPM `@plugin`; config in the [`tsesh/`](../tsesh) package |

To move forward: bump `MU_CREW_DOTFILES` in `pins.py` and run `./dotfiles-sync --apply tmux`; update tsesh with `prefix` + `U`. `.tmux.conf` overrides mu-crew's colours and glyphs after sourcing it, from `docs/palette.toml` and `docs/glyphs.toml` (region `tmux-mu-crew-glyphs`).

## Interactive guide

For a walkthrough with quizzes, see
[`../guides/TMUX.md`](../guides/TMUX.md). Run `make serve-guides` from the repo
root to open it in a browser.

## Plugin inventory

This configuration uses TPM to manage navigation, clipboard, picker, and
status-line plugins.

- `tmux-plugins/tpm`: tmux plugin manager. It installs, updates, and loads the rest of the plugins from `.tmux.conf`.
- `tmux-plugins/tmux-yank`: copies from tmux into the system clipboard. Most useful in copy mode and for pushing text out of tmux into the desktop clipboard.
- `martintrojer/tmux-fingers-rs`: hint-based picking inside visible pane content, similar to Vimium-style jump labels for paths, URLs, SHAs, numbers, and other matches. This is a Rust port of `Morantron/tmux-fingers`; configuration is the same (`@fingers-*` options), the binary is `tmux-fingers-rs`.
- `mu-crew/tmux-session-picker`: `tsesh`, the session picker behind `prefix` + `s` / `g` / `T`. See its README for keys and config.
- `sainnhe/tmux-fzf`: fzf-powered tmux management for sessions, windows, panes, bindings, clipboard history, and process actions.
- `christoomey/vim-tmux-navigator`: moves between Neovim splits and tmux panes with the same control-key motions, no prefix.

## Active integrations

From your current `tmux/.tmux.conf`:

- The visible status bar is native tmux formatting using named Catppuccin Mocha palette variables defined near the top of `.tmux.conf`.
- Built-in tmux UI surfaces such as `choose-tree`, menus, popups, and prompts are also styled directly with Catppuccin Mocha hex values instead of the stock tmux colors.
- Vim split to tmux pane movement comes from `christoomey/vim-tmux-navigator`.
- CPU, RAM (a pressure score on macOS) and uptime come from mu-crew's poller: one background loop writes them into tmux options, and the segments are pure formats with warn and high levels.
- Agent state (`working / waiting / done / blocked / error / crashed / idle`) is owned by [murmur](https://github.com/mu-crew/murmur), an installed tool; mu-crew/dotfiles renders it and this package places it. See [AI Agent Attention](#ai-agent-attention).
- Window labels use the active pane's `@murmur_pane_label`, else an editor's command, else a meaningful title (not empty, the hostname, or path-shaped), else the cwd basename for shells or the command otherwise, through the pure tmux format `@win_label`.
- The agent pill in `status-right` is mu-crew's `@mu_crew_pill`: a robot icon, then one coloured `<count><glyph>` run per state, most urgent first, then a dim crew total. It is pure tmux format over murmur's `@murmur_count_*` options (this host) and mu-crew's poller for remote peers, so no process runs on a redraw. `.tmux.conf` supplies the box background and hides the box when no agents exist.

## Status bar layout

The bar follows the shared language in [`docs/LAYOUT.md`](../docs/LAYOUT.md): filled cells are affordances for place, focus, modal state, or attention.

- Left: a filled session block.
- Center: merged window labels (`number + active-pane label`) with a filled active window, flat inactive windows, and inline agent-state / zoom markers. The state glyphs are the `agent_*` keys in [`docs/glyphs.toml`](../docs/glyphs.toml), one each for crashed, error, blocked, done, working, waiting, and idle.
- Nothing in the bar runs a command on a redraw: every segment reads tmux options. murmur and mu-crew's poller repaint when they change them; the 5 s `status-interval` only catches a pane's command or title changing under the window labels.
- Right: a boxed `PREFIX` segment and a boxed agent segment (robot icon + urgency-ordered attention runs + an optional trailing crew total), followed by flatter glyph-based `CPU`, `RAM`, `host`, and `uptime` segments. The agent segment disappears only when neither human agents nor crew exist.

## Built-in tmux UI

Native tmux pickers and overlays use the same palette as the status bar instead of the default yellow-accent tmux theme.

Repo-defined bindings in the current `tmux/.tmux.conf`:

- `prefix` + `s`: `tsesh` session picker popup
- `prefix` + `S`: tmux `choose-tree` session picker, sorted by name
- `prefix` + `g`: switch to last session via `tsesh`
- `prefix` + `T`: create or switch to a session rooted at the current pane path
- `prefix` + `u`: toggle between a workspace session and its `mu-*` workstream sessions. From a workspace it lists `mu-*` sessions; from a `mu-*` session it lists workspaces. The likely counterpart (`hacking/foo` ↔ `mu-foo`, matched on the session basename) comes first, the rest by recent activity
- `prefix` + `R`: reload `~/.tmux.conf` (mirrors sway `mod+Shift+r`)
- `prefix` + `r`: cycle active pane width 1/3 → 1/2 → 2/3 (mirrors sway `mod+r`)
- `prefix` + `v`: clipboard history picker (mirrors sway `mod+v`)
- `prefix` + `Tab`: tmux-fingers-rs pick visible matches
- `prefix` + `!`: break the current pane out into a new window
- `prefix` + `M`: move the current pane into the selected window or pane as a split
- `prefix` + `w`: built-in tmux session-window tree picker
- `prefix` + `a`: agent jump list (`murmur pick`) — type to narrow, enter jumps
  local or remote; `ctrl-a` toggles crew. For a live fleet view run
  `murmur dash` in a pane (see [AI Agent Attention](#ai-agent-attention))
- `prefix` + `Ctrl-g`: cheatsheet popup
- mouse click on the left status session block: opens the tmux session picker
- built-in menus, prompts, and popups use Mocha background/foreground colors with a sky selection highlight

Mental model for pane moving:

- `prefix` + `!`: split current pane away from its window. Current pane becomes a new one-pane window.
- `prefix` + `M`: pick destination in tmux tree, then insert current pane there as a split. Source window loses that pane.
- In short: `!` means "pull this pane out"; `M` means "move this pane into there".

## Session persistence

This config does not save or restore tmux state across reboots. The workflow is intentionally on-the-fly:

- `tsesh` recreates any project session in two keystrokes (`prefix` + `s`), with pinned sessions and optional startup commands from `~/.config/tsesh/config.toml` (the [`tsesh/`](../tsesh) package).
- `detach-on-destroy off` keeps sessions sticky within a running tmux server, so accidental window closes don't kick you out.
- Neovim's `shada` restores oldfiles, registers, global marks, and command/search history across restarts. Buffer lists and window layouts are **not** persisted — use `<leader>fo` (recent files) or `mini.starter` to re-enter.
- Shell history is global via zsh.
- Agent CLIs (`codex`, `opencode`, `pi`) keep their conversation state in their own session stores, not in tmux pane state.

### `tsesh` config

Pinned sessions and scan filters live in [`tsesh/.config/tsesh/config.toml`](../tsesh/.config/tsesh/config.toml). Every key is documented in the [tmux-session-picker README](https://github.com/mu-crew/tmux-session-picker) and its `examples/config.toml`.

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

- `prefix` + `s` opens the `tsesh` picker popup.
- `tsesh` merges pinned sessions, live tmux sessions, and `zoxide` directories, with a fallback `find` scan on `Ctrl-f`.
- `prefix` + `g` keeps last-session switching on an easy key without colliding with your existing tmux binds.
- `prefix` + `w` remains tmux's standard session-window tree picker.
- `prefix` + `Ctrl-g` moves the cheatsheet off a prime lowercase key.
- This is complementary to `tmux-fingers-rs`: `tmux-fzf` is for tmux state and management, while `tmux-fingers-rs` is for picking text from pane content.
- Your config makes the popup larger than the plugin default with `TMUX_FZF_OPTIONS="-p -w 80% -h 75% -m"`.
- The `-m` flag enables multi-select in pickers that support it.

## AI Agent Attention

Agent state is owned by [murmur](https://github.com/mu-crew/murmur), an
installed tool rather than a script in this package, so it can answer "is
anything blocked on me right now" across every machine.

The tmux side of it is [mu-crew/dotfiles](https://github.com/mu-crew/dotfiles): focus hooks that call `murmur clear`, the `prefix` + `a` / `C-m` / `G` / `u` keys, and the window, pane-border and pill formats that read murmur's `@murmur_*` options. This package sources it, places the formats in the status bar, and overrides its colours and glyphs from `docs/palette.toml` and `docs/glyphs.toml`, so murmur state looks the same here as in waybar and zsh.

What murmur owns is behaviour: reported state, peer collect, crash detection, `status` / `pick` / `dash`. The boundary is *tools own behaviour; mu-crew/dotfiles owns the shared tmux wiring; this repo owns placement and theme*. Why tmux + murmur + mu + mule (and not herdr or workmux) is in [`docs/DECISIONS.md`](../docs/DECISIONS.md) § Agent state awareness.

murmur publishes `@murmur_session_state`, `@murmur_window_state`, `@murmur_pane_state`, `@murmur_pane_label`, and the global `@murmur_count_<state>` options; see its ARCHITECTURE.md. The pane label requires a murmur release newer than 0.6.0; the session state is what `tsesh` colours its rows from.

Because murmur aggregates across machines, the status bar paints the whole
fleet. A blocked agent on another host shows up here.

| command | what it does |
| --- | --- |
| `murmur status` | `<state>\t<count>` lines, most urgent first, then optional `crew\t<count>`. mu-crew's remote poller reads the `--json` form |
| `murmur pick` | the `prefix + a` popup: type to narrow, enter jumps, `ctrl-a` toggles crew |
| `murmur dash` | live cards + pane glance; `prefix` + `G` goes back to it |
| `murmur clear --pane <id>` | clears attention for one pane. What the focus hooks call |

Those commands resolve on PATH — the *tmux server's* PATH, which is frozen at
the moment the server started. Install murmur while a server is running and
nothing picks it up until `tmux kill-server` (or `tmux setenv -g PATH "$PATH"`),
even though it works fine in your shell. `dotfiles-sync` checks both PATHs and
reports that gap as `UNREACHABLE`.

### Harness support

**pi** is first-class: murmur's extension runs in-process and pushes activity,
so `running`, `done`, and crash detection are real rather than inferred.

**codex, opencode, and Cursor CLI** use `murmur notify` (attention only — no
ownership or crash detection). This repo wires:

| Harness | Where |
| --- | --- |
| codex | `notify = [...]` in `~/.codex/config.toml` (manual; snippet in mu-crew/dotfiles `codex/config.toml`) |
| opencode | `opencode/.config/opencode/plugin/notify.ts` |
| Cursor CLI | `cursor/.cursor/hooks.json` → `stop` → `murmur notify --source cursor` |

mu-managed pi panes (`MU_MANAGED_AGENT=1`) clear on `agent_end` rather than
showing `done`, and murmur records them as `driver = orchestrated` so pick and
dash hide the crew by default (`ctrl-a` / `--all` reveals them). mu consumes
those completions itself, so a sticky "finished, unseen" badge is noise nobody
is expected to acknowledge.

### Setup

```bash
murmur init      # once per machine
murmur link pi   # installs the extension into ~/.pi/agent/extensions/
```

`dotfiles-sync --apply` does not install murmur; it is an npm package, not a
symlink. Harness hooks, peers, and hard ssh cases:
[murmur docs/setup.md](https://github.com/mu-crew/murmur/blob/main/docs/setup.md)
and [SSH.md](https://github.com/mu-crew/murmur/blob/main/SSH.md).

## Cheatsheet

`prefix + Ctrl-g` opens an fzf cheatsheet popup inside tmux. The script auto-derives entries from `tmux list-keys -T prefix -N`, so adding `bind -N "label" key cmd` in `.tmux.conf` is enough to make it appear in the picker — no separate cheatsheet edit required. Stock tmux defaults show up with their own one-liners.

The script is Python (`tmux/.config/tmux/scripts/cheatsheet`). Section membership lives in the `SECTION_KEYS` dict near the top; add a key there to put it in a section. The picker shows a curated subset by default — keys mapped into the `Sessions / Windows / Panes / Resize / Copy & Pick / Tools` sections. Useful flags:

- `--all` dumps every binding in the prefix table; uncategorised keys go to a trailing `Other` section.
- `--no-picker` prints the rendered cheatsheet to stdout instead of opening fzf, used by `tmux/.config/tmux/scripts/test-status-tools` to assert formatting.

A small static block in the script lists plugin binds (tmux-fingers-rs, tmux-fzf, TPM) and no-prefix keys (`C-q`, `C-h/j/k/l` via vim-tmux-navigator) since those don't show up as annotated `bind -N` entries. Update `PLUGIN_EXTRAS` / `NO_PREFIX_EXTRAS` when adding or removing plugins.
