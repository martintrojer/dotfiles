# Tmux Learning Guide

Companion to [`tmux/README.md`](../tmux/README.md). Repo conventions,
bindings, and the local pickers / status helpers — all rendered by
`tmux/.tmux.conf` and a few small scripts under `tmux/.config/tmux/scripts/`.

## Core model

The setup stays close to stock tmux: one prefix, one server,
sessions/windows/panes, native UI, top status bar, and sensible defaults for
clipboard, mouse, focus events, and history.

- `Ctrl`+`b` stays the prefix
- Windows and panes are 1-indexed
- Status bar lives at the top
- Mouse, OSC52 clipboard passthrough, focus events, and true color all on
- `detach-on-destroy off` keeps you inside the tmux server when windows
  disappear
- **Session** = top-level workspace; **window** = tab; **pane** = split
- The repo prefers rebuilding sessions quickly instead of restoring snapshots
- The status bar, popups, menus, prompts, and choose-tree surfaces are all
  styled directly in `.tmux.conf` with Catppuccin Mocha colors, no theme plugin

```quiz
[[questions]]
q = "What is the tmux prefix in this config?"
options = ["`Ctrl`+`a`", "`Ctrl`+`b`", "`Ctrl`+`Space`"]
answer = 1
why = "The config keeps stock tmux prefix behavior on `Ctrl`+`b`."

[[questions]]
q = "Where does the status bar live?"
options = ["Bottom", "Hidden by default", "Top"]
answer = 2
why = "The status bar is explicitly set to the top."

[[questions]]
q = "What index do windows and panes start at?"
options = ["0", "1", "2"]
answer = 1
why = "Both `base-index` and `pane-base-index` are set to 1."
```

## Session and navigation bindings

The highest-value binds are about getting into the right session quickly and
switching context without opening extra menus by hand.

- `prefix`+`s` — local popup-backed `tms` picker (pinned + live + zoxide + scan)
- `prefix`+`S` — tmux's built-in session tree, sorted by name
- `prefix`+`g` — jump to the last session via `tms`
- `prefix`+`T` — create or switch to a session rooted at the current pane path
- `prefix`+`R` — reload `~/.tmux.conf`
- `prefix`+`w` — tmux's built-in session-window tree picker
- click the left status session block to open the session picker

`tms` is the opinionated repo-specific picker; `choose-tree` is tmux's native
universal picker. Both earn their place.

```quiz
[[questions]]
q = "Which binding opens the local popup-backed `tms` picker?"
options = ["`prefix`+`s`", "`prefix`+`S`", "`prefix`+`F`"]
answer = 0
why = "Lowercase `s` is the repo-defined `tms` picker popup."

[[questions]]
q = "Which binding switches to the last session via `tms`?"
options = ["`prefix`+`g`", "`prefix`+`G`", "`prefix`+`w`"]
answer = 0
why = "The easy last-session key is `prefix`+`g`."

[[questions]]
q = "Which binding reloads `~/.tmux.conf`?"
options = ["`prefix`+`r`", "`prefix`+`R`", "`prefix`+`Ctrl`+`r`"]
answer = 1
why = "Uppercase `R` is reserved for reloading the tmux config (lowercase `r` cycles pane width)."
```

## Pane and window management

Splits inherit the current pane path, arrows mirror sway-style movement, and
two dedicated binds cover the difference between *pulling* a pane out and
*moving* it somewhere else.

- `prefix`+`"` — split vertically (inherits `#{pane_current_path}`)
- `prefix`+`%` — split horizontally (inherits `#{pane_current_path}`)
- `prefix`+`c` — new window (inherits `#{pane_current_path}`)
- `prefix`+`arrow` — focus pane in that direction
- `prefix`+`Shift`+`arrow` — swap pane with the directional neighbor
- `prefix`+`r` — cycle pane width through 1/3 → 1/2 → 2/3
- `prefix`+`!` — pull the current pane out into a new one-pane window
- `prefix`+`M` — choose a target window/pane and insert this pane there as a split
- `Ctrl`+`h` / `Ctrl`+`j` / `Ctrl`+`k` / `Ctrl`+`l` — focus the pane left / down / up / right (no prefix) via `christoomey/vim-tmux-navigator`. The same chord steps between Neovim splits *and* tmux panes seamlessly: hopping out of an `nvim` window into the next tmux pane (or back in) uses one motion, with no prefix and no mental mode-switch.

```quiz
[[questions]]
q = "Which binding moves the current pane into a chosen target window or pane as a split?"
options = ["`prefix`+`!`", "`prefix`+`M`", "`prefix`+`m`"]
answer = 1
why = "Uppercase `M` runs the move-pane picker and inserts the pane into the chosen destination."

[[questions]]
q = "Which binding breaks the current pane out into a new window?"
options = ["`prefix`+`!`", "`prefix`+`w`", "`prefix`+`c`"]
answer = 0
why = "Think of `!` as *pull this pane out*."

[[questions]]
q = "Which binding cycles pane width between 1/3, 1/2, and 2/3?"
options = ["`prefix`+`R`", "`prefix`+`r`", "`prefix`+`|`"]
answer = 1
why = "Lowercase `r` is the resize-cycle binding."

[[questions]]
q = "How do you move focus between a Neovim split and the next tmux pane?"
options = [
  "`prefix`+arrow, then repeat from inside Neovim with `Ctrl`+`w` motions",
  "`Ctrl`+`h/j/k/l` (no prefix) — `vim-tmux-navigator` makes one chord work across nvim splits and tmux panes",
  "You have to leave Neovim first; tmux can't see nvim splits",
]
answer = 1
why = "`christoomey/vim-tmux-navigator` bridges nvim and tmux so `Ctrl`+`h/j/k/l` flows seamlessly into and out of nvim panes without the prefix."
```

## Pickers and overlays

Different pickers solve different problems: text visible in panes, tmux
objects, clipboard buffers, annotated keybindings, and the standard
session/window tree.

- `prefix`+`Tab` — `tmux-fingers-rs` hint picking on visible text
- typing the shown hint copies the match to clipboard and tmux buffer; `Shift`
  on the final hint character copies *and* pastes immediately into the active
  pane (the primary action this config is tuned for)
- `prefix`+`F` — open `tmux-fzf`
- `prefix`+`w` — native session-window tree picker
- `prefix`+`v` — clipboard history
- `prefix`+`Ctrl`+`g` — repo cheatsheet popup. Derived from
  `tmux list-keys -T prefix -N`, so any new named binding shows up automatically.

```quiz
[[questions]]
q = "Which binding starts `tmux-fingers-rs` on visible pane content?"
options = ["`prefix`+`Tab`", "`prefix`+`F`", "`prefix`+`f`"]
answer = 0
why = "`Tab` launches `tmux-fingers-rs`; type the hint to copy, or `Shift`+hint to copy and paste back into the active pane."

[[questions]]
q = "Which binding opens `tmux-fzf`?"
options = ["`prefix`+`f`", "`prefix`+`F`", "`prefix`+`w`"]
answer = 1
why = "Uppercase `F` is reserved for `tmux-fzf`."

[[questions]]
q = "Which binding opens the repo cheatsheet popup?"
options = ["`prefix`+`Ctrl`+`g`", "`prefix`+`g`", "`prefix`+`?`"]
answer = 0
why = "The cheatsheet lives on `prefix`+`Ctrl`+`g` to avoid colliding with more common keys."
```

## Copy mode

The in-tmux text workspace: scroll, search, select, copy. `mode-keys vi`
is set, so vim muscle memory carries over.

- mouse scroll up enters copy mode (because `mouse on`); `prefix`+`[` is the keyboard equivalent; `q` or `Escape` to exit
- `h` `j` `k` `l` step; `w` `b` `e` word; `{` `}` paragraph
- `Ctrl`+`u` / `Ctrl`+`d` half page; `Ctrl`+`b` / `Ctrl`+`f` full page
- `g` / `G` top / bottom of scrollback; `H` `M` `L` top/middle/bottom of viewport
- `/foo` forward search, `?foo` backward, `n` / `N` to repeat
- `v` begin selection; `Ctrl`+`v` toggle rectangle; `y` copy and exit
- `Alt`+`Up` / `Alt`+`Down` jump to previous / next shell prompt (needs OSC 133 markers; emitted by `zsh/.zsh/tools.zsh`)
- copies land in the system clipboard via `tmux-yank` + `set-clipboard on`
- inspect every binding with `tmux list-keys -T copy-mode-vi`

```quiz
[[questions]]
q = "How do you enter copy mode?"
options = [
  "Only with `prefix`+`[`",
  "Either `prefix`+`[` or scrolling up with the mouse",
  "Only by binding it manually first",
]
answer = 1
why = "`mouse on` makes scroll-up enter copy mode automatically; `prefix`+`[` is the keyboard equivalent."

[[questions]]
q = "What do `Ctrl`+`u` and `Ctrl`+`d` do in copy mode?"
options = [
  "Undo and redo the last selection",
  "Half page up and half page down",
  "Switch to the previous and next session",
]
answer = 1
why = "They're the standard vi half-page motions, available because `mode-keys vi` is set."

[[questions]]
q = "What do `Alt`+`Up` and `Alt`+`Down` do in copy mode?"
options = [
  "Move the cursor one line up or down",
  "Jump to the previous or next shell prompt (when OSC 133 markers are emitted)",
  "Resize the current pane vertically",
]
answer = 1
why = "They navigate scrollback by command boundary instead of by lines, complementing `Ctrl`+`u`/`Ctrl`+`d` scrolling."
```

## Status bar and agent attention

The bar tries to surface only the high-signal state: current session,
active-pane-derived window labels, prefix state, AI attention, CPU, RAM, host,
and uptime.

- **Left:** filled session block
- **Center:** merged window labels derived from the active pane
- **Right:** boxed `PREFIX` and `AI` segments, then CPU, RAM, host, uptime
- agent-attention marks windows with pending attention as `[!]`
- shows a boxed `AI <count>` segment in the status bar
- `prefix`+`A` opens a picker listing flagged windows
- attention clears when you focus the actual agent pane
- pending attention lives in `$XDG_STATE_HOME/tmux-agent-attention/pending.jsonl`;
  the script can fire desktop notifications and OSC 777 terminal notifications
  when an agent finishes and needs attention

```quiz
[[questions]]
q = "Which binding opens the agent-attention picker?"
options = ["`prefix`+`a`", "`prefix`+`A`", "`prefix`+`!`"]
answer = 1
why = "Uppercase `A` opens the list of flagged windows."

[[questions]]
q = "What does a window marker of `[!]` mean?"
options = [
  "The pane is zoomed",
  "TPM needs an update",
  "An agent in that window needs attention",
]
answer = 2
why = "The marker means the window has queued agent attention."

[[questions]]
q = "Where is pending attention stored by default?"
options = [
  "`~/.tmux/pending.json`",
  "`~/.local/state/tmux-agent-attention/pending.jsonl`",
  "`~/.cache/tmux/attention.log`",
]
answer = 1
why = "The runtime queue is a JSONL file under XDG state."
```

## Persistence, tms config, and TPM

The repo avoids full session restore. Instead, it optimizes recreation:
pinned sessions in `tms.toml`, quick attach/create flows, and TPM for plugin
lifecycle.

- No save/restore plugin for tmux layout state
- `tms` recreates sessions quickly; Neovim shada, zsh history, and agent-native
  session stores cover the state that actually matters
- pinned sessions live in `~/.config/tmux/tms.toml`
- inside the `tms` picker: `Ctrl`+`c` filters to configured sessions only;
  `Ctrl`+`t` shows live tmux sessions; `Ctrl`+`x` shows zoxide entries;
  `Ctrl`+`f` runs the fallback `fd` scan
- `./dotfiles-sync --apply` only clones TPM itself at a pinned ref
- `prefix`+`I` — install plugin entries (TPM)
- `prefix`+`U` — update plugins
- `prefix`+`Alt`+`u` — uninstall removed plugins

```quiz
[[questions]]
q = "Where do pinned `tms` sessions live?"
options = [
  "`~/.tmux/tms.conf`",
  "`~/.config/tmux/tms.toml`",
  "`~/.config/tms/config.toml`",
]
answer = 1
why = "Pinned session definitions are stored in the tmux package at `tms.toml`."

[[questions]]
q = "Which key installs all TPM plugin entries after a fresh setup?"
options = ["`prefix`+`I`", "`prefix`+`P`", "`prefix`+`U`"]
answer = 0
why = "TPM uses `prefix`+`I` for install."

[[questions]]
q = "Which statement matches the repo's tmux philosophy?"
options = [
  "Restore every pane layout after reboot",
  "Keep sessions disposable and recreate them cheaply",
  "Disable shell history and rely on tmux scrollback",
]
answer = 1
why = "The setup prefers disposable sessions and fast recreation over magic restoration."
```
