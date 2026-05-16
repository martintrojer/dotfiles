# Hammerspoon Learning Guide

Companion to [`hammerspoon/README.md`](../hammerspoon/README.md). One Hyper
layer for window resizing, directional focus, app launch / cycle, and a couple
of macOS-specific helpers — all driven by letter mnemonics.

## Mental model

Hammerspoon gives you one Hyper layer for windows, desktops (mostly removed),
app focus, and a small set of launch helpers. The config is explicit enough
that the mnemonics are part of the design.

- Hyper is `Shift`+`Cmd`+`Alt`+`Ctrl`. Every binding starts from that chord.
- Letter mnemonics beat memorizing arbitrary chords.
- Pressing the same key repeatedly often cycles state.
- App focus prefers windows on the *current* Space instead of dragging you
  across desktops.
- `Hyper`+`/` toggles an on-screen help overlay generated from the same
  binding registry the config uses internally.

```quiz
[[questions]]
q = "What is the Hyper chord here?"
options = ["`Shift`+`Alt`", "`Shift`+`Cmd`+`Alt`+`Ctrl`", "`Cmd`+`Ctrl`"]
answer = 1
why = "Every Hammerspoon binding hangs off the four-modifier Hyper chord."

[[questions]]
q = "What does `Hyper`+`/` do?"
options = ["Open Safari", "Toggle the help overlay", "Reload Hammerspoon"]
answer = 1
why = "The config builds a help screen from its binding registry and shows it on `Hyper`+`/`."

[[questions]]
q = "How does app focus behave when possible?"
options = [
  "It prefers windows on the current Space",
  "It always jumps to another desktop",
  "It only launches new windows",
]
answer = 0
why = "The helpers intentionally avoid unnecessary Space switches."
```

## Window layout keys

The strongest part of the config is the repeatable window geometry system:
left/center/right resize cluster, optional top/bottom cycles, and a three-state
center/full cycle.

- `Q` — left band cycles 1/3 → 1/2 → 2/3 width
- `W` — right band cycles 1/3 → 1/2 → 2/3 width
- `E` — top band cycles 1/3 → 1/2 → 2/3 height
- `X` — bottom band cycles 1/3 → 1/2 → 2/3 height
- `R` — center cycle: full → center 90% → tall center

`R` is the workhorse center/full key, kept away from the risky `Ctrl`+`D` miss
that earlier configs used.

```quiz
[[questions]]
q = "Which key cycles the focused window through full → center 90% → tall center?"
options = ["`R`", "`F`", "`S`"]
answer = 0
why = "`R` is the general center/full cycle key now."

[[questions]]
q = "Which key controls the left-edge 1/3 → 1/2 → 2/3 width cycle?"
options = ["`E`", "`Q`", "`X`"]
answer = 1
why = "`Q` is the left-side width cycle in the row-oriented cluster."

[[questions]]
q = "Which key controls the right-edge 1/3 → 1/2 → 2/3 width cycle?"
options = ["`E`", "`Q`", "`W`"]
answer = 2
why = "`W` is the right-side width cycle in the main resize cluster."
```

## Directional focus and desktops

Beyond reshaping windows, the layer mainly uses `H`/`J`/`K`/`L` directional
focus. Earlier attempts at Hyper-driven desktop jumps were dropped as too
unreliable on macOS.

- `Hyper`+`H`/`J`/`K`/`L` — focus the nearest window left/down/up/right
- Uses Hammerspoon's directional window search, not tiling state
- Desktop switching via synthetic Mission Control key events was removed
  because it was too unpredictable behind the Hyper layer
- All window placement goes through a small gap helper, so edges and adjacent
  windows keep a clean uniform margin

```quiz
[[questions]]
q = "What do `Hyper`+`H`/`J`/`K`/`L` do?"
options = [
  "Move the window to another monitor",
  "Focus a window by direction",
  "Resize the current window",
]
answer = 1
why = "The H/J/K/L bindings call directional focus helpers."

[[questions]]
q = "Why are desktop jumps no longer part of the Hyper layer?"
options = [
  "macOS removed Spaces entirely",
  "The Hyper wrappers around Mission Control desktop switching were too unpredictable",
  "H/J/K/L already switch desktops",
]
answer = 1
why = "The repo tried Hyper-driven desktop switching but removed it because the synthetic events were too unreliable."

[[questions]]
q = "Why does the config use a gap helper when placing windows?"
options = [
  "To add visible spacing between windows and screen edges",
  "To disable resize animations",
  "To detect app names",
]
answer = 0
why = "The helper applies a uniform gap to outer edges and shared inner borders."
```

## App launcher mnemonics

App keys are trimmed to a core set that mirrors sway where it makes sense.
Pressing the same app key again while the app is already frontmost cycles
between its windows on the current Space.

- `B` — Browser
- `I` — IDE
- `T` — Terminal: Ghostty
- `Y` — Finder / files
- `M` — Music

`B`/`T`/`I`/`Y`/`M` mirror the sway browser/terminal/IDE/files/music layer
directly.

```quiz
[[questions]]
q = "Which app key is mapped to the terminal?"
options = ["`G`", "`T`", "`PadEnter`"]
answer = 1
why = "`T` reads naturally as Terminal. macOS uses Ghostty (the only macOS terminal in this repo)."

[[questions]]
q = "What usually happens if you press an app key again while that app is already frontmost on the current Space?"
options = [
  "It cycles that app's windows",
  "It closes the app",
  "It always opens a fresh instance",
]
answer = 0
why = "The helpers treat repeated focus as a signal to cycle windows for that app."

[[questions]]
q = "Which key is used for Finder / files?"
options = ["`M`", "`T`", "`Y`"]
answer = 2
why = "`Y` is the files key, matching sway's file launcher muscle memory."
```

## Special-case actions

A handful of bindings exist outside the launcher layer for the cases where
plain "focus the app" isn't enough.

- `Hyper`+`B` — open the Browser, matching the sway launcher layer
- `Hyper`+`Y` — focus Finder; if Finder is already frontmost, create a new
  Finder window on the current Space instead
- `Hyper`+`Return` (and `Hyper`+`PadEnter`) — open a new Ghostty window on
  the current Space using `open -na "Ghostty"`, which creates a fresh window
  without activating an existing Ghostty window on another Space (which
  would swoosh macOS away). App-local new-window behavior, not generic
  Space-jumping focus tricks.

```quiz
[[questions]]
q = "What does `Hyper`+`B` do?"
options = ["Open Browser", "Send tmux prefix Ctrl-b", "Open Bluetooth settings"]
answer = 0
why = "`B` matches the sway launcher layer: Browser on B."

[[questions]]
q = "What is special about `Hyper`+`Y`?"
options = [
  "It always opens Notes",
  "It toggles Night Shift",
  "It focuses Finder, or creates a new Finder window if Finder is already frontmost",
]
answer = 2
why = "`Y` is the Finder/files special case."

[[questions]]
q = "What does `Hyper`+`Return` (or `PadEnter`) do?"
options = [
  "Open a new terminal window on the current Space",
  "Resize the current window",
  "Accept the help overlay",
]
answer = 0
why = "`Return`/`PadEnter` is reserved for creating a fresh Ghostty window on the current Space."
```

## Implementation details that matter

The helper layer explains why the keybindings feel consistent: current-space
filtering, per-window cycle state, and Electron fast paths avoid sluggishness
and surprise context jumps.

- **Current-space filtering** — window enumeration first checks which windows
  belong to the focused Space, then filters app windows against that set.
  Keeps focus and cycling local.
- **Per-window cycle memory** — the 1/3 → 1/2 → 2/3 keys keep cycle state per
  key *and* per focused window id, so each window keeps its own layout cycle
  position independently.
- **Electron app fast path** — Code, Slack, Discord, Teams, Notion, Signal,
  and Spotify skip full window enumeration because AX queries are too slow
  there. The config falls back to faster activation rules for them.

```quiz
[[questions]]
q = "Why does the config keep a skip list for Electron-style apps?"
options = [
  "To force them into fullscreen",
  "To avoid slow full window enumeration",
  "To disable their shortcuts",
]
answer = 1
why = "Those apps are slow to enumerate fully through AX APIs, so the config uses a faster path."

[[questions]]
q = "What is cycle state tracked against?"
options = [
  "Only the key",
  "Only the application name",
  "Both the key and the focused window id",
]
answer = 2
why = "That lets different windows keep independent cycle positions."

[[questions]]
q = "Why filter windows by the focused Space first?"
options = [
  "To avoid jumping to windows on other desktops",
  "To enable wallpaper changes",
  "To make screenshots sharper",
]
answer = 0
why = "The config is intentionally current-Space biased."
```
