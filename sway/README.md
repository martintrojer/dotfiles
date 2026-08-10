# Sway

This package provides the main Wayland compositor setup. The emphasis is on a
small, mainstream stack (sway + waybar + fuzzel + kanshi + mako + swayidle +
swaylock) that stays close to upstream defaults so it can be reasoned about
without surprises.

> **New to sway?** [Sway School](https://martintrojer.github.io/sway-school/) is
> a tree-first tutorial that explains the i3/sway layout model from the ground
> up — the conceptual prerequisite for understanding why this config is shaped
> the way it is.

## Files

- `~/.config/sway/config` — main config (Mod4, Catppuccin colors, app launchers,
  workspaces, layout primitives, media keys, screenshots).
- `~/.config/sway/scripts/` — session, screenshot, lock, app launcher,
  `preset-width`, and `window-back-and-forth` helpers.
- Windowing: tiling with splits, tabbed layouts, floating mode, scratchpad-ready
  primitives, and six numbered workspaces.

The hotkey overlay (`mod+F1`) lives under `fuzzel/` because it's a fuzzel
picker, not a sway script — see `fuzzel/README.md`. `F1` mirrors
hammerspoon's `Hyper+F1` so help is the same key on both OSes.

## Mental Map

The keybindings are intentionally aligned with `tmux/.tmux.conf` so the same
verb does the same thing whether you're inside tmux or on the bare desktop.

| Verb                          | Sway              | Tmux            |
| ----------------------------- | ----------------- | --------------- |
| Cycle preset sizes              | `mod+r`           | `prefix+r`      |
| Reload config                 | `mod+Shift+r`     | `prefix+R`      |
| Clipboard history picker      | `mod+v`           | `prefix+v`      |
| Fullscreen / zoom             | `mod+f`           | `prefix+z`      |
| Last session/workspace        | `mod+g`           | `prefix+g`      |
| Last window (same workspace)  | `mod+Shift+g`     | `prefix+l`      |
| Focus left/down/up/right      | `mod+h/j/k/l`     | `C-h/j/k/l`     |
| Move element left/down/up/right | `mod+Shift+h/j/k/l` | (n/a, panes don't move that way) |
| Primary "switcher" picker     | `mod+Tab` (windows) | `prefix+s` (sessions) |

Modifier conventions inside sway:

- `mod+<letter>` — launch an app (`b` browser, `y` yazi (TUI files), `m` cider,
  `e` emoji). Terminal is `mod+Return` (mirrors `Hyper+Return` in
  `hammerspoon/` so the verb is identical on Linux and macOS). GUI file
  managers (thunar etc.) deliberately don't get a bind — reach via
  `mod+space` fuzzel on the rare occasion.

  Singleton vs spawn behavior differs per app; second press of the
  same bind does different things depending on which mechanism is in
  play. Cheat sheet:

  | Bind          | App            | Singleton via                                   | Re-press effect          |
  | ------------- | -------------- | ----------------------------------------------- | ------------------------ |
  | `mod+Return`  | foot           | `footclient` → `sway-foot-server.service`       | new window               |
  | `mod+b`       | chrome         | `--user-data-dir` profile (own logic)           | new window               |
  | `mod+y`       | yazi           | none — fresh foot + fresh yazi each time        | new yazi window          |
  | `mod+m`       | Cider          | `raise-window` script + scratchpad rule         | focus existing / spawn   |
  | `mod+Shift+m` | Cider controls | fuzzel + playerctl/MPRIS                        | media action picker      |
  | `mod+Shift+a` | Audio mixer    | `wiremix` in a centered floating terminal       | new mixer window         |
  | `mod+Shift+b` | Bluetooth TUI  | `bluetui` in a centered floating terminal       | new Bluetooth TUI        |
  | `mod+Shift+w` | Network TUI    | `nmtui` in a centered floating terminal         | new NetworkManager TUI   |
  | `mod+e`       | bemoji         | none (single-shot picker)                       | n/a                      |

  `raise-window` is fire-and-forget on cold spawn — the very first
  press launches and returns immediately; second press focuses once
  the window has appeared. See TODO #14 for the deferred fix.
- `mod+Shift+<letter>` — destructive / friction-needed actions (`Shift+q`
  kill, `Shift+p` powermenu, `Shift+r` reload-config, `Shift+n`
  send-to-scratchpad, `Shift+space` floating-toggle). `Shift+a` summons the
  audio mixer, `Shift+b` summons the Bluetooth TUI, `Shift+w` summons the
  NetworkManager TUI, and `Shift+m` opens Cider controls; these stay on Shift
  because their base keys are app launchers. Non-destructive pickers (toolboxes,
  ssh, hotkeys) used to live on this layer but moved to base `mod+<letter>`
  slots; only `Shift+p` (powermenu) stays here because lock/suspend/shutdown
  earn the friction. Lock is reached via `mod+Shift+p` powermenu — no dedicated
  key.
- `mod+Shift+<motion>` — move container (`Shift+h/j/k/l`, `Shift+arrows`).
- `mod+Ctrl+<motion>` — workspace-level move (`Ctrl+PgUp/Dn` move container +
  follow). Move-workspace-to-other-output is `mod+Shift+u` (left) / `mod+Shift+i`
  (right) — letter-based only.
- `mod+1..6` — workspaces (`workspace_auto_back_and_forth` makes the same key
  toggle back to the previous workspace, complementing `mod+g`).
- `mod+g` / `mod+Shift+g` — back to the last workspace / the last window on
  this workspace. Same verb at two scopes: `g` is "back where I was", Shift
  narrows it. `mod+g` crosses workspaces, `mod+Shift+g` never leaves one. See
  the layout section for what the window half has to do to work.

Layout containers:

- `mod+[` / `mod+]` — split vertical / horizontal.
- `mod+.` — toggle between tabbed and split layout for the current container.
- `mod+f` / `mod+Shift+space` — fullscreen / floating toggle.
- `mod+c` / `mod+Ctrl+c` — focus parent / child container.
- `mod+r` cycles preset sizes via `~/.config/sway/scripts/preset-width`.
  **Tiled:** width through `33 → 50 → 67 ppt`. **Floating:** `center50 →
  center90 → tallCenter` (50×60%, 90×90%, then 70% wide × full height, all
  centered), mirroring hammerspoon's Hyper+R minus full. Position is absolute
  within the workspace; the cycle index persists in `$XDG_STATE_HOME/sway/`,
  separately per layer. Its tree walk and floating detection both shipped
  crashes on an ordinary keypress, hence the tests in
  `~/.config/sway/scripts/tests/` (`make check-desktop-tests`).
- `mod+-` / `mod+=` — shrink / grow width by 50 px; `Shift+-` / `Shift+=` for
  height (sway clamps on tiled windows). Resize *mode* is intentionally not
  bound — the preset cycle plus these steppers cover the workflow.
- `mod+Shift+g` — `workspace back_and_forth` one level in: swap to the last
  window on this workspace (`~/.config/sway/scripts/window-back-and-forth`,
  named for the sway command it completes). Reads sway's own per-workspace
  focus history, inventing no ordering and keeping no state. Floats are why
  it's bound — directional focus doesn't work between them, so a browser plus
  one popped-in window otherwise has no one-key swap — but history ignores the
  tiled/floating barrier the focus verbs are organised *around*, so it toggles
  across that too. `mod+a` stays the deliberate crossing.

  Nothing in the focus family reads history, measured on 1.11 against three
  windows whose history order differed from their layout order (with two you
  can't tell a toggle from a cycle): `focus prev`/`next` walk the layout and
  dead-end at the edge, `focus child` no-ops when flat, `focus
  tiling`/`floating` recall only their own layer, `focus mode_toggle` crosses
  layers. Marks fail too — `mark`/`unmark` act on the *focused* window, so the
  bookkeeping clobbers the mark it just set.

  One trap, covered by tests since a wrong answer is silent: a `focus` entry
  can name a split container rather than a window, so the lookup descends
  `focus[0]` to a leaf and skips stale ids left by closed windows.

Pickers (all under `fuzzel/.config/fuzzel/scripts/`):

- `mod+space` — fuzzel app launcher (default `--prompt 'Run '`).
- `mod+Tab` — `windows`, swaymsg-driven window switcher. Sorted by
  most-recently-used with the currently focused window pushed to the bottom,
  so `mod+Tab Enter` toggles back to the previous window in two keystrokes
  (Alt-Tab style). Each row starts with `·` (other windows) or `•` (the
  currently focused window, which is pushed to the bottom).
- `mod+v` — `clipboard`, clipman history.
- `mod+e` — `emoji`, bemoji-backed.
- `mod+\` — `calc`, qalc/bc-backed.
- `mod+s` — `ssh`, ssh-config host picker.
- `mod+t` — `toolboxes`, toolbox/distrobox container picker.
- `mod+Shift+a` — `wiremix`, centered floating PipeWire mixer for changing
  volume, per-app levels, and input/output devices.
- `mod+Shift+b` — `bluetui`, centered floating Bluetooth TUI.
- `mod+Shift+w` — `nmtui`, centered floating NetworkManager TUI.
- `mod+Shift+m` — `cider`, small Cider media-control picker. Kept on Shift because `mod+m` summons Cider itself.
- `mod+Shift+p` — `powermenu` (lock / suspend / logout / reboot / shutdown).
  Kept on Shift because it's destructive (suspend / shutdown).
- `mod+grave` — `chrome-tabs`, DevTools-protocol tab switcher (paired
  visually with `mod+Tab` window switcher — grave and Tab sit adjacent
  on the keyboard).
- `mod+F1` — `hotkeys`, parses this file's bindings and dispatches the
  chosen action via `swaymsg`. F1 mirrors hammerspoon's `Hyper+F1`.

## Session Model

The compositor is started by your display manager / TTY launcher. Once running,
sway exec's `~/.config/sway/scripts/session-start`, which:

1. Imports `DISPLAY`, `WAYLAND_DISPLAY`, `XDG_CURRENT_DESKTOP`,
   `XDG_SESSION_TYPE`, and `SWAYSOCK` into systemd's user environment so units
   started later see the live Wayland session.
2. Updates dbus's activation environment with the same variables.
3. Starts `sway-session.target`.
4. Asks `kanshictl reload` to re-assert output profiles, ignoring failure.

Step 4 exists because sway runs this script via `exec_always`, so it also runs
on `swaymsg reload` — and since the config declares no `output` lines (kanshi
owns them), a reload resets every output to scale 1.0. On a cold start kanshi
isn't listening yet, `kanshictl` fails harmlessly, and kanshi applies its own
profile when it starts moments later.

`sway-session.target` declares `Wants=` for the desktop daemons, so each one
gets started exactly once when the session comes up:

| Unit                              | Daemon                              |
| --------------------------------- | ----------------------------------- |
| `sway-clipman-watcher.service`    | `wl-paste --watch clipman store`    |
| `sway-kanshi.service`             | `kanshi` (output profiles)          |
| `sway-mako.service`               | `mako` (notifications)              |
| `sway-waybar.service`             | `waybar` (status bar)               |
| `swaybg.service`                  | wallpaper via `session-wallpaper`   |
| `swayidle.service`                | idle/lock/suspend via `session-swayidle` |

Vendor units that would otherwise be easy to start in parallel with these
session-owned services are masked by `dotfiles-sync --apply`: `mako.service`,
`waybar.service`, `kanshi.service`, `foot-server.service`, and
`foot-server.socket`. That keeps the Sway session uniformly owned by the
`sway-*` units while still letting `sway-mako.service` use mako's D-Bus-aware
`Type=dbus`/`BusName=org.freedesktop.Notifications` semantics.

Naming rule: daemons whose binary already starts with `sway` (`swaybg`,
`swayidle`) keep their bare name; others are prefixed (`sway-kanshi`,
`sway-mako`, etc.) to make `systemctl --user list-units 'sway*'` show the full
session in one place.

Idle behavior lives in `~/.config/sway/scripts/session-swayidle` because
monitor power commands are compositor-specific
(`swaymsg "output * power off"`).

The idle ladder is dim (300s) → lock (330s) → displays off (350s) → suspend
(3600s).

### Caffeinate

The suspend step — and only that step — is skipped while the caffeinate flag
exists. Dim, lock, and display-off still fire on schedule, so caffeinate keeps
a long job alive without leaving the screen unlocked.

Toggle it from the fuzzel powermenu (`mod+Shift+e` → `Caffeinate`), which also
reports the current state in its label, or by clicking the waybar glyph. While
active, waybar shows `󰅶` in the centre; see
[`waybar/README.md`](../waybar/README.md#caffeinate).

The flag is `$XDG_RUNTIME_DIR/caffeinate`, deliberately not under
`$XDG_STATE_HOME`: that directory is a `mode=700` tmpfs which logind destroys
when your last session ends, so **reboot, crash, and logout all clear it** with
no cleanup code to get wrong. A persistent flag could survive a crash and
silently leave suspend disabled for days.

It is a guard inside the timeout rather than `systemd-inhibit --what=sleep`,
which would block every sleep path (lid switch, logind's own `IdleAction`, an
explicit `systemctl suspend`) and would need a live process to hold the lock.
The ask was one timeout.

`Ctrl+Alt+Delete` runs `~/.config/sway/scripts/session-quit`, which stops
`sway-session.target` and then `swaymsg exit` — leaving systemd cleanly
shutting down the daemons before the compositor itself terminates.

## Lock Screen

`~/.config/sway/scripts/lock-screen` is a small wrapper around
`swaylock`. Image work — blur and cache management — lives in the
[`wallpaper`](../fedora/bin/.local/bin/wallpaper) helper. The
lock-screen script just:

1. Calls `wallpaper status`, which returns JSON like
   `{"path": "...", "lock_image": "..."}`.
2. Builds `swaylock --image <lock_image> --scaling fill <passthrough-args>`.
3. exec's into `swaylock`.

Locking must always succeed, so there's a fallback chain:

- `wallpaper` helper missing or status JSON malformed → solid Catppuccin
  base color (`swaylock --color 1e1e2e`).
- `lock_image` missing (e.g. magick not installed when the wallpaper
  was set) but `path` present → raw wallpaper, no blur.
- `path` also missing → solid color again.

Swaylock chrome colors (ring, key-hl, state colors) come from
`swaylock/.config/swaylock/config`, keeping the lock UI theme-coherent
across wallpapers.

Unknown flags pass through to swaylock verbatim (`parse_known_args`),
which is why `session-swayidle` can keep calling `lock-screen --daemonize`.

### Lock image rendering (lives in `wallpaper`)

The rendering recipe (downscaled blurred wallpaper) lives in
`fedora/bin/.local/bin/wallpaper`. It runs:

- as part of `wallpaper set <url-or-file>` and `wallpaper use [archive-file]`,
  so the next lock after a wallpaper change is instant; or
- via `wallpaper rebuild-cache` to force a fresh render (used after
  editing the render constants in `wallpaper`).

The blur (sigma 8) and lock render cap (2560×1440) are constants at
the top of the script. Change them and bump `RENDER_VERSION` next to
the constants — that invalidates all cached entries automatically.

Results are cached under `$XDG_CACHE_HOME/wallpaper/<sha1(path+mtime+RENDER_VERSION)>.{png,sixel-WxH}`.
The warm path (cache hit during `wallpaper status`) is essentially
instant. Cold render is ~3–6 s depending on wallpaper size. Cache
entries are kept for the lifetime of the source archive entry — when
you delete a wallpaper via the fzf picker, every `<key>.*` artifact
is cleaned up alongside it. There is no time-based pruning, so a
wallpaper kept for years keeps its warm lock-screen cache for years.
The `sixel-WxH` siblings are per-pane-size thumbnails for the fzf
picker preview pane (see `wallpaper preview`): first paint pays the
full ImageMagick cost, subsequent cursor moves over the same entry
at the same pane size are a bytes-to-stdout pass.

## Screenshots

`~/.config/sway/scripts/screenshot` writes PNGs to `~/Pictures/Screenshots/`
named `Screenshot from YYYY-MM-DD HH-MM-SS.png`. Modes:

- `mod+F5` — `all` (full output set, `grim`)
- `mod+Ctrl+F5` — `output` (currently focused output, `grim -o`)
- `mod+Alt+F5` — `region` (drag-select via `slurp`)

## Outputs

Output configuration is delegated to `kanshi`. Sway's config never touches
`output` directly. To change a profile, edit `~/.config/kanshi/config` and
`systemctl --user reload-or-restart sway-kanshi.service`.

Because sway owns no `output` lines, `swaymsg reload` drops the panel back to
scale 1.0 on its own. `session-start` re-runs `kanshictl reload` to undo that;
if scaling ever looks wrong after a reload, `kanshictl reload` is the fix.

## Theme

Catppuccin Mocha hex values are inlined at the top of `~/.config/sway/config`.
See `docs/THEME.md` for the full palette and where it's mirrored across
other tools (waybar, mako, foot, swaylock, etc.).
