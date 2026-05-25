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
- `~/.config/sway/scripts/` — session, screenshot, lock, app launcher, and
  preset-width helpers.
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
| Cycle 1/3 → 1/2 → 2/3 width   | `mod+r`           | `prefix+r`      |
| Reload config                 | `mod+Shift+r`     | `prefix+R`      |
| Clipboard history picker      | `mod+v`           | `prefix+v`      |
| Fullscreen / zoom             | `mod+f`           | `prefix+z`      |
| Last session/workspace        | `mod+g`           | `prefix+g`      |
| Focus left/down/up/right      | `mod+h/j/k/l`     | `C-h/j/k/l`     |
| Move element left/down/up/right | `mod+Shift+h/j/k/l` | (n/a, panes don't move that way) |
| Primary "switcher" picker     | `mod+Tab` (windows) | `prefix+s` (sessions) |

Modifier conventions inside sway:

- `mod+<letter>` — launch an app (`b` browser, `y` yazi (TUI files), `m` cider,
  `i` vscode, `e` emoji). Terminal is `mod+Return` (mirrors `Hyper+Return` in
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
  | `mod+i`       | VS Code        | `raise-window` script                           | focus existing / spawn   |
  | `mod+e`       | bemoji         | none (single-shot picker)                       | n/a                      |

  `raise-window` is fire-and-forget on cold spawn — the very first
  press launches and returns immediately; second press focuses once
  the window has appeared. See TODO #14 for the deferred fix.
- `mod+Shift+<letter>` — destructive / friction-needed actions (`Shift+q`
  kill, `Shift+p` powermenu, `Shift+r` reload-config, `Shift+n`
  send-to-scratchpad, `Shift+space` floating-toggle). Non-destructive
  pickers (toolboxes, ssh, hotkeys) used to live on this layer but moved
  to base `mod+<letter>` slots; only `Shift+p` (powermenu) stays here
  because lock/suspend/shutdown earn the friction. Lock is reached via
  `mod+Shift+p` powermenu — no dedicated key.
- `mod+Shift+<motion>` — move container (`Shift+h/j/k/l`, `Shift+arrows`).
- `mod+Ctrl+<motion>` — workspace-level move (`Ctrl+PgUp/Dn` move container +
  follow). Move-workspace-to-other-output is `mod+Shift+u` (left) / `mod+Shift+i`
  (right) — letter-based only.
- `mod+1..6` — workspaces (`workspace_auto_back_and_forth` makes the same key
  toggle back to the previous workspace, complementing `mod+g`).

Layout containers:

- `mod+[` / `mod+]` — split vertical / horizontal.
- `mod+.` — toggle between tabbed and split layout for the current container.
- `mod+f` / `mod+Shift+f` — fullscreen / floating.
- `mod+c` / `mod+Ctrl+c` — focus parent / child container.
- `mod+r` cycles preset widths via `~/.config/sway/scripts/preset-width`, which
  persists state in `$XDG_STATE_HOME/sway/preset-width` so consecutive presses
  rotate through `33 → 50 → 67 ppt`.
- `mod+-` / `mod+=` — shrink / grow width by 10 ppt; `Shift+-` / `Shift+=` for
  height. Resize *mode* is intentionally not bound — the preset cycle plus the
  ten-percent steppers cover the workflow.

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
4. Reload-or-restarts `sway-kanshi.service` so monitor profile selection always
   sees the current outputs.

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

Naming rule: daemons whose binary already starts with `sway` (`swaybg`,
`swayidle`) keep their bare name; others are prefixed (`sway-kanshi`,
`sway-mako`, etc.) to make `systemctl --user list-units 'sway*'` show the full
session in one place.

Idle behavior lives in `~/.config/sway/scripts/session-swayidle` because
monitor power commands are compositor-specific
(`swaymsg "output * power off"`).

`Ctrl+Alt+Delete` runs `~/.config/sway/scripts/session-quit`, which stops
`sway-session.target` and then `swaymsg exit` — leaving systemd cleanly
shutting down the daemons before the compositor itself terminates.

## Lock Screen

`~/.config/sway/scripts/lock-screen` is a small (~140 line) wrapper
around `swaylock`. All the picture work — blur, banner stamp, accent
color pluck, cache management — lives in the [`wallpaper`](../../fedora/bin/.local/bin/wallpaper)
helper. The lock-screen script just:

1. Calls `wallpaper status`, which returns JSON like
   `{"path": ..., "lock_image": ..., "accent": "9b8c32"}`.
2. Builds `swaylock --image <lock_image> --scaling fill <passthrough-args>`.
3. exec's into `swaylock`.

Locking must always succeed, so there's a fallback chain:

- `wallpaper` helper missing or status JSON malformed → solid Catppuccin
  base color (`swaylock --color 1e1e2e`).
- `lock_image` missing (e.g. magick not installed when the wallpaper
  was set) but `path` present → raw wallpaper, no blur or banner.
- `path` also missing → solid color again.

The wallpaper accent is deliberately scoped to the banner text *inside*
the rendered lock image — it doesn't bleed into the swaylock chrome.
All five state ring colors (rest, clear, caps-lock, ver, wrong) plus
`key-hl-color` come from `swaylock/.config/swaylock/config` so the
chrome stays theme-coherent across wallpapers. The status JSON still
emits `accent` for future consumers (e.g. propagating into waybar/mako
at wallpaper-set time).

Unknown flags pass through to swaylock verbatim (`parse_known_args`),
which is why `session-swayidle` can keep calling `lock-screen --daemonize`.

### Lock image rendering (lives in `wallpaper`)

The rendering recipe (blurred wallpaper + banner stamp) lives in
`fedora/bin/.local/bin/wallpaper`. It runs:

- as part of `wallpaper set <url-or-file>` and `wallpaper use [archive-file]`,
  so the next lock after a wallpaper change is instant; or
- via `wallpaper rebuild-cache` to force a fresh render (used after
  editing the render constants in `wallpaper`).

The blur (sigma 18), banner geometry, font (`JetBrainsMono-NF-Bold`),
and saturation scale (0.85) are constants at the top of the script.
Change them and bump `RENDER_VERSION` next to the constants — that
invalidates all cached entries automatically.

Results are cached under `$XDG_CACHE_HOME/wallpaper/<sha1(path+mtime+RENDER_VERSION)>.{png,color,sixel-WxH}`.
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

## Theme

Catppuccin Mocha hex values are inlined at the top of `~/.config/sway/config`.
See `docs/THEME.md` for the full palette and where it's mirrored across
other tools (waybar, mako, foot, swaylock, etc.).
