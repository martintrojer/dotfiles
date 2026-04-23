# Sway

This package provides the main Wayland compositor setup. The emphasis is on a
small, mainstream stack (sway + waybar + fuzzel + kanshi + mako + swayidle +
swaylock) that stays close to upstream defaults so it can be reasoned about
without surprises.

## Files

- `~/.config/sway/config` — main config (Mod4, Catppuccin colors, app launchers,
  workspaces, layout primitives, media keys, screenshots, idle inhibit).
- `~/.config/sway/scripts/` — session, screenshot, lock, app launcher, and
  preset-width helpers.
- Windowing: tiling with splits, tabbed layouts, floating mode, scratchpad-ready
  primitives, and six numbered workspaces.

The hotkey overlay (`mod+Shift+/`) lives under `fuzzel/` because it's a fuzzel
picker, not a sway script — see `fuzzel/README.md`.

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

- `mod+<letter>` — launch an app (`t` term, `b` browser, `y` yazi (TUI files),
  `m` cider, `i` vscode, `e` emoji).
- `mod+Shift+<letter>` — launch a fuzzel picker (`Shift+t` toolboxes, `Shift+s`
  ssh, `Shift+p` powermenu, `Shift+b` browser profile, `Shift+/` hotkeys),
  exception: `mod+Shift+y` launches thunar (GUI files, paired with `mod+y`
  yazi). Lock is reached via `mod+Shift+p` powermenu — no dedicated key.
- `mod+Shift+<motion>` — move container (`Shift+h/j/k/l`, `Shift+arrows`).
- `mod+Ctrl+<motion>` — workspace-level move (`Ctrl+PgUp/Dn` move container +
  follow, `Ctrl+Home/End` move workspace to other output).
- `mod+1..6` — workspaces (`workspace_auto_back_and_forth` makes the same key
  toggle back to the previous workspace, complementing `mod+g`).

Layout containers:

- `mod+[` / `mod+]` — split vertical / horizontal.
- `mod+.` — toggle between tabbed and split layout for the current container.
- `mod+Ctrl+f` — force tabbed layout.
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
- `mod+Shift+s` — `ssh`, ssh-config host picker.
- `mod+Shift+t` — `toolboxes`, toolbox container picker.
- `mod+Shift+p` — `powermenu` (lock / suspend / logout / reboot / shutdown).
- `mod+grave` — `chrome-tabs`, DevTools-protocol tab switcher (paired
  visually with `mod+Tab` window switcher — grave and Tab sit adjacent
  on the keyboard).
- `mod+Shift+/` — `hotkeys`, parses this file's bindings and dispatches the
  chosen action via `swaymsg`.

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
See `THEME.md` in the repo root for the full palette and where it's mirrored
across other tools (waybar, mako, alacritty, swaylock, etc.).
