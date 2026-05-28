# Waybar

Calm, exception-only status bar for Sway. The visual language follows
[`docs/LAYOUT.md`](../../docs/LAYOUT.md): blocks are affordances, not decoration.

## Shape

The default bar only shows orientation/context:

- left: workspaces and focused window title
- right: issues, notifications, tray, clock

Power-user telemetry is intentionally not always visible. It is aggregated by
`~/.config/waybar/scripts/issues` and appears only when something crosses a
threshold. Healthy state emits an empty module with the `ok` class.

Layout language:

- workspaces are blocks because they are navigation objects
- issues and notifications become blocks only when actionable
- tray and clock are ambient context, so they stay unboxed
- window title is context, not a status cell

## Issue policy

`custom/issues` currently surfaces:

- high CPU load (left click: `btop`)
- high CPU/GPU temperature (left click: `btop`)
- high RAM or swap pressure (left click: `btop`)
- `/var` disk pressure
- offline or weak Wi-Fi (left click: `nmtui`)
- low Bluetooth battery (left click: `bluetui`)
- muted output (left click: `wiremix`; right click: unmute)
- live microphone (left click: `wiremix`; right click: mute)

Details stay in the tooltip; the bar text stays compact. Click behavior follows
one rule: left click opens the relevant TUI, right click performs the safe direct
fix where one exists.

## Demo mode

For layout tuning, both Waybar scripts read a shared demo flag at
`$XDG_STATE_HOME/waybar/demo`:

```sh
~/.config/waybar/scripts/issues demo on      # show all synthetic issue types
~/.config/waybar/scripts/notifications demo on
systemctl --user restart sway-waybar.service

~/.config/waybar/scripts/issues demo off
systemctl --user restart sway-waybar.service
```

The flag is shared, so either script can toggle it. Demo mode does not change
system state; it only changes script output.

## Audio controls

Audio controls moved out of always-visible Waybar modules:

- media keys still adjust volume/mute via Sway bindings
- `mod+Shift+a` summons `wiremix` in a centered floating terminal for changing
  output/input devices and per-app levels
- Waybar only surfaces audio when state is surprising, e.g. output muted or mic
  live

## Notifications

`~/.config/waybar/scripts/notifications` is also quiet by default. It only shows
when notification history exists, notifications are visible, or DND is enabled.

Click actions:

- left click: open notification picker
- right click: clear visible notifications and history by restarting `mako`
- mouse back: toggle DND
