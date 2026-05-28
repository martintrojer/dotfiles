# Waybar

Calm, exception-only status bar for Sway. The visual language follows
[`docs/LAYOUT.md`](../../docs/LAYOUT.md): blocks are affordances, not decoration.

## Shape

The default bar is a sparse top HUD:

- left: workspaces
- center: issues, hidden when healthy
- right: notifications, weather, and clock

There is intentionally no tray and no window title. App/system controls live in
keybindings and issue click actions; focused-window context belongs to the app,
tmux, or the window switcher rather than the desktop bar.

Power-user telemetry is intentionally not always visible. It is aggregated by
`~/.config/waybar/scripts/issues` and appears only when something crosses a
threshold. Healthy state emits an empty module with the `ok` class.

Layout language:

- workspaces are blocks because they are navigation objects
- issues sit in the center and become blocks only when actionable
- notifications become blocks only when actionable
- clock is ambient context, so it stays unboxed

## Sizing and typography

The bar is tuned to stay at 20px on the main display, preserving an extra tmux
row compared with the old larger bar.

- base font: SF Compact + JetBrains fallback, 10pt, weight 500
- issue/notification blocks: JetBrains Mono Nerd Font
- window/tray modules are omitted rather than shrunk into ambiguity

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

## Weather

`~/.config/waybar/scripts/weather` is ambient context, not telemetry. It renders
only the last good wttr.in result, cached at `$XDG_CACHE_HOME/waybar/weather.json`
for up to one hour. Refreshes happen in the background after ten minutes; if
there is no valid cache, the module renders nothing instead of showing an error.

Set `WTTR_LOCATION` to pin the location; otherwise wttr.in guesses from IP.

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
