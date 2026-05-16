# foot

[foot](https://codeberg.org/dnkl/foot) is the Wayland terminal for the Sway
setup. Run as a server (`sway-foot-server.service`); `footclient` spawns
near-instant windows that share one process.

## Package layout

Single stow package, Linux-only (foot is Wayland-only).

```sh
stow foot
```

Foot reloads its config automatically on file change.

## Config highlights

- Catppuccin Mocha palette (matches the rest of the desktop)
- Hack Nerd Font Mono @ 12pt with `vertical-letter-offset=0`
- 8px window padding, 90% background opacity
- 50000 lines of scrollback, 3-line wheel multiplier
- Hide mouse cursor while typing
- Selection saved to clipboard
- URL mode (`Ctrl+Shift+o`) labels every URL on screen for keyboard launch

## Sway integration

`set $term footclient` in `sway/.config/sway/config`; waybar and fuzzel both
launch `footclient` directly. There is no `foot` (no -client) fallback —
fix the systemd unit if it's not running.
