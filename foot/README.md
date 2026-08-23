# foot

[foot](https://codeberg.org/dnkl/foot) is the Wayland terminal for the Sway
setup. The `sway-foot-server.service` unit runs the server, and `footclient`
opens windows that share its process.

## Package layout

Single package, Linux-only (foot is Wayland-only). Linked by
`./dotfiles-sync --apply`.

Foot reloads its config automatically on file change.

## Config highlights

- Catppuccin Mocha palette (matches the rest of the desktop)
- Hack Nerd Font Mono @ 12pt with `vertical-letter-offset=0`
- 8px window padding, 90% background opacity
- 50000 lines of scrollback, 3-line wheel multiplier
- Hide mouse cursor while typing
- Selection saved to clipboard
- Copy/paste: `Ctrl+Shift+c` / `Ctrl+Shift+v` (foot default) plus
  `Ctrl+Insert` / `Shift+Insert` so the Rainy75 Cmd-layer chord works
- URL mode (`Ctrl+Shift+o`) labels every URL on screen for keyboard launch

## Sway integration

The `set $term footclient` line in `sway/.config/sway/config` selects the
client. Waybar and fuzzel also launch `footclient` directly. If the server is
not running, fix the systemd unit. The configuration does not fall back to
`foot`.
