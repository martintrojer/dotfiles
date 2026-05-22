# Theme Color Settings

All configs are unified around **Catppuccin Mocha**, propagated from
a single source of truth at `docs/palette.toml`.

---

## Authoring flow

The single source of truth is `docs/palette.toml`. To change a color
across the stack:

1. Edit `docs/palette.toml`.
2. Run `make theme`.
3. Run `make check-theme` (also part of `make check-all`) to verify
   every consumer matches.

The renderer lives at `_dotfiles_sync/render_theme.py`. It walks every
file that carries a `THEME BEGIN: <name> ... THEME END: <name>`
marker pair, looks up the matching template under
`_dotfiles_sync/themes/<name>.tmpl`, expands ``{{mocha.<colorname>}}``
placeholders against the palette, and rewrites the content strictly
*between* the markers. The marker lines themselves and everything
outside the marked region are preserved verbatim.

Placeholder syntax:

- `{{mocha.<name>}}` — full hex with leading `#` (e.g. `#cba6f7`).
- `{{mocha.<name>|nohash}}` — bare 6-char hex (e.g. `cba6f7`). Used
  by swaylock/foot/fuzzel which want hex without the prefix and
  sometimes append an alpha suffix in the template.

A typo in a color name or filter fails loud rather than passing the
raw placeholder through.

---

## Catppuccin Mocha Reference

| Name | Hex | Usage |
|------|-----|-------|
| rosewater | #f5e0dc | |
| flamingo | #f2cdcd | |
| pink | #f5c2e7 | |
| mauve | #cba6f7 | Primary accent |
| red | #f38ba8 | Errors, warnings, critical |
| maroon | #eba0ac | |
| peach | #fab387 | |
| yellow | #f9e2af | Warnings, highlights |
| green | #a6e3a1 | Success |
| teal | #94e2d5 | Secondary accent |
| sky | #89dceb | |
| sapphire | #74c7ec | |
| blue | #89b4fa | Links, info |
| lavender | #b4befe | Focus rings, borders |
| text | #cdd6f4 | Primary text |
| subtext1 | #bac2de | |
| subtext0 | #a6adc8 | |
| overlay2 | #9399b2 | |
| overlay1 | #7f849c | |
| overlay0 | #6c7086 | Muted text |
| surface2 | #585b70 | |
| surface1 | #45475a | |
| surface0 | #313244 | Borders |
| base | #1e1e2e | Background |
| mantle | #181825 | Darker background |
| crust | #11111b | Darkest background |

(This table is hand-maintained alongside `docs/palette.toml`. The
TOML is canonical for the renderer; this table is the human-readable
gloss.)

---

## Generated regions

Files with `THEME BEGIN ... THEME END` markers, owned by the renderer:

| File | Region |
|------|--------|
| `sway/.config/sway/config` | `sway-palette` |
| `waybar/.config/waybar/style.css` | `waybar-palette` |
| `waybar/.config/waybar/config.jsonc` | `waybar-calendar-colors` |
| `mako/.config/mako/config` | `mako-colors` |
| `swaylock/.config/swaylock/config` | `swaylock-colors` |
| `tmux/.tmux.conf` | `tmux-palette` |
| `zsh/.zsh/tools.zsh` | `zsh-prompt-colors` |
| `foot/.config/foot/foot.ini` | `foot-colors` |
| `fuzzel/.config/fuzzel/fuzzel.ini` | `fuzzel-colors` |
| `btop/.config/btop/themes/current.theme` | `btop-colors` |
| `eza/.config/eza/theme.yml` | `eza-colors` |
| `tmux/.config/tmux/scripts/status-hostname` | `status-hostname-colors` |
| `tmux/.config/tmux/scripts/status-ram` | `status-ram-colors` |
| `tmux/.config/tmux/scripts/tms` | `tms-palette` |
| `fedora/bin/.local/bin/wallpaper` | `wallpaper-fallback-color` |
| `sway/.config/sway/scripts/lock-screen` | `lock-screen-fallback-color` |
| `sway/.config/sway/scripts/session-wallpaper` | `session-wallpaper-fallback-color` |

---

## Outside the renderer (not generated)

These carry their own hex values and are intentionally not in the
generator:

- `bat/.config/bat/themes/Catppuccin Mocha.tmTheme` — vendored from
  the Catppuccin upstream. Update via their distribution.
- `yazi/.config/yazi/flavors/catppuccin-mocha.yazi/` — vendored
  Catppuccin flavor.
- `glow/.config/glow/catppuccin-mocha.json` — vendored.
- `ghostty/.config/ghostty/config` — uses the built-in named theme
  (`theme = catppuccin-mocha`); no hex in our config.
- `nvim/.config/nvim/` — `catppuccin/nvim` plugin handles theming.
- `waybar/.config/waybar/style.css` `@define-color base50` — `base`
  at 50% alpha. Hand-derived from `mocha.base`.
- `guides/style.css` + `guides/build/` — public website assets.
  Could be folded in later.
- Test files (`tmux/.../test-status-tools`) — hex appears as
  *assertion fixtures* exercising production code. They're meant
  to fail loud if the palette moves; that's the test working.

---

## Chrome/Chromium Custom Color

Settings → Appearance → Theme → Custom color

RGB: `30, 30, 46` (from base #1e1e2e)
