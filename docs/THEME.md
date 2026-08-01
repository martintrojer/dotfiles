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

The palette deliberately carries the complete Catppuccin Mocha set,
including colors no template references today (e.g. `flamingo`). Do not
prune unreferenced entries — the set is the reference, not a usage list.

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
| `tmux/.tmux.conf` | `tmux-palette`, `tmux-agent-glyphs` |
| `zsh/.zsh/tools.zsh` | `zsh-prompt-colors` |
| `foot/.config/foot/foot.ini` | `foot-colors` |
| `fuzzel/.config/fuzzel/fuzzel.ini` | `fuzzel-colors` |
| `btop/.config/btop/themes/current.theme` | `btop-colors` |
| `eza/.config/eza/theme.yml` | `eza-colors` |
| `tmux/.config/tmux/scripts/status-hostname` | `status-hostname-colors` |
| `tmux/.config/tmux/scripts/status-ram` | `status-ram-colors` |
| `tmux/.config/tmux/scripts/tms` | `tms-palette` |
| `sway/.config/sway/scripts/lock-screen` | `lock-screen-fallback-color` |
| `sway/.config/sway/scripts/session-wallpaper` | `session-wallpaper-fallback-color` |
| `fedora/bin/.local/bin/wallpaper` | `wallpaper-fallback-color` |
| `guides/style.css` | `guides-palette` |

---

## Non-color values: the `glyph` group

One region is not about color. `tmux-agent-glyphs` renders the
agent-state glyph chain used by both `window-status-format` and
`window-status-current-format`, and its glyphs come from `STATE_GLYPH`
in `tmux/.config/tmux/scripts/_tmux_common.py`, not from
`docs/palette.toml`.

That dict is already the single source for every Python display surface
(the `tms` picker, the `prefix + a` menu, the status pill). `.tmux.conf`
cannot import Python, so it used to re-spell `✗ ! ▶ ·` by hand — twice,
once per window-status line — and nothing failed when the copies
diverged. `load_palette()` now exposes those glyphs to templates as a
synthetic `glyph` group (`{{glyph.crashed}}`), so the chain is generated
and `make check-theme` fails on drift. `palette.toml` may not define a
`glyph` group itself; the renderer rejects it rather than silently
shadowing the Python.

The two window-status lines share the one rendered `@agent_glyphs`
option and supply their own background, because tmux carries the
surrounding `#[bg=...]` into an `#{E:...}` re-expansion.

---

## The blindspot audit

`CONSUMERS` is hand-maintained, so forgetting a `Consumer()` entry used
to be silent: the file just drifted. `make check-theme` now also runs
`render_theme.py --audit`, which walks the repo for hex values matching
`docs/palette.toml` and fails on any hit outside a `THEME BEGIN..END`
region. Only exact palette values count — a deliberately non-Catppuccin
color (`guides/style.css` uses `#eef2ff` for headings) is not drift.

Files that legitimately hold palette hex outside a region live in
`AUDIT_ALLOWLIST` in `render_theme.py`, each with its reason. That list
is the executable version of the section below; keep the two in step.

---

## Outside the renderer (not generated)

These carry their own hex values and are intentionally not in the
generator. Each is in `AUDIT_ALLOWLIST`:

- `bat/.config/bat/themes/Catppuccin Mocha.tmTheme` — vendored from
  the Catppuccin upstream. Update via their distribution.
- `yazi/.config/yazi/flavors/catppuccin-mocha.yazi/` — vendored
  Catppuccin flavor.
- `glow/.config/glow/catppuccin-mocha.json` — vendored Catppuccin glow
  style. Co-maintained with `local-bin/.local/bin/m`, but we never
  hand-edit the colors, so templating 50+ values buys nothing.
- `ghostty/.config/ghostty/config` — uses the built-in named theme
  (`theme = catppuccin-mocha`); no hex in our config.
- `nvim/.config/nvim/` — `catppuccin/nvim` plugin handles theming.
- `waybar/.config/waybar/style.css` — the `@define-color` block *is*
  generated (`waybar-palette`), but the alpha derivations further down
  the file (`alpha(@base, 0.55)`) are hand-written GTK CSS on top of
  the generated names.
- `tmux/.config/tmux/scripts/test-status-tools` — carries no hex. It
  asserts on palette *key names* in the generated `tms-palette` region
  (every key the render path reaches exists, and every generated key is
  referenced), so it fails loud if that template drifts.

---

## Chrome/Chromium Custom Color

Settings → Appearance → Theme → Custom color

RGB: `30, 30, 46` (from base #1e1e2e)
