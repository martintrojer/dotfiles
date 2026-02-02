# Theme Color Settings

All configs are now unified around **Catppuccin Mocha**.

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

---

## Config Files

| Component | File | Theme Source |
|-----------|------|--------------|
| Bat | `bat/.config/bat/themes/` | Catppuccin Mocha tmTheme (run `bat cache --build`) |
| Btop | `btop/.config/btop/themes/current.theme` | Catppuccin Mocha colors |
| Fuzzel | `fuzzel/.config/fuzzel/fuzzel.ini` | Catppuccin Mocha palette |
| Ghostty | `ghostty/.config/ghostty/config` | `theme = Catppuccin Mocha` |
| Hypr Palette | `hypr/.config/hypr/mocha.conf` | Full Catppuccin palette |
| Hyprlock | `hypr/.config/hypr/hyprlock.conf` | Sources `mocha.conf` |
| Hyprpaper | `hypr/.config/hypr/hyprpaper.conf` | Wallpaper config |
| Mako | `mako/.config/mako/config` | Catppuccin hex values |
| Neovim | `nvim/.config/nvim/lua/plugins/colorscheme.lua` | catppuccin/nvim plugin |
| Niri | `niri/.config/niri/config.kdl` | Catppuccin hex values |
| Tmux | `tmux/.tmux.conf` | `catppuccin/tmux` plugin, mocha flavor |
| Waybar | `waybar/.config/waybar/style.css` | Catppuccin colors defined |
| Waybar Calendar | `waybar/.config/waybar/config.jsonc` | Inline Catppuccin hex |
| Yazi | `yazi/.config/yazi/theme.toml` | catppuccin-mocha flavor |

---

## Chrome/Chromium Custom Color

Run the script to set the theme color (Chrome must be closed):

```bash
chrome/set-chrome-theme "#1e1e2e"          # Dry-run (preview changes)
chrome/set-chrome-theme --apply "#1e1e2e"  # Apply changes
```

Or set manually: Settings → Appearance → Theme → Custom color

RGB: `30, 30, 46` (from base #1e1e2e)
