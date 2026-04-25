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
| Eza | `eza/.config/eza/theme.yml` | Catppuccin Mocha mauve accent |
| Fuzzel | `fuzzel/.config/fuzzel/fuzzel.ini` | Catppuccin Mocha palette |
| Ghostty | `ghostty/.config/ghostty/config` | `theme = Catppuccin Mocha` |
| Mako | `mako/.config/mako/config` | Catppuccin hex values |
| Neovim | `nvim/.config/nvim/init.lua` (setup) + `nvim/.config/nvim/lua/plugins.lua` (spec) | catppuccin/nvim plugin |
| Sway | `sway/.config/sway/config` | Catppuccin hex values |
| Zsh | `zsh/.zsh/tools.zsh` | Native `PROMPT`/`RPROMPT` with Catppuccin hex values |
| Swaylock | `swaylock/.config/swaylock/config` | Catppuccin-inspired colors with wallpaper from `wallpapers/` |
| Tmux | `tmux/.tmux.conf` | Native tmux status bar using Catppuccin Mocha hex values |
| Waybar | `waybar/.config/waybar/style.css` | Catppuccin colors defined |
| Waybar Calendar | `waybar/.config/waybar/config.jsonc` | Inline Catppuccin hex |
| Wallpapers | `wallpapers/.config/wallpapers/` | Shared background assets |
| Yazi | `yazi/.config/yazi/theme.toml` | catppuccin-mocha flavor |

---

## Chrome/Chromium Custom Color

Settings → Appearance → Theme → Custom color

RGB: `30, 30, 46` (from base #1e1e2e)

---

## Zsh Prompt

The native zsh prompt uses Catppuccin Mocha colors directly:

- Directory: lavender `#b4befe`
- Prompt chevron: yellow `#f9e2af`
- Error marker: red `#f38ba8`
- SSH hostname reminder: overlay0 `#6c7086`
- Toolbox name reminder: teal `#94e2d5`
