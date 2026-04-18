# Wallpapers

Wallpaper image assets are intentionally not stored in this repo. This package is
documentation-only.

Per-machine wallpaper flow:

```bash
wallpaper set https://example.com/my-wallpaper.jpg
```

This stores wallpapers under `~/.local/share/wallpapers/archive/`, updates `~/.local/share/wallpapers/current`, and restarts `swaybg.service`.

To switch to a previously archived wallpaper:

```bash
wallpaper use
```

This opens an `fzf` picker. When `chafa` is available, it is used for previews.

Consumers:
- `~/.config/niri/scripts/session-wallpaper` uses the installed wallpaper when present, otherwise falls back to the Catppuccin Mocha base color
- `~/.config/niri/scripts/lock-screen` uses the installed wallpaper for `swaylock`, otherwise falls back to the same solid color

Useful subcommands:

```bash
wallpaper current
wallpaper restart
```
