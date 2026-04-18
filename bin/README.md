# Bin

Small wrapper executables that should be available on `$PATH` even outside an
interactive shell live here under `.local/bin/`.

The toolbox-backed commands use `_toolbox_shim`, which first prefers a real
host binary and otherwise falls back to the toolbox container named by the
wrapper.

Wallpaper helpers:
- `wallpaper set <url-or-file>` stores the wallpaper under `~/.local/share/wallpapers/archive/`, updates `~/.local/share/wallpapers/current`, and restarts `swaybg.service`
- `wallpaper use [archive-file]` switches to an archived wallpaper; without an argument it opens an `fzf` picker and uses `chafa` for previews when available
- `wallpaper current` prints the active wallpaper path if one is installed
- `wallpaper restart` restarts `swaybg.service` to re-apply the current wallpaper
