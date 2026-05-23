# Fedora Bin

Small wrapper executables that should be available on `$PATH` even outside an
interactive shell live here under `.local/bin/`.

The toolbox-backed command wrappers use `tbx --prefer-host -c <toolbox>`, which
first prefers a real host binary and otherwise falls back to the toolbox
container named by the wrapper.

For ad-hoc commands, use `tbx <command> [args...]` to run the command in the
`dev` toolbox by default. Use `-c <toolbox>` for another toolbox, for example:

```sh
tbx python --version
tbx -c ollama ollama serve
```

Set `TBX_DEFAULT_TOOLBOX` to change the default. Pass `--prefer-host` to `tbx`
when you want wrapper-like behavior that
uses a host binary if one exists before falling back to the toolbox.

Wallpaper helpers:
- `wallpaper set <url-or-file>` stores the wallpaper under `~/.local/share/wallpapers/archive/`, updates `~/.local/share/wallpapers/current`, and restarts `swaybg.service`
- `wallpaper use [archive-file]` switches to an archived wallpaper; without an argument it opens an `fzf` picker with sixel previews rendered by ImageMagick (foot renders sixel natively)
- `wallpaper current` prints the active wallpaper path if one is installed
- `wallpaper restart` restarts `swaybg.service` to re-apply the current wallpaper
- `wallpaper preview <archive-file>` renders a wallpaper as sixel (`magick … sixel:-`) into the `fzf` preview pane (used internally by `wallpaper use`); falls back to printing the path if `magick` is missing
