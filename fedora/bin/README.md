# Fedora Bin

Small wrapper executables that should be available on `$PATH` even outside an
interactive shell live here under `.local/bin/`.

These wrappers are on `$PATH` for the whole graphical session (not just
interactive shells) via `fedora/systemd/.config/environment.d/10-local-bin.conf`,
which the systemd user manager applies at login. Without it, apps launched from
fuzzel inherit only the bare login PATH and fail with "command not found".

Gaming helpers (`steam-session`, `optirun`, `optiscaler-client`,
`fix-steam-games`, `steam-pause`, `steamos-session-select`) live in the
quarantined gaming layer at
[`fedora/gaming/home/.local/bin/`](../gaming/README.md), not here.

## tbx

The toolbox-backed command wrappers use `tbx --prefer-host -c <toolbox>`, which
first prefers a real host binary and otherwise falls back to the toolbox
container named by the wrapper.

For ad-hoc commands, use `tbx <command> [args...]` to run the command in the
`dev` toolbox by default. Use `-c <toolbox>` for another toolbox, for example:

```sh
tbx python --version
```

Set `TBX_DEFAULT_TOOLBOX` to change the default. Pass `--prefer-host` to `tbx`
when you want wrapper-like behavior that uses a host binary if one exists before
falling back to the toolbox.

## lms

`lms` is the LM Studio CLI. The bundled binary only runs inside the LM Studio
flatpak sandbox, so the wrapper shells into it via
`flatpak run --command=sh ai.lmstudio.lm-studio`. Use it exactly like the
native CLI:

```sh
lms server start          # start the headless OpenAI-compatible server (:1234)
lms server status
lms ls                    # list downloaded models
lms ps                    # list loaded models
lms load <model>
```

The headless server is normally managed by `lmstudio-server.service` (see
[../README.md](../README.md)); the wrapper is for ad-hoc use.

## Wallpaper helpers

- `wallpaper set <url-or-file>` stores the wallpaper under `~/.local/share/wallpapers/archive/`, updates `~/.local/share/wallpapers/current`, and restarts `swaybg.service`
- `wallpaper use [archive-file]` switches to an archived wallpaper; without an argument it opens an `fzf` picker with sixel previews rendered by ImageMagick (foot renders sixel natively)
- `wallpaper current` prints the active wallpaper path if one is installed
- `wallpaper restart` restarts `swaybg.service` to re-apply the current wallpaper
- `wallpaper preview <archive-file>` renders a wallpaper as sixel (`magick … sixel:-`) into the `fzf` preview pane (used internally by `wallpaper use`); falls back to printing the path if `magick` is missing
