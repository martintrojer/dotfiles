# Fedora Bin

Small wrappers that must be available on `$PATH` outside interactive shells
live under `.local/bin/`.

These wrappers are on `$PATH` for the whole graphical session (not just
interactive shells) via `fedora/systemd/.config/environment.d/10-local-bin.conf`,
which the systemd user manager applies at login. Without it, apps launched from
fuzzel inherit only the bare login PATH and fail with "command not found".

Gaming helpers (`steam-session`, `optirun`, `optiscaler-sync`, `steam-pause`,
`steamos-session-select`) live in the
quarantined gaming layer at
[`fedora/gaming/home/.local/bin/`](../gaming/README.md), not here.

## tbx

Toolbox-backed wrappers use `tbx --prefer-host -c <toolbox>`. This command uses
a host binary when available, then falls back to the toolbox named by the
wrapper.

For ad-hoc commands, use `tbx <command> [args...]` to run the command in the
`dev` toolbox by default. Use `-c <toolbox>` for another toolbox, for example:

```sh
tbx python --version
```

Set `TBX_DEFAULT_TOOLBOX` to change the default. Pass `--prefer-host` to `tbx`
when you want wrapper-like behavior that uses a host binary if one exists before
falling back to the toolbox.

`cava` is the one shipped wrapper of that shape: it runs the audio visualiser
from the `dev` toolbox unless the host has its own binary.

## nvdiff

`nvdiff <left> <right>` opens Neovim's `:DiffTool` on a pair of paths and
reattaches to `/dev/tty`, so it works as a VCS difftool rather than only as an
interactive command. It backs the `gvd` and `jvd` aliases; see
[`zsh/README.md`](../../zsh/README.md).

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

The headless server is normally managed by `lmstudio-server.service` through
the companion `lmstudio-server` supervisor (see [../README.md](../README.md));
this wrapper remains useful for ad-hoc CLI commands.

## t3code

Launcher for the extracted T3 Code AppImage at
`~/.local/opt/t3code/squashfs-root/`. It puts mise's shims on `PATH` (T3 Code
spawns `codex`/`claude`/`opencode`, which a GUI launch would not otherwise
find) and selects native Wayland. Installed and updated by
[`fedora/t3code/setup-t3code.sh`](../t3code/README.md).

## Wallpaper helpers

- `wallpaper set <url-or-file>` keeps the original under `~/.local/share/wallpapers/archive/`, renders it for the active display, updates `~/.local/share/wallpapers/current`, and restarts `swaybg.service`
- `wallpaper use` opens an `fzf` archive picker with sixel previews and activates the selected wallpaper; it does not accept a filename
- `wallpaper current` prints the display-sized render when available and the original archive path as a fallback
- `wallpaper rebuild-cache` rebuilds the display render, then creates the blurred lock-screen image from that same composition
- `wallpaper restart` restarts `swaybg.service` to re-apply the current wallpaper
- `wallpaper preview <archive-file>` renders a wallpaper as sixel (`magick … sixel:-`) into the fzf preview pane; this is an internal picker command

The display render uses the focused output's physical resolution, or 2560×1440
when Sway output discovery fails. It trims transparent outer padding before
measuring the image. A centered crop fills the display when it would discard no
more than 10% of a visible source dimension. Larger aspect-ratio differences
place the complete visible image over a blurred, darkened copy that fills the
screen. `#08090c` fills any transparent holes left in the background.
