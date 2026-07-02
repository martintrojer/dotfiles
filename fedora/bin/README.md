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

These wrappers are on `$PATH` for the whole graphical session (not just
interactive shells) via `fedora/systemd/.config/environment.d/10-local-bin.conf`,
which the systemd user manager applies at login. Without it, apps launched from
fuzzel (e.g. Steam) inherit only the bare login PATH and fail with "command not
found". The embedded "Steam (gamescope)" SDDM session is launched via a
non-interactive login shell that sources neither `~/.zshrc` nor `environment.d`,
so `steam-session` prepends `~/.local/bin` to PATH itself before exec'ing
gamescope.

Gaming:
- **Sway desktop**: light SDR gaming. Launch games normally.
- **Steam (gamescope) SDDM session**: HDR gaming. `steam-session` owns the
  display via gamescope DRM, enables HDR, draws MangoHud (`--mangoapp`), and
  exports only HDR WSI env: `DXVK_HDR=1 ENABLE_GAMESCOPE_WSI=1`.
- **Steam (gamescope stream) SDDM session**: same launcher with
  `GS_OUT_W=1920 GS_OUT_H=1080 GS_HDR=0 GS_SUNSHINE=1` for streaming to a
  handheld via Sunshine. See [../docs/STREAMING.md](../docs/STREAMING.md).
- **Per-game OptiScaler/FSR4/GameMode**: use `optirun %command%`. It sets
  `WINEDLLOVERRIDES=dxgi=n,b`, `PROTON_FSR4_UPGRADE=1`,
  `DXIL_SPIRV_CONFIG=wmma_rdna3_workaround`, then runs the game via
  `gamemoderun`. Opt out with `OPTIRUN_OPTI=0`, `OPTIRUN_FSR4=0`,
  `OPTIRUN_RDNA3=0`, or `OPTIRUN_GAMEMODE=0`; use `OPTIRUN_DLL=<name>` for a
  non-`dxgi` proxy DLL.
- `steam-session` refuses to run inside another graphical session, prepends
  `~/.local/bin` to PATH, exports `MANGOHUD_CONFIGFILE`, and accepts
  `GS_OUT_W/H`, `GS_REFRESH`, `GS_HDR`, `GS_ARGS`. See
  [../docs/HDR-GAMING.md](../docs/HDR-GAMING.md).
- `steam-pause {pause,resume,list}` finds running Steam games (their
  `reaper ... AppId=` processes), walks each child tree, and `SIGSTOP`/`SIGCONT`s
  the children (leaving the reaper alive so Steam doesn't see the game as
  exited). Extracted from the SDH-PauseGames Decky plugin, no Decky needed.
  `../setup-steam-pause.sh` copies it to `/usr/local/bin` and enables a oneshot
  unit (`../systemd-system/steam-pause-games.service`) ordered around
  `sleep.target` that runs `pause` before suspend and `resume` on wake,
  avoiding crackling audio and frozen emulators in the "Steam (gamescope)"
  session. A `system-sleep` hook can't be used (Atomic `/usr/lib` is read-only
  and `systemd-sleep` reads only that dir); the script is copied, not symlinked
  to `~/.local/bin`, since the root unit can't exec under `$HOME` (SELinux
  `user_home_t`, 203/EXEC). Stopped games hold RAM/VRAM, so best for short
  suspends.
- `../setup-power-key.sh` installs a global logind drop-in so the power button
  suspends: short press = sleep, long press = power off. Applies to all sessions
  (Sway, gamescope, SDDM greeter). Pairs with the pause unit and with Steam's
  own "Suspend" menu item (same `systemctl suspend` path).

OptiScaler manager (GUI):
- `optiscaler-client` installs/updates/runs upstream OptiScaler Client into
  `~/.local/share/optiscaler-client/` (binary not vendored). Commands: `run`,
  `update [--tag TAG] [--force]`, `status`, `path`.
- `optiscaler-client.desktop` exposes it to `drun` launchers.
- `optirun %command%` loads installed proxy DLLs for a game; override manually
  with `WINEDLLOVERRIDES=... %command%` if needed.
- First run seeds stable prefs from `fedora/optiscaler-client-seed/` only when
  missing. Private/volatile state (`games.json`, `Cache/`, geometry, timestamps)
  stays untracked.
- `fix-steam-games` checks/fixes OptiScaler games: stamps `ShortcutKey=0x24`,
  applies `optirun %command%` to installed games with OptiScaler, and removes
  stale old wrappers from installed games without OptiScaler. Dry-run by default;
  write with `--apply` (refuses while Steam is running unless `--force`). Limit
  with `--only shortcut` or `--only launch-options`; override the key with
  `--key 0xNN`.

Wallpaper helpers:
- `wallpaper set <url-or-file>` stores the wallpaper under `~/.local/share/wallpapers/archive/`, updates `~/.local/share/wallpapers/current`, and restarts `swaybg.service`
- `wallpaper use [archive-file]` switches to an archived wallpaper; without an argument it opens an `fzf` picker with sixel previews rendered by ImageMagick (foot renders sixel natively)
- `wallpaper current` prints the active wallpaper path if one is installed
- `wallpaper restart` restarts `swaybg.service` to re-apply the current wallpaper
- `wallpaper preview <archive-file>` renders a wallpaper as sixel (`magick … sixel:-`) into the `fzf` preview pane (used internally by `wallpaper use`); falls back to printing the path if `magick` is missing
