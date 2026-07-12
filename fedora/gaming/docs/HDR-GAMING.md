# HDR gaming on Fedora Sway Atomic

Box: AMD RX 7800 XT + HDR LG 4K on `DP-1`.

## Model

Use these environments:

- **Sway desktop**: light SDR gaming. Launch games normally. No nested gamescope.
- **Steam (gamescope) SDDM session**: real HDR. gamescope uses the DRM backend,
  owns KMS/DP-1, starts Steam Big Picture, and draws MangoHud with `--mangoapp`.
- **Steam (gamescope stream) SDDM session**: same launcher, 1080p SDR + Sunshine
  for handheld streaming. See [STREAMING.md](./STREAMING.md).

Sway 1.11 lacks the color-management protocol needed for HDR. For real HDR, log
into the gamescope session instead of nesting gamescope inside Sway. Background:
[gamescope#2008](https://github.com/ValveSoftware/gamescope/issues/2008),
[ArchWiki HDR](https://wiki.archlinux.org/title/HDR_monitor_support).

## Setup

Run `config/setup-gamescope-session.sh` after `os/setup-steam.sh` and
`dotfiles-sync --apply`.

It installs:

- `/usr/local/bin/steam-session` → stowed `~/.local/bin/steam-session`
- `/usr/local/share/wayland-sessions/steam.desktop` (couch HDR)
- `/usr/local/share/wayland-sessions/steam-stream.desktop` (1080p SDR streaming)

## Use

Pick **Steam (gamescope)** in SDDM. In gamepad UI the power menu has no "Exit
Steam"; use **Desktop Mode** to return to SDDM. That button calls
`steamos-session-select` on `PATH`, and the stowed shim
(`~/.local/bin/steamos-session-select`) shuts Steam down and ends the session so
you land back at the SDDM greeter (we have no desktop-in-gamescope target).

`steam-session` is intentionally narrow:

```bash
gamescope --backend drm -W 3840 -H 2160 -r 60 --hdr-enabled -f --mangoapp -e -- \
  env DXVK_HDR=1 ENABLE_GAMESCOPE_WSI=1 steam -gamepadui
```

It owns/enforces:

- `--backend drm` — direct KMS/display ownership.
- `--hdr-enabled` — HDR10 output.
- `DXVK_HDR=1 ENABLE_GAMESCOPE_WSI=1` — Vulkan/Proton HDR through gamescope WSI.
- `--mangoapp` — session MangoHud overlay.

It does **not** inject GameMode, OptiScaler, FSR4, DLL overrides, or performance
tweaks into Steam's CEF UI.

Knobs: `GS_OUT_W`, `GS_OUT_H`, `GS_REFRESH`, `GS_HDR=0`, `GS_ARGS`.

## Per-game OptiScaler / FSR4 / GameMode

For games that need the OptiScaler/FSR4/GameMode path, set Steam launch options:

```text
optirun %command%
```

`optirun` applies only to that game:

- `WINEDLLOVERRIDES=dxgi=n,b`
- `PROTON_FSR4_UPGRADE=1`
- `DXIL_SPIRV_CONFIG=wmma_rdna3_workaround` for RDNA3/RX 7000
- `gamemoderun`

Opt-outs:

```text
OPTIRUN_OPTI=0 optirun %command%
OPTIRUN_FSR4=0 optirun %command%
OPTIRUN_RDNA3=0 optirun %command%
OPTIRUN_GAMEMODE=0 optirun %command%
OPTIRUN_DLL=winmm optirun %command%
```

## Notes

- `steam-session` refuses to run inside an existing graphical session.
- SDDM starts it from a minimal environment, so it prepends `~/.local/bin` and
  exports `MANGOHUD_CONFIGFILE` itself.
- Games with pre-launchers may need skip flags, e.g. Cyberpunk:
  `%command% --launcher-skip`.
- OptiScaler overlay is remapped from Insert to Home (`0x24`). If it drifts,
  run `fix-steam-games` to inspect or `fix-steam-games --apply` to fix.

## Quick reference

| Goal | How |
| --- | --- |
| HDR session | Steam (gamescope) in SDDM |
| Stream to handheld (1080p SDR, Sunshine) | Steam (gamescope stream) in SDDM |
| SDR desktop gaming | Sway; launch normally |
| OptiScaler/FSR4/GameMode for one game | `optirun %command%` |
