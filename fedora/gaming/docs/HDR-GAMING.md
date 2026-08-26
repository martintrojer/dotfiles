# HDR gaming on Fedora Sway Atomic

Box: AMD RX 7800 XT + HDR LG 4K on `DP-1`.

## Model

Use these environments:

- **Sway desktop**: light SDR gaming. Launch games normally. No nested gamescope.
- **Steam (gamescope) SDDM session**: HDR. gamescope uses the DRM backend,
  owns KMS/DP-1, starts Steam Big Picture, and draws MangoHud with `--mangoapp`.
- **Streaming to a handheld**: Steam Remote Play, from either environment. It
  negotiates the capture down to the client's resolution limit, so the host's
  mode does not affect the stream.

Sway 1.11 lacks the color-management protocol needed for HDR. For HDR, log
into the gamescope session instead of nesting gamescope inside Sway. Background:
[gamescope#2008](https://github.com/ValveSoftware/gamescope/issues/2008),
[ArchWiki HDR](https://wiki.archlinux.org/title/HDR_monitor_support).

## Session

HDR runs in the **Steam (gamescope)** SDDM session. Installing it, entering it,
and the "Desktop Mode" exit are covered in
[GAMESCOPE-SESSION.md](./GAMESCOPE-SESSION.md).

## HDR env

`steam-session` is intentionally narrow:

```bash
gamescope --backend drm -W 3840 -H 2160 -r 60 --hdr-enabled -f --mangoapp -e -- \
  env DXVK_HDR=1 ENABLE_GAMESCOPE_WSI=1 steam -gamepadui
```

It owns/enforces:

- `--backend drm`: direct KMS/display ownership.
- `--hdr-enabled`: HDR10 output.
- `DXVK_HDR=1 ENABLE_GAMESCOPE_WSI=1`: Vulkan/Proton HDR through gamescope WSI.
- `--mangoapp`: session MangoHud overlay.

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
- `gamemoderun` when installed

## Notes

- Games with pre-launchers may need skip flags, e.g. Cyberpunk:
  `%command% --launcher-skip`.
- `optiscaler-sync` previews bulk installation/update and
  `optiscaler-sync --apply` performs it, including remapping the OptiScaler
  overlay from Insert to Home (`0x24`).

## Quick reference

| Goal | How |
| --- | --- |
| HDR session | Steam (gamescope) in SDDM |
| Stream to handheld | Steam Remote Play from any session |
| SDR desktop gaming | Sway; launch normally |
| OptiScaler/FSR4/GameMode for one game | `optirun %command%` |
