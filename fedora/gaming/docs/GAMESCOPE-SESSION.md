# Gamescope SDDM session

This SDDM session runs Steam's gamepad UI under gamescope on the DRM backend. It
provides HDR ([HDR-GAMING.md](./HDR-GAMING.md)).

## Install

Run `config/setup-gamescope-session.sh` after `os/setup-steam.sh` and
`dotfiles-sync --apply`. It symlinks the linked helpers and session entries into
system paths (writable + persistent on Atomic via `/usr/local -> /var/usrlocal`,
which SDDM already searches):

- `/usr/local/bin/steam-session` → linked `~/.local/bin/steam-session`
- `/usr/local/bin/steamos-session-select` → linked exit shim
- `/usr/local/share/wayland-sessions/steam.desktop` (couch HDR)

## Enter

Pick **Steam (gamescope)** at the SDDM login screen. `steam-session` refuses to
run inside an existing graphical session, so it's SDDM-only, not launchable from
Sway.

## Exit — "Desktop Mode"

Steam's gamepad UI has no plain "Exit Steam"; the way out is the power menu's
**Desktop Mode**. That runs `steamos-session-select` and *blocks until it
returns*. We have no desktop-in-gamescope target, so the shim just tears the
session down and drops back to the SDDM greeter.

This works because `steam-session` launches `steam -steamos3`. The
Desktop Mode handler only runs the `steamos-session-select` exec when Steam
believes it is in SteamOS game mode (`Plat_IsSteamOSGameMode`). With `-gamepadui`
alone the button opens its modal but execs nothing and hangs. We deliberately
do not pass `-steamdeck`, the handheld battery, OSK, and fan profile. That profile
is wrong for a couch Steam Machine; `-steamos3` alone gates the exec path.

Two traps the shim handles:

- **Deadlock:** A synchronous `steam -shutdown` would hang because Steam cannot quit
  while blocked waiting for the shim to return. The shim detaches the teardown
  (`setsid --fork`) and returns 0 immediately, unblocking the gamepad UI.
- **Lingering compositor:** gamescope runs in embed mode (`-e`), so
  it normally exits when its Steam child dies. As a fallback, the
  shim also `pkill`s gamescope by full command line if Steam lingers (its `comm`
  is truncated to `gamescope-wl`, so `pkill -x gamescope` would miss).

The shim lives in `/usr/local/bin` (always on the system PATH) rather than
relying on `~/.local/bin` being inherited by the PATH the gamepad UI uses to
resolve `steamos-session-select`.
