# Fedora Gaming Layer

The main rig's gaming, streaming, and RGB stack lives in a separate namespace
so the [Fedora baseline](../README.md) remains a COPR-free Sway setup.

The module is active by default on Fedora hosts. Suppress it with
`./dotfiles-sync --apply --skip-gaming` on work or laptop machines. The main rig
needs no flag. See the
partitioning rationale in [`../../docs/DECISIONS.md`](../../docs/DECISIONS.md).

This layer is the scoped exception to the baseline's COPR-free and minimal
overlay rules. It adds RPM Fusion, the Sunshine COPR, and graphical packages
that do not belong in the baseline. [`docs/DECISIONS.md`](./docs/DECISIONS.md)
records the history.

## Layout

- **`os/`** — package layering (`rpm-ostree`). Needs RPM Fusion enabled first.
- **`config/`** — one-shot config installers (udev rules, systemd units, logind
  drop-ins, firewall ports, SDDM sessions, controller firmware). Run once after
  a clean install; they don't layer packages and rarely need re-running.
- **`data/`** — tracked tool policy read at runtime:
  `data/optiscaler-overrides.json` contains reviewed AppID target overrides.
- **`docs/`** — [gamescope session](./docs/GAMESCOPE-SESSION.md),
  [HDR gaming](./docs/HDR-GAMING.md), and [streaming](./docs/STREAMING.md)
  procedures.
- **`home/`** — the single package. Everything gaming that lands in `$HOME`
  (bin helpers, GameMode config, the Sunshine unit override). Linked
  by default, skipped with `--skip-gaming`.

## Setup Flow

Order for a fresh install (after the baseline `os/` + `setup-mise.sh`):

1. Enable **RPM Fusion** (free + nonfree) and the **Sunshine COPR** — see the
   header of [`os/steam-packages.sh`](./os/steam-packages.sh) for the exact
   repo-setup commands.
2. `os/setup-steam.sh` — layer gaming/Steam packages.
3. `config/setup-gamescope-session.sh` — install the "Steam (gamescope)"
   embedded session selectable at SDDM (see
   [`docs/GAMESCOPE-SESSION.md`](./docs/GAMESCOPE-SESSION.md)).
4. `config/setup-openrgb.sh` — wire i2c/SMBus access for OpenRGB (loads
   `i2c-dev`, creates the `i2c` group + udev rule, adds you to it). Needs the
   `openrgb` rpm from step 2. See "OpenRGB / RGB" below.

Other `config/` installers, run as needed: `setup-power-key.sh` (power button
suspends), `setup-steam-pause.sh` (pause games across suspend),
`setup-wake-usb.sh` (only the power button wakes the tower), `setup-sunshine.sh`
(open Sunshine ports to the configured wired LAN only), `setup-bt-firmware.sh`
(fix Xbox controller BT
drops; see "Bluetooth Controller" below).

## Package List

`os/steam-packages.sh` — gaming/Steam packages (single `steam_packages` array),
gated behind RPM Fusion + the Sunshine COPR. `os/setup-steam.sh` is a thin
wrapper that sources the array and calls `rpm-ostree install`.

`gamemode` and `7zip` are not listed even though this stack
uses both: the Sericea base image already ships them, and naming a base-image
package makes `rpm-ostree install` fail with "already provided by" and layer
_none_ of the array. `tests/test_steam_packages.py` asserts they stay out. If a
future rebase drops either one, add it back along with `--allow-inactive` in
`setup-steam.sh`.

## bin Helpers

The `home/.local/bin/` wrappers are on `$PATH` for the whole graphical session
(not just interactive shells) via
`fedora/systemd/.config/environment.d/10-local-bin.conf`. The embedded "Steam
(gamescope)" SDDM session is launched via a non-interactive login shell that
sources neither `~/.zshrc` nor `environment.d`, so `steam-session` prepends
`~/.local/bin` to PATH itself before exec'ing gamescope.

Gaming modes:

- **Sway desktop**: light SDR gaming. Launch games normally.
- **Steam (gamescope) SDDM session**: HDR gaming. `steam-session` owns the
  display via gamescope DRM, enables HDR, shows the `--mangoapp` overlay, and
  exports only HDR WSI env: `DXVK_HDR=1 ENABLE_GAMESCOPE_WSI=1`.
- **Steam (gamescope stream) SDDM session**: same launcher with
  `GS_OUT_W=1920 GS_OUT_H=1080 GS_HDR=0 GS_SUNSHINE=1` for streaming to a
  handheld via Sunshine. See [docs/STREAMING.md](./docs/STREAMING.md).
- **Per-game OptiScaler/FSR4/GameMode**: use `optirun %command%`. It sets
  `WINEDLLOVERRIDES=dxgi=n,b` and `PROTON_FSR4_UPGRADE=1`, then runs the game
  via `gamemoderun` when available.
- `steam-session` refuses to run inside another graphical session, prepends
  `~/.local/bin` to PATH, and accepts
  `GS_OUT_W/H`, `GS_REFRESH`, `GS_HDR`, `GS_ARGS`. See
  [docs/GAMESCOPE-SESSION.md](./docs/GAMESCOPE-SESSION.md) for the session and
  [docs/HDR-GAMING.md](./docs/HDR-GAMING.md) for the HDR env.
- `steam-pause {pause,resume,list}` finds running Steam games (their
  `reaper ... AppId=` processes), walks each child tree, and `SIGSTOP`/`SIGCONT`s
  the children (leaving the reaper alive so Steam doesn't see the game as
  exited). Extracted from the SDH-PauseGames Decky plugin, no Decky needed.
  `config/setup-steam-pause.sh` copies it to `/usr/local/bin` and enables a
  oneshot unit (`config/systemd-system/steam-pause-games.service`) ordered around
  `sleep.target` that runs `pause` before suspend and `resume` on wake,
  avoiding crackling audio and frozen emulators in the "Steam (gamescope)"
  session. A `system-sleep` hook can't be used (Atomic `/usr/lib` is read-only
  and `systemd-sleep` reads only that dir); the script is copied, not symlinked
  to `~/.local/bin`, since the root unit can't exec under `$HOME` (SELinux
  `user_home_t`, 203/EXEC). Stopped games hold RAM/VRAM, so best for short
  suspends.
- `config/setup-power-key.sh` installs a global logind drop-in so the power button
  suspends: short press = sleep, long press = power off. Applies to all sessions
  (Sway, gamescope, SDDM greeter). Pairs with the pause unit and with Steam's
  own "Suspend" menu item (same `systemctl suspend` path).
- `config/setup-wake-usb.sh` installs a udev rule
  (`config/wake-usb/99-disable-usb-wakeup.rules`) pinning `power/wakeup=disabled`
  for the desktop keyboard/mouse (and BT dongle) by USB vendor:product ID, so
  only the power button wakes the tower. A bumped mouse will not. The IDs remain
  stable across reboots and reconnections; add devices by editing the rule.

OptiScaler sync:

- `optiscaler-sync` discovers every installed Steam game containing a recognized
  DLSS, XeSS, or FidelityFX upscaler DLL and reports what it would install or
  update. It is a dry run unless `--apply` is passed.
- `optiscaler-sync --apply` downloads the latest stable official
  `optiscaler/OptiScaler` release, requires its GitHub API SHA-256 digest,
  validates and extracts the `.7z` with Fedora's `7zip`, then installs its full
  payload for every unambiguous game. Multiple candidate directories are
  skipped unless `data/optiscaler-overrides.json` selects a reviewed target;
  Talos Principle 2 is explicitly skipped pending one.
- Each install owns `.optiscaler-sync/manifest.json`, backs up pre-existing
  collisions with verified backup digests, preserves a game's existing
  `OptiScaler.ini`, sets only `ShortcutKey=0x24` and `Fsr4Update=auto`, and ensures
  the Steam launch option
  contains `optirun %command%` without dropping suffix arguments.
- `optiscaler-sync uninstall` previews removal for every managed game;
  `optiscaler-sync uninstall --apply` restores verified backups and refuses
  externally modified managed files or launch options. Add `--force` only when
  those files should be replaced or removed. Mutation always refuses to run while
  Steam is running.
- A failed game transaction rolls back that game and does not stop the remaining
  games. Run with all games closed so they cannot race INI or DLL updates.

## MangoHud

Installed via the `mangohud` package in `os/steam-packages.sh`. No config is
tracked in this repo.

- Enable per-game by adding `mangohud %command%` to a title's Steam launch
  options, or run `mangohud <program>` directly. These use MangoHud's own
  defaults / your ad-hoc `~/.config/MangoHud/MangoHud.conf` if you make one.
- In the Steam (gamescope) session, gamescope shows the overlay with
  `--mangoapp`, and in SteamOS game mode (`-steamos3`) Steam **owns** it: the
  Quick Access "Performance" panel drives mangoapp over its control socket and
  rewrites `~/.config/MangoHud/MangoHud.conf` at runtime. That's why the config
  isn't linked — Steam would clobber it every session.

## GameMode (Feral)

Config: `home/.config/gamemode.ini` → `~/.config/gamemode.ini`.

- GameMode reads `$XDG_CONFIG_HOME/gamemode.ini`, not
  `~/.config/gamemode/gamemode.ini`.
- Installed via the `gamemode` package in `os/steam-packages.sh`.
- Use directly with `gamemoderun %command%`, or `optirun %command%` for the
  OptiScaler/FSR4/GameMode bundle.

The local policy in `gamemode.ini` enables:

- `renice=10` — game process nice `-10`.
- `ioprio=0` — best-effort I/O priority 0.
- custom hooks: start `tuned-adm profile throughput-performance`; end
  `tuned-adm profile balanced`.

`tuned-adm` is used because tuned owns CPU governor policy on this host.
GameMode's native `desiredgov`/`desiredprof` are left unset. Hooks run as the
user and intentionally do not use `sudo`; polkit authorizes the active local
session over D-Bus.

**gamemode group on Fedora Atomic/Silverblue.** `renice=10` needs `gamemode`
group membership. On rpm-ostree systems the group may exist in `/usr/lib/group`
but not `/etc/group`, so copy it before `usermod`:

```bash
grep -E '^gamemode:' /usr/lib/group | sudo tee -a /etc/group
sudo usermod -aG gamemode "$USER"
systemctl reboot
```

Verify: `gamemoded -t`, then `gamemoded -s` while a game is running. Caveat:
`[gpu]` settings such as `amd_performance_level` are ignored from the
user-local config.

## Bluetooth Controller

The TP-Link UB500 (Realtek RTL8761BU) ships firmware `0xdfc6d922`, which causes
mid-session Xbox controller disconnects ([xpadneo](https://atar-axis.github.io/xpadneo/)).
`config/setup-bt-firmware.sh` downgrades to the known-good `0x09a98a6b`:

- copies `config/firmware/rtl_bt/*.xz` (extracted from linux-firmware history,
  recompressed xz/CRC32) to `/etc/firmware/rtl_bt/` — `/usr/lib/firmware` is
  read-only on ostree.
- relabels them `lib_t` (SELinux Enforcing denies the loader reading `etc_t`).
- adds `firmware_class.path=/etc/firmware` via `rpm-ostree kargs` so the kernel
  searches there first (BT firmware loads at boot, before userspace).

Survives `rpm-ostree upgrade` since it lives in `/etc` + kargs, not `/usr`.
Reboot, then verify: `dmesg | grep 'RTL: fw version'` shows `0x09a98a6b`.
Revert: `sudo rpm-ostree kargs --delete=firmware_class.path=/etc/firmware &&
sudo rm -rf /etc/firmware/rtl_bt` (then reboot).

## OpenRGB / RGB

`openrgb` (from `os/steam-packages.sh`) controls motherboard / RAM / GPU RGB. To
reach those over SMBus it needs i2c access. Without
it the i2c nodes stay `root:root 0600` and OpenRGB sees no controllers.
`config/setup-openrgb.sh` wires up everything (run it after layering `openrgb`):

- `config/openrgb/i2c-dev.conf` → `/etc/modules-load.d/i2c-dev.conf`: load
  `i2c-dev` at boot so `/dev/i2c-*` nodes exist.
- creates the `i2c` system group and adds the invoking user to it.
- `config/openrgb/99-i2c.rules` → `/etc/udev/rules.d/99-i2c.rules`: give the
  `i2c` group `rw` on the i2c-dev nodes.

```bash
fedora/gaming/config/setup-openrgb.sh
# log out/in (or: newgrp i2c) so the group membership applies, then verify:
getent group i2c && ls -l /dev/i2c-*   # expect: root i2c, crw-rw----
```

**Turning lighting off (no boot service).** OpenRGB persists the lighting
*mode* into the GPU/board firmware, so a single command sticks across full
power cycles, so no `rgb.service` is needed. This box shipped with the GPU stuck in
a rainbow cycle; setting it to direct/off once fixed it permanently:

```bash
openrgb --list-devices              # find the device index
openrgb --device 0 --mode direct --color 000000
```

Run once. Re-run only if a BIOS update or "restore defaults" ever brings the
rainbow back.
