# Fedora Gaming Layer

The main rig's gaming/streaming/RGB stack, quarantined in its own namespace so
the [Fedora baseline](../README.md) stays a clean, COPR-free Sway setup.

**Opt-out module.** Active by default on Fedora hosts; suppress it with
`./dotfiles-sync --apply --skip-gaming` on work/laptop machines that want a pure
Sway baseline with zero gaming footprint. The main rig needs no flag. See the
partitioning rationale in [`../../docs/DECISIONS.md`](../../docs/DECISIONS.md).

This is a **deliberate, scoped break** from the baseline's COPR-free /
minimal-overlay rules: it layers RPM Fusion, a COPR (Sunshine), and graphical
packages that don't fit the minimal baseline. The break is intentional — the
history is in [`docs/DECISIONS.md`](./docs/DECISIONS.md) here.

## Layout

- **`os/`** — package layering (`rpm-ostree`). Needs RPM Fusion enabled first.
- **`config/`** — one-shot config installers (udev rules, systemd units, logind
  drop-ins, firewall ports, SDDM sessions, controller firmware). Run once after
  a clean install; they don't layer packages and rarely need re-running.
- **`data/`** — committed tool state read at runtime, not scripts to run:
  `data/optiscaler-client-seed/` (first-run prefs the `optiscaler-client`
  helper seeds into `~/.config`).
- **`docs/`** — [HDR gaming](./docs/HDR-GAMING.md) and
  [streaming](./docs/STREAMING.md) procedures.
- **`home/`** — the single stow package. Everything gaming that lands in `$HOME`
  (bin helpers, MangoHud/GameMode configs, the Sunshine unit override). Stowed
  by default, skipped with `--skip-gaming`.

## Setup Flow

Order for a fresh install (after the baseline `os/` + `setup-mise.sh`):

1. Enable **RPM Fusion** (free + nonfree) and the **Sunshine COPR** — see the
   header of [`os/steam-packages.sh`](./os/steam-packages.sh) for the exact
   repo-setup commands.
2. `os/setup-steam.sh` — layer gaming/Steam packages.
3. `config/setup-gamescope-session.sh` — install the "Steam (gamescope)"
   embedded HDR session selectable at SDDM (see
   [`docs/HDR-GAMING.md`](./docs/HDR-GAMING.md)).
4. `config/setup-openrgb.sh` — wire i2c/SMBus access for OpenRGB (loads
   `i2c-dev`, creates the `i2c` group + udev rule, adds you to it). Needs the
   `openrgb` rpm from step 2. See "OpenRGB / RGB" below.

Other `config/` installers, run as needed: `setup-power-key.sh` (power button
suspends), `setup-steam-pause.sh` (pause games across suspend),
`setup-wake-usb.sh` (only the power button wakes the tower), `setup-sunshine.sh`
(open Sunshine firewall ports), `setup-bt-firmware.sh` (fix Xbox controller BT
drops — see "Bluetooth Controller" below).

## Package List

`os/steam-packages.sh` — gaming/Steam packages (single `steam_packages` array),
gated behind RPM Fusion + the Sunshine COPR. `os/setup-steam.sh` is a thin
wrapper that sources the array and calls `rpm-ostree install`.

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
  display via gamescope DRM, enables HDR, draws MangoHud (`--mangoapp`), and
  exports only HDR WSI env: `DXVK_HDR=1 ENABLE_GAMESCOPE_WSI=1`.
- **Steam (gamescope stream) SDDM session**: same launcher with
  `GS_OUT_W=1920 GS_OUT_H=1080 GS_HDR=0 GS_SUNSHINE=1` for streaming to a
  handheld via Sunshine. See [docs/STREAMING.md](./docs/STREAMING.md).
- **Per-game OptiScaler/FSR4/GameMode**: use `optirun %command%`. It sets
  `WINEDLLOVERRIDES=dxgi=n,b`, `PROTON_FSR4_UPGRADE=1`,
  `DXIL_SPIRV_CONFIG=wmma_rdna3_workaround`, then runs the game via
  `gamemoderun`. Opt out with `OPTIRUN_OPTI=0`, `OPTIRUN_FSR4=0`,
  `OPTIRUN_RDNA3=0`, or `OPTIRUN_GAMEMODE=0`; use `OPTIRUN_DLL=<name>` for a
  non-`dxgi` proxy DLL.
- `steam-session` refuses to run inside another graphical session, prepends
  `~/.local/bin` to PATH, exports `MANGOHUD_CONFIGFILE`, and accepts
  `GS_OUT_W/H`, `GS_REFRESH`, `GS_HDR`, `GS_ARGS`. See
  [docs/HDR-GAMING.md](./docs/HDR-GAMING.md).
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
  only the power button wakes the tower — a bumped mouse won't. Matched by ID
  so it survives reboots and re-plugging; add devices by editing the rule.

OptiScaler manager (GUI):

- `optiscaler-client` installs/updates/runs upstream OptiScaler Client into
  `~/.local/share/optiscaler-client/` (binary not vendored). Commands: `run`,
  `update [--tag TAG] [--force]`, `status`, `path`.
- `optiscaler-client.desktop` exposes it to `drun` launchers.
- `optirun %command%` loads installed proxy DLLs for a game; override manually
  with `WINEDLLOVERRIDES=... %command%` if needed.
- First run seeds stable prefs from `data/optiscaler-client-seed/` only when
  missing. Private/volatile state (`games.json`, `Cache/`, geometry, timestamps)
  stays untracked.
- `fix-steam-games` checks/fixes OptiScaler games in three passes: `ShortcutKey`
  (Home / `0x24`), Steam launch options (`optirun %command%`, clearing stale
  wrappers), and the FSR 4 backend. Dry-run by default; `--apply` to write
  (refuses while Steam runs unless `--force`). Scope with
  `--only {shortcut,launch-options,fsr4}`; override the key with `--key 0xNN`.
  Run with games closed so an in-game "Save" can't race INI edits.
- **FSR4 pass** (`--only fsr4`): harvests the newest `amdxcffx64.dll` from
  installed Proton (nothing downloaded/vendored, so newer Proton → newer FSR),
  caches it under `~/.local/share/fsr4-backend/<version>/`, drops it next to
  each game's OptiScaler, and sets `Fsr4Update=true` so the version (e.g.
  4.1.1) shows in the OptiScaler menu (`--watermark` also burns it into the
  frame). Only touches complete OptiScaler installs; edits `OptiScaler.ini` in
  place, writing only keys that version already knows (future flags degrade
  gracefully). Also detects the client's stale "Extras"
  `amd_fidelityfx_upscaler_dx12.dll` (e.g. 4.0.2c) — via hash against
  `~/.config/OptiscalerClient/Cache/Extras/*/` and embedded FSR version — and
  restores OptiScaler's clean DLL from `Cache/OptiScaler/<ver>/`, keeping the
  upscaler DLL and backend a matched pair. `--revert` restores both DLLs from
  `.bak` and resets flags to `auto`.

## MangoHud

Config: `home/.config/MangoHud/MangoHud.conf` → `~/.config/MangoHud/MangoHud.conf`.

- Installed via the `mangohud` package in `os/steam-packages.sh`.
- Enable per-game by adding `mangohud %command%` to a title's Steam launch
  options, or run `mangohud <program>` directly.
- Default in-overlay toggle is `Shift_R+F12`.
- In the Steam (gamescope) session, gamescope draws this config with
  `--mangoapp`. The `gamemode` widget is intentionally disabled there: `optirun`
  applies GameMode to the game process, but mangoapp is session-level and would
  report its own context instead.

## GameMode (Feral)

Config: `home/.config/gamemode.ini` → `~/.config/gamemode.ini`.

- GameMode reads `$XDG_CONFIG_HOME/gamemode.ini`, not
  `~/.config/gamemode/gamemode.ini`.
- Installed via the `gamemode` package in `os/steam-packages.sh`.
- Use directly with `gamemoderun %command%`, or `optirun %command%` for the
  OptiScaler/FSR4/GameMode bundle.

**Local policy.** Enabled in `gamemode.ini`:

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
reach those over SMBus it needs i2c access, which is the common gotcha — without
it the i2c nodes stay `root:root 0600` and OpenRGB sees no controllers.
`config/setup-openrgb.sh` wires up everything (run it after layering `openrgb`):

- `config/openrgb/i2c-dev.conf` → `/etc/modules-load.d/i2c-dev.conf` — load
  `i2c-dev` at boot so `/dev/i2c-*` nodes exist.
- creates the `i2c` system group and adds the invoking user to it.
- `config/openrgb/99-i2c.rules` → `/etc/udev/rules.d/99-i2c.rules` — give the
  `i2c` group `rw` on the i2c-dev nodes.

```bash
fedora/gaming/config/setup-openrgb.sh
# log out/in (or: newgrp i2c) so the group membership applies, then verify:
getent group i2c && ls -l /dev/i2c-*   # expect: root i2c, crw-rw----
```

**Turning lighting off (no boot service).** OpenRGB persists the lighting
*mode* into the GPU/board firmware, so a single command sticks across full
power cycles — no `rgb.service` needed. This box shipped with the GPU stuck in
a rainbow cycle; setting it to direct/off once fixed it permanently:

```bash
openrgb --list-devices              # find the device index
openrgb --device 0 --mode direct --color 000000
```

Run once. Re-run only if a BIOS update or "restore defaults" ever brings the
rainbow back.
