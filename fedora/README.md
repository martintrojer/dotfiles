# Fedora Setup

Fedora-specific bootstrap scripts plus `containers/` and `systemd/` stow packages.
Targets **Fedora Sway Atomic (Sericea)** — the rpm-ostree Sway spin. Other Fedora
variants are not supported.

## Setup Flow

Scripts split into two buckets by what they do:

- **`os/`** — package layering (`rpm-ostree`/`dnf`). Re-run these on a cadence:
  after a major-version rebase or when rebuilding the system clean. They need a
  reboot to take effect.
- **`config/`** — one-shot config installers (udev rules, systemd units, logind
  drop-ins, firewall ports, SDDM sessions). Run once after a clean install;
  they don't layer packages and rarely need re-running.
- `setup-mise.sh` (top level) — userland tool install via `mise`; no root, no
  reboot, no packages layered.
- **`data/`** — committed tool state read at runtime, not scripts to run:
  `data/optiscaler-client-seed/` (first-run prefs the `optiscaler-client`
  helper seeds into `~/.config`).

Order for a fresh install:

1. `os/setup-base.sh` — layer base packages (`rpm-ostree`).
2. `os/setup-sway.sh` — layer extra Sway session packages.
3. `setup-mise.sh` — install userland tools with `mise`.
4. `os/setup-steam.sh` (optional) — gaming/Steam packages; needs RPM Fusion
   enabled first (see `os/steam-packages.sh`).
5. `config/setup-gamescope-session.sh` (optional) — install the "Steam
   (gamescope)" embedded HDR session selectable at SDDM (see
   `docs/HDR-GAMING.md`).
6. `config/setup-openrgb.sh` (optional) — wire i2c/SMBus access for OpenRGB
   (loads `i2c-dev`, creates the `i2c` group + udev rule, adds you to it).
   Needs the `openrgb` rpm from `os/setup-steam.sh`. See "OpenRGB / RGB" below.
7. `os/setup-toolbox.sh` (optional) — run inside a Fedora toolbox.

Other `config/` installers, run as needed: `setup-power-key.sh` (power button
suspends), `setup-steam-pause.sh` (pause games across suspend),
`setup-wake-usb.sh` (only the power button wakes the tower), `setup-sunshine.sh`
(open Sunshine firewall ports). See `bin/README.md` for the first three.

## Package Lists

The `os/` setup scripts are thin wrappers around shared package arrays; all
call `rpm-ostree install`:

- `os/base-packages.sh` — minimal bootstrap + CLI tooling on top of Sericea.
- `os/sway-packages.sh` — extra desktop/session packages only (nothing Sericea
  already ships: sway, foot, kanshi, swaybg/idle/lock, waybar, wl-clipboard,
  pipewire, xdg-desktop-portal-wlr, etc.).
- `os/steam-packages.sh` — gaming/Steam packages (single `steam_packages` array),
  gated behind RPM Fusion. A deliberate break from the COPR-free baseline; see
  [`docs/DECISIONS.md`](../docs/DECISIONS.md).

Related docs: gaming
session/per-game helpers (`steam-session`, `optirun`, `optiscaler-client`) in
[`bin/README.md`](./bin/README.md); HDR gaming in
[`docs/HDR-GAMING.md`](./docs/HDR-GAMING.md).

## Decisions

- The **baseline** (base + sway + mise) stays COPR-free and prefers stock Fedora
  repos. `google-chrome-stable` is the one exception (assumes Google's Chrome
  repo is enabled).
- The **gaming layer is a deliberate, scoped break**: `steam-packages.sh` layers
  RPM Fusion plus graphical packages that don't fit the minimal baseline. Kept
  in its own array so non-gaming hosts never pull it in. Enable RPM Fusion
  manually before `os/setup-steam.sh`. See
  [`docs/DECISIONS.md`](../docs/DECISIONS.md).
- `mise` is core bootstrap, so the base keeps a small build toolchain
  (`binutils`, `gcc`, `gcc-c++`, `make`). `git`, `git-lfs`, `stow`, `tmux`, `zsh`
  are baseline; comfort CLIs that don't need host-layering live in
  `setup-mise.sh`.
- The tmux/zsh session flow uses local scripts + `fzf`, `zoxide`, `fd`, `eza`
  rather than a session-manager binary.
- Wallpapers: `wallpaper set <url-or-file>` (stores under
  `~/.local/share/wallpapers/`, restarts `swaybg.service`).

## GTK Theme

`gtk-3.0/.config/gtk-3.0/settings.ini` ships an `Adwaita-dark` default for GTK3
apps (Sway doesn't push a GTK theme). GTK4 apps instead follow
`gsettings set org.gnome.desktop.interface color-scheme prefer-dark` — run once
per machine if GTK4 apps disagree with GTK3; it's per-user state, not stowed.

## Stow Packages

Fedora stow packages: `containers`, `gtk-3.0`, `systemd`. From the repo root,
`./dotfiles-sync --apply` handles the Fedora-only logic. Manual equivalent:

```bash
stow -d fedora -t ~ containers gtk-3.0 systemd
```

## Containers And User Services

User-scoped Quadlet/systemd assets:

- `containers/.config/containers/systemd/postgres.container`
- `systemd/.config/systemd/user/` — `sway-session.target`,
  `sway-clipman-watcher`, `sway-kanshi`, `sway-mako`, `swaybg`, `swayidle`,
  `sway-waybar`, `toolbox-dev`, `ollama-toolbox` services.

(RGB runs as a **system** service, not a user one — see "OpenRGB / RGB" below.)

Flow: stow → reload → enable units:

```bash
./dotfiles-sync --apply
systemctl --user daemon-reload
systemctl --user enable --now postgres.service
systemctl --user enable --now toolbox-dev.service
systemctl --user enable --now ollama-toolbox.service
```

Notes:

- `postgres.container` is a Quadlet file; systemd generates `postgres.service`
  from it after reload.
- `sway-session.target` is started by `~/.config/sway/scripts/session-start`
  (after importing the Sway session env) and owns the desktop services
  (waybar, mako, swayidle, …); `session-quit` stops them. Vendor user units for
  `mako`, `waybar`, `kanshi`, `foot-server` are masked by `dotfiles-sync --apply`
  so D-Bus activation can't start duplicates.
- Re-run `systemctl --user daemon-reload` after editing `*.service`/`*.container`.
- `toolbox-dev.service` / `ollama-toolbox.service` assume toolboxes named `dev` /
  `ollama` already exist (with `ollama` installed inside the latter).

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
- `config/openrgb/rgb.service` → `/etc/systemd/system/rgb.service` — a
  **system** oneshot that sets the color on boot (not a user/login service).
  Edit `--color` in the unit; `000000` turns lighting off.

```bash
fedora/config/setup-openrgb.sh
# log out/in (or: newgrp i2c) so the group membership applies, then verify:
getent group i2c && ls -l /dev/i2c-*   # expect: root i2c, crw-rw----
```
