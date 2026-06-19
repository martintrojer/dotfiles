# Fedora Setup

Fedora-specific bootstrap scripts plus `containers/` and `systemd/` stow packages.
Targets **Fedora Sway Atomic (Sericea)** — the rpm-ostree Sway spin. Other Fedora
variants are not supported.

## Setup Flow

1. `setup-base.sh` — layer base packages (`rpm-ostree`).
2. `setup-sway.sh` — layer extra Sway session packages.
3. `setup-mise.sh` — install userland tools with `mise`.
4. `setup-steam.sh` (optional) — gaming/Steam packages; needs RPM Fusion + the
   LACT COPR enabled first (see `steam-packages.sh`).
5. `setup-gamescope-session.sh` (optional) — install the "Steam (gamescope)"
   embedded HDR session selectable at SDDM (see `docs/HDR-GAMING.md`).
6. `setup-openrgb.sh` (optional) — wire i2c/SMBus access for OpenRGB (loads
   `i2c-dev`, creates the `i2c` group + udev rule, adds you to it). Needs the
   `openrgb` rpm from `setup-steam.sh`. See "OpenRGB / RGB" below.
7. `setup-toolbox.sh` (optional) — run inside a Fedora toolbox.

## Package Lists

Setup scripts are thin wrappers around shared package arrays; all call
`rpm-ostree install`:

- `base-packages.sh` — minimal bootstrap + CLI tooling on top of Sericea.
- `sway-packages.sh` — extra desktop/session packages only (nothing Sericea
  already ships: sway, foot, kanshi, swaybg/idle/lock, waybar, wl-clipboard,
  pipewire, xdg-desktop-portal-wlr, etc.).
- `steam-packages.sh` — gaming/Steam packages (single `steam_packages` array),
  gated behind RPM Fusion + the LACT COPR. A deliberate break from the
  COPR-free baseline; see [`docs/DECISIONS.md`](../docs/DECISIONS.md).

GPU tuning state lives in `lact/config.yaml` (a committed snapshot of the live
`/etc/lact/config.yaml`; not stowed, since the daemon rewrites the system file).
`dotfiles-sync --check` flags drift between the two — see
[`docs/LACT.md`](./docs/LACT.md#repo-snapshot--drift).

Related docs: GPU tuning with LACT in [`docs/LACT.md`](./docs/LACT.md); gaming
session/per-game helpers (`steam-session`, `optirun`, `optiscaler-client`) in
[`bin/README.md`](./bin/README.md); HDR gaming in
[`docs/HDR-GAMING.md`](./docs/HDR-GAMING.md).

## Decisions

- The **baseline** (base + sway + mise) stays COPR-free and prefers stock Fedora
  repos. `google-chrome-stable` is the one exception (assumes Google's Chrome
  repo is enabled).
- The **gaming layer is a deliberate, scoped break**: `steam-packages.sh` layers
  RPM Fusion + the `ilyaz/LACT` COPR plus graphical packages that don't fit the
  minimal baseline. Kept in its own array so non-gaming hosts never pull it in.
  Enable RPM Fusion + LACT COPR manually before `setup-steam.sh`. See
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

`openrgb` (from `steam-packages.sh`) controls motherboard / RAM / GPU RGB. To
reach those over SMBus it needs i2c access, which is the common gotcha — without
it the i2c nodes stay `root:root 0600` and OpenRGB sees no controllers.
`setup-openrgb.sh` wires up everything (run it after layering `openrgb`):

- `openrgb/i2c-dev.conf` → `/etc/modules-load.d/i2c-dev.conf` — load `i2c-dev`
  at boot so `/dev/i2c-*` nodes exist.
- creates the `i2c` system group and adds the invoking user to it.
- `openrgb/99-i2c.rules` → `/etc/udev/rules.d/99-i2c.rules` — give the `i2c`
  group `rw` on the i2c-dev nodes.
- `openrgb/rgb.service` → `/etc/systemd/system/rgb.service` — a **system**
  oneshot that sets the color on boot (the OpenRGB analog of `lactd`, not a
  user/login service). Edit `--color` in the unit; `000000` turns lighting off.

```bash
fedora/setup-openrgb.sh
# log out/in (or: newgrp i2c) so the group membership applies, then verify:
getent group i2c && ls -l /dev/i2c-*   # expect: root i2c, crw-rw----
```
