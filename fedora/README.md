# Fedora Setup

Fedora-specific bootstrap scripts plus `containers/`, `gtk-3.0/`, and `systemd/`
stow packages. Targets **Fedora Sway Atomic (Sericea)** — the rpm-ostree Sway
spin. Other Fedora variants are not supported.

The **gaming layer lives in its own quarantined namespace**,
[`gaming/`](./gaming/README.md). It is active by default on the main rig and
suppressed with `./dotfiles-sync --apply --skip-gaming` on work/laptop hosts, so
those get a pure Sway baseline with zero gaming footprint. Everything in *this*
README is baseline — installable on any Fedora Sway host, COPR-free.

## Setup Flow

Scripts split into two buckets by what they do:

- **`os/`** — package layering (`rpm-ostree`/`dnf`). Re-run these on a cadence:
  after a major-version rebase or when rebuilding the system clean. They need a
  reboot to take effect.
- `setup-mise.sh` (top level) — userland tool install via `mise`; no root, no
  reboot, no packages layered.

Order for a fresh install:

1. `os/setup-base.sh` — layer base packages (`rpm-ostree`).
2. `os/setup-sway.sh` — layer extra Sway session packages.
3. `setup-mise.sh` — install userland tools with `mise`.
4. `os/setup-toolbox.sh` (optional) — run inside a Fedora toolbox.

For gaming, streaming, RGB, and controller/hardware fixes, see
[`gaming/README.md`](./gaming/README.md).

## Package Lists

The `os/` setup scripts are thin wrappers around shared package arrays; all
call `rpm-ostree install`:

- `os/base-packages.sh` — minimal bootstrap + CLI tooling on top of Sericea.
- `os/sway-packages.sh` — extra desktop/session packages only (nothing Sericea
  already ships: sway, foot, kanshi, swaybg/idle/lock, waybar, wl-clipboard,
  pipewire, xdg-desktop-portal-wlr, etc.).

## Decisions

- The **baseline** (base + sway + mise) stays COPR-free and prefers stock Fedora
  repos. `google-chrome-stable` is the one exception (assumes Google's Chrome
  repo is enabled).
- The **gaming layer is a deliberate, scoped break** quarantined in
  [`gaming/`](./gaming/README.md); see
  [`docs/DECISIONS.md`](../docs/DECISIONS.md) for the partitioning rationale.
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

Baseline Fedora stow packages: `bin`, `containers`, `gtk-3.0`, `systemd`. From
the repo root, `./dotfiles-sync --apply` handles the Fedora-only logic. Manual
equivalent:

```bash
stow -d fedora -t ~ bin containers gtk-3.0 systemd
```

The gaming layer adds one more package (`gaming/home`), stowed by default and
skipped with `--skip-gaming`; see [`gaming/README.md`](./gaming/README.md).

## Containers And User Services

User-scoped Quadlet/systemd assets:

- `containers/.config/containers/systemd/postgres.container`
- `systemd/.config/systemd/user/` — `sway-session.target`,
  `sway-clipman-watcher`, `sway-kanshi`, `sway-mako`, `swaybg`, `swayidle`,
  `sway-waybar`, `toolbox-dev`, `ollama-toolbox` services.

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
