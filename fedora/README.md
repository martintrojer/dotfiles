# Fedora Setup

This directory holds Fedora-specific bootstrap scripts plus `containers/` and `systemd/` packages that are only stowed on Fedora-family systems.

This setup targets **Fedora Sway Atomic (Sericea)** — the rpm-ostree Sway spin. Mutable Workstation and other Fedora variants are not supported.

## Setup Flow

Typical order:

1. Run `fedora/setup-base.sh` to layer the base packages with `rpm-ostree`.
2. Run `fedora/setup-sway.sh` to layer the extra Sway session packages on top of Sericea with `rpm-ostree`.
3. Run `fedora/setup-mise.sh` to install userland tools with `mise`.
4. Optionally run `fedora/setup-toolbox.sh` inside a Fedora toolbox.

If you want the tmux session workflow from this repo, make sure `tmux`, `fzf`, `zoxide`, `fd`, and `eza` are installed too.

For non-Fedora toolboxes:

- `fedora/setup-toolbox-ubuntu.sh`
- `fedora/setup-toolbox-arch.sh`

## Package Lists

The main Fedora setup scripts are intentionally thin wrappers around shared package lists:

- `base-packages.sh`: common bootstrap and CLI tooling layered on top of the Sericea base.
- `sway-packages.sh`: extra desktop/session packages this setup layers on top of the Sericea Sway session.

This keeps the package decisions in one place. Both wrappers call `rpm-ostree install`.

## Decisions

The package split reflects a few explicit decisions:

- The Fedora host setup is intended to stay COPR-free, and package choices should prefer what is available in standard Fedora repositories.
- `base-packages.sh` is meant to be a viable minimal bootstrap baseline, not a full daily-driver package set.
- `mise` is a core bootstrap tool, so the host base keeps a small native build toolchain: `binutils`, `gcc`, `gcc-c++`, and `make`.
- `git`, `git-lfs`, `ripgrep`, `stow`, `tmux`, and `zsh` are treated as common baseline tooling.
- `btop` and `gdu` are still in base as shared comfort tools.
- The tmux and zsh session-launch flow uses local scripts plus `fzf`, `zoxide`, `fd`, and `eza`, rather than a separate session-manager binary.
- Desktop/session packages live in `sway-packages.sh` as a single `sway_packages` array. The list only contains packages layered on top of Sericea — anything Sericea already ships (sway, swaybg, swayidle, swaylock, waybar, wl-clipboard, pipewire, etc.) is intentionally not duplicated here.
- `btop`, `gdu`, and `distrobox` do not fit the package split perfectly, but they are placed pragmatically based on how this setup is actually bootstrapped and used.
- Wallpapers are managed per machine with `wallpaper set <url-or-file>`, which stores files under `~/.local/share/wallpapers/` and restarts `swaybg.service`.

## GTK Theme

`fedora/gtk-3.0/.config/gtk-3.0/settings.ini` ships an `Adwaita-dark` default for GTK3 apps. Sway does not push a GTK theme on its own, so without this file GTK apps render in the system light default.

GTK4 apps follow `gsettings set org.gnome.desktop.interface color-scheme prefer-dark` instead of the `settings.ini` mechanism. If you notice GTK4 apps disagreeing with GTK3, run that gsetting once per machine; it is intentionally not stowed because it is per-user state, not config.

## Stow Notes

The Fedora stow packages under this directory are:

- `containers`
- `gtk-3.0`
- `systemd`

From the repo root, the helper script `./stow-all.py` handles Fedora-only stow logic automatically.

If you need to stow the Fedora packages directly:

```bash
stow -d fedora -t ~ containers gtk-3.0 systemd
```

## Containers And User Services

This directory also carries user-scoped container and systemd assets:

- `containers/.config/containers/systemd/postgres.container`
- `systemd/.config/systemd/user/sway-session.target`
- `systemd/.config/systemd/user/sway-clipman-watcher.service`
- `systemd/.config/systemd/user/sway-kanshi.service`
- `systemd/.config/systemd/user/sway-mako.service`
- `systemd/.config/systemd/user/swaybg.service`
- `systemd/.config/systemd/user/swayidle.service`
- `systemd/.config/systemd/user/sway-waybar.service`
- `systemd/.config/systemd/user/toolbox-dev.service`
- `systemd/.config/systemd/user/ollama-toolbox.service`

The intended flow is:

1. Stow `containers` and `systemd` into `~/.config/...`
2. Reload the user systemd manager
3. Enable and start the units you want

Typical commands:

```bash
./stow-all.py --apply
systemctl --user daemon-reload
systemctl --user enable --now postgres.service
systemctl --user enable --now toolbox-dev.service
systemctl --user enable --now ollama-toolbox.service
```

Notes:

- `postgres.container` is a Quadlet file under `~/.config/containers/systemd/`, so systemd generates `postgres.service` from it after reload.
- `sway-session.target` is started by `~/.config/sway/scripts/session-start`, after importing the Sway session environment. It owns the desktop services, including `sway-waybar.service` for `waybar/config.jsonc` and `swayidle.service` for Sway monitor power commands.
- Desktop daemons tied to `sway-session.target` follow the same lifecycle and are stopped by `~/.config/sway/scripts/session-quit`.
- If you change `*.service` or `*.container` files later, run `systemctl --user daemon-reload` again before restarting them.
- `toolbox-dev.service` assumes a toolbox named `dev` already exists.
- `ollama-toolbox.service` assumes a toolbox named `ollama` exists and that `ollama` is installed inside it.
