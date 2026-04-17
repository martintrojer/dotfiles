# Fedora Setup

This directory holds Fedora-specific bootstrap scripts plus `containers/` and `systemd/` packages that are only stowed on Fedora-family systems.

## Setup Flow

Typical order:

1. Run `fedora/setup-base.sh` on rpm-ostree systems, or `fedora/setup-base-workstation.sh` on mutable Fedora installs.
2. Run `fedora/setup-niri.sh` on rpm-ostree systems, or `fedora/setup-niri-workstation.sh` on mutable Fedora installs.
3. Run `fedora/setup-mise.sh` to install userland tools with `mise`.
4. Optionally run `fedora/setup-toolbox.sh` inside a Fedora toolbox.

If you want the tmux / `sesh` session workflow from this repo, make sure `sesh` is installed too. The dotfiles only manage `~/.config/sesh/sesh.toml`; they do not install the `sesh` binary for you.

For non-Fedora toolboxes:

- `fedora/setup-toolbox-ubuntu.sh`
- `fedora/setup-toolbox-arch.sh`

## Package Lists

The main Fedora setup scripts are intentionally thin wrappers around shared package lists:

- `base-packages.sh`: common bootstrap and CLI tooling
- `niri-packages.sh`: desktop/session packages for the Niri environment

This keeps the package decisions in one place while allowing different installers:

- `rpm-ostree install` on immutable Fedora
- `dnf install -y` on mutable Fedora

## Decisions

The package split reflects a few explicit decisions:

- The Fedora host setup is intended to stay COPR-free. Third-party repo enablement was removed from the Niri setup, and package choices should prefer what is available in standard Fedora repositories.
- `base-packages.sh` is meant to be a viable minimal bootstrap baseline, not a full daily-driver package set.
- `mise` is a core bootstrap tool, so the host base keeps a small native build toolchain: `automake`, `binutils`, `gcc`, `gcc-c++`, and `make`.
- `git`, `git-lfs`, `jq`, `ripgrep`, `stow`, `tmux`, and `zsh` are treated as common baseline tooling.
- `btop` and `gdu` are still in base as shared comfort tools.
- `sesh` is expected by the tmux and zsh session-launch flow, but it is installed separately from the RPM base packages.
- Desktop/session packages live in `niri-packages.sh`, including `alacritty`, `distrobox`, `fastfetch`, `rclone`, `swaybg`, `swayidle`, `swaylock`, `waybar`, and `wtype`.
- `wl-clipboard` is part of the common Niri package set because the active Niri config starts `wl-paste`.
- `btop`, `gdu`, and `distrobox` do not fit the package split perfectly, but they are placed pragmatically based on how this setup is actually bootstrapped and used.
- Hypr-specific setup was removed in favor of `swaybg`, `swayidle`, and `swaylock`.
- Wallpaper assets were moved to the shared `wallpapers/` stow package.

## Stow Notes

The Fedora stow packages under this directory are:

- `containers`
- `systemd`

From the repo root, the helper script `./stow-all.py` handles Fedora-only stow logic automatically.

If you need to stow the Fedora packages directly:

```bash
stow -d fedora -t ~ containers systemd
```

## Containers And User Services

This directory also carries user-scoped container and systemd assets:

- `containers/.config/containers/systemd/postgres.container`
- `systemd/.config/systemd/user/clipman-watcher.service`
- `systemd/.config/systemd/user/kanshi.service`
- `systemd/.config/systemd/user/mako.service`
- `systemd/.config/systemd/user/niri-session.target`
- `systemd/.config/systemd/user/swaybg.service`
- `systemd/.config/systemd/user/swayidle.service`
- `systemd/.config/systemd/user/toolbox-dev.service`
- `systemd/.config/systemd/user/ollama-toolbox.service`
- `systemd/.config/systemd/user/waybar.service`

The intended flow is:

1. Stow `containers` and `systemd` into `~/.config/...`
2. Reload the user systemd manager
3. Enable and start the units you want

Typical commands:

```bash
./stow-all.py --apply
systemctl --user daemon-reload
systemctl --user enable clipman-watcher.service kanshi.service mako.service swaybg.service swayidle.service waybar.service
systemctl --user enable --now postgres.service
systemctl --user enable --now toolbox-dev.service
systemctl --user enable --now ollama-toolbox.service
```

Notes:

- `postgres.container` is a Quadlet file under `~/.config/containers/systemd/`, so systemd generates `postgres.service` from it after reload.
- `clipman-watcher.service` replaces the old compositor-startup `wl-paste --watch clipman store` hook with an explicit user service.
- `waybar.service`, `mako.service`, `kanshi.service`, `swaybg.service`, and `swayidle.service` replace the old compositor-startup hooks for long-running desktop daemons.
- `niri-session.target` is started by `~/.config/niri/scripts/session-start`, which imports the Wayland session environment into the user manager and waits for Niri IPC before starting Niri-specific services. This keeps those daemons out of other desktop sessions such as GNOME and avoids noisy startup retries while the compositor is still coming up.
- Desktop daemons tied to `niri-session.target` should stop when the session target is stopped, rather than lingering until the entire user session exits.
- If you change `*.service` or `*.container` files later, run `systemctl --user daemon-reload` again before restarting them.
- `toolbox-dev.service` assumes a toolbox named `dev` already exists.
- `ollama-toolbox.service` assumes a toolbox named `ollama` exists and that `ollama` is installed inside it.
