# Fedora Setup

Fedora-specific bootstrap scripts plus `bin/`, `gtk-3.0/`, and `systemd/` stow
packages. Targets **Fedora Sway Atomic (Sericea)** — the rpm-ostree Sway
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
- `mise/.config/mise/config.toml` — tracked global manifest for Fedora userland
  tools; `setup-mise.sh` installs it without rewriting it.

Order for a fresh install:

1. `os/setup-base.sh` — layer base packages (`rpm-ostree`).
2. `os/setup-sway.sh` — layer extra Sway session packages.
3. `setup-mise.sh` — install userland tools with `mise`.
4. `os/setup-toolbox.sh` (optional) — run inside a Fedora toolbox.

For gaming, streaming, RGB, and controller/hardware fixes, see
[`gaming/README.md`](./gaming/README.md).

For daily automatic flatpak updates (no stock Fedora timer on Sway Atomic) and
per-app flatpak fixes (e.g. the Cider Apple Music client), see
[`flatpak/README.md`](./flatpak/README.md).

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
  are baseline; comfort CLIs that don't need host-layering live in the tracked
  mise manifest. The manifest tracks which tools belong on a host while
  `latest` lets setup install current releases without version-maintenance
  commits. Use `mise outdated` to review updates and `mise upgrade` to
  apply them. `mise ls --global --missing` detects missing declared tools, and
  `mise ls --prunable` reveals installed versions no tracked config needs.
- `setup-mise.sh` bootstraps `mise` only when absent, then installs directly from
  the tracked manifest. It does not activate mise or generate another config;
  shell activation remains solely in `zsh/.zsh/tools.zsh`. For a clean host,
  run `./dotfiles-sync --apply` before the script so the manifest is also stowed
  at `~/.config/mise/config.toml`. Repeated runs are safe. The script does not
  remove unrelated installed versions; inspect `mise ls --prunable` and run
  `mise prune --tools` explicitly when deletion is wanted.
- To verify the optional mise bootstrap before executing it, follow the upstream
  [GPG instructions](https://mise.jdx.dev/installing-mise.html) and run the
  verified installer, then rerun `setup-mise.sh`.
- The tmux/zsh session flow uses local scripts + `fzf`, `zoxide`, `fd`, `eza`
  rather than a session-manager binary.
- Wallpapers: `wallpaper set <url-or-file>` (stores under
  `~/.local/share/wallpapers/`, restarts `swaybg.service`). The manifest includes
  ImageMagick for wallpaper renders and Lua plus luacheck for `nvdiff` and repo
  checks.

## GTK Theme

`gtk-3.0/.config/gtk-3.0/settings.ini` ships an `Adwaita-dark` default for GTK3
apps (Sway doesn't push a GTK theme). GTK4 apps instead follow
`gsettings set org.gnome.desktop.interface color-scheme prefer-dark` — run once
per machine if GTK4 apps disagree with GTK3; it's per-user state, not stowed.

## Stow Packages

Baseline Fedora stow packages: `bin`, `gtk-3.0`, `mise`, `systemd`. From the
repo root, `./dotfiles-sync --apply` handles the Fedora-only logic. Manual
equivalent:

```bash
stow -d fedora -t ~ bin gtk-3.0 mise systemd
```

The gaming layer adds one more package (`gaming/home`), stowed by default and
skipped with `--skip-gaming`; see [`gaming/README.md`](./gaming/README.md).

## User Services

`systemd/.config/systemd/user/` contains `sway-session.target`,
`sway-clipman-watcher`, `sway-kanshi`, `sway-mako`, `swaybg`, `swayidle`,
`sway-waybar`, and `lmstudio-server` services.

Flow: stow → reload → enable units:

```bash
./dotfiles-sync --apply
systemctl --user daemon-reload
systemctl --user enable --now lmstudio-server.service
```

To retire the deleted Toolbox service on a host that previously stowed it, run:

```bash
systemctl --user disable --now toolbox-dev.service
rm -f ~/.config/systemd/user/toolbox-dev.service
systemctl --user daemon-reload
```

This removes only the obsolete unit; the `dev` container remains available on
demand and is not stopped or removed.

To finish removing the retired PostgreSQL Quadlet from a host after syncing
this repo, run:

```bash
systemctl --user disable --now postgres.service
rm -f ~/.config/containers/systemd/postgres.container
systemctl --user daemon-reload
systemctl --user reset-failed postgres.service
systemctl --user is-active postgres.service || true
systemctl --user is-enabled postgres.service || true
systemctl --user cat postgres.service || true
ss -ltn '( sport = :5432 )'
```

The final checks should report no active/enabled `postgres.service`, no listener
on port 5432, and no generated unit. Keep the `pg_data` Podman volume and cached
PostgreSQL image unless destructive cleanup is separately approved.

Notes:

- `sway-session.target` is started by `~/.config/sway/scripts/session-start`
  (after importing the Sway session env) and owns the desktop services
  (waybar, mako, swayidle, …); `session-quit` stops them. Vendor user units for
  `mako`, `waybar`, `kanshi`, `foot-server` are masked by `dotfiles-sync --apply`
  so D-Bus activation can't start duplicates.
- Re-run `systemctl --user daemon-reload` after editing `*.service`/`*.container`.
- `lmstudio-server.service` runs the LM Studio flatpak in its hidden
  `--run-as-service` mode, then starts and monitors the OpenAI-compatible API
  on `http://localhost:1234/v1`. It reports active only after `/v1/models`
  responds and restarts if either the flatpak or API dies. The unit belongs to
  the graphical login session; it does not use lingering or run before login.
