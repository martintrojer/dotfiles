# Fedora Setup

Fedora-specific bootstrap scripts and the `bin/`, `gtk-3.0/`, and `systemd/`
packages. They target **Fedora Sway Atomic (Sericea)**, the rpm-ostree Sway
spin. Other Fedora variants are unsupported.

The gaming layer lives under [`gaming/`](./gaming/README.md). It is active by
default on the main rig. Run `./dotfiles-sync --apply --skip-gaming` on work or
laptop hosts to install only the Sway baseline documented here.

## Setup Flow

The scripts and manifest have separate roles:

- **`os/`** — package layering (`rpm-ostree`/`dnf`). Re-run these on a cadence:
  after a major-version rebase or when rebuilding the system clean. They need a
  reboot to take effect.
- **`config/`** — system configuration under `/etc` (writable and persistent on
  Atomic). Idempotent; re-run only when the tracked file changes.
- `mise/.config/mise/config.toml` — tracked global manifest for Fedora userland
  tools; `setup-mise.sh` installs it without rewriting it.

Order for a fresh install:

1. `os/setup-base.sh` — layer base packages (`rpm-ostree`).
2. `os/setup-sway.sh` — layer extra Sway session packages.
3. `setup-mise.sh` — install userland tools with `mise`.
4. `config/setup-zram.sh` — install the zram swap + VM tuning.
5. `os/setup-toolbox.sh` (optional) — run inside a Fedora toolbox.

For gaming, streaming, RGB, and controller/hardware fixes, see
[`gaming/README.md`](./gaming/README.md).

For daily automatic flatpak updates (no stock Fedora timer on Sway Atomic) and
per-app flatpak fixes (e.g. the Cider Apple Music client), see
[`flatpak/README.md`](./flatpak/README.md).

For T3 Code (agent-harness UI, shipped upstream as a bare AppImage with no
Fedora package), see [`t3code/README.md`](./t3code/README.md).

## Package Lists

The `os/` setup scripts are thin wrappers around shared package arrays; all
call `rpm-ostree install`:

- `os/base-packages.sh` — minimal bootstrap + CLI tooling on top of Sericea.
- `os/sway-packages.sh` — extra desktop/session packages only (nothing Sericea
  already ships: sway, foot, kanshi, swaybg/idle/lock, waybar, wl-clipboard,
  pipewire, xdg-desktop-portal-wlr, etc.).

## Decisions

- **No COPRs.** A COPR is a single-maintainer build with no distro QA, so it
  needs a reason no stock or RPM Fusion package can give; "upstream is newer
  there" is not one. Past attempts and why they were dropped are in
  [`gaming/docs/DECISIONS.md`](./gaming/docs/DECISIONS.md). Sericea's own
  `fedora-workstation-repositories` ships an enabled PyCharm COPR file in
  `/etc/yum.repos.d/`; nothing here installs from it, and it is not ours to
  remove.
- **Third-party repos in use:** Google's Chrome repo in the baseline (for
  `google-chrome-stable`) and RPM Fusion free + nonfree for the gaming layer
  only. No COPRs does not mean stock Fedora only.
- The **gaming layer is a deliberate, scoped break** of the minimal-overlay
  rule — RPM Fusion plus graphical packages — quarantined in
  [`gaming/`](./gaming/README.md); see
  [`docs/DECISIONS.md`](../docs/DECISIONS.md) for the partitioning rationale.
- `mise` is core bootstrap, so the base keeps a small build toolchain
  (`binutils`, `gcc`, `gcc-c++`, `make`). `git`, `git-lfs`, `tmux`, `zsh`
  are baseline; comfort CLIs that don't need host-layering live in the tracked
  mise manifest. The manifest tracks which tools belong on a host while
  `latest` lets setup install current releases without version-maintenance
  commits. Use `mise outdated` to review updates and `mise upgrade` to
  apply them. `mise ls --global --missing` detects missing declared tools, and
  `mise ls --prunable` reveals installed versions no tracked config needs.
- `setup-mise.sh` bootstraps `mise` only when absent, then installs directly from
  the tracked manifest. It does not activate mise or generate another config;
  shell activation remains solely in `zsh/.zsh/tools.zsh`. For a clean host,
  run `./dotfiles-sync --apply` before the script so the manifest is also linked
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
  ImageMagick for wallpaper renders and Lua for `nvdiff` and repo checks;
  `luacheck` arrives as a luarock under that Lua install, not as its own tool.

## Swap (zram)

There is no disk swap; all swap is zram, tracked in `config/zram/` and
installed by `config/setup-zram.sh`. Two files, two jobs:

| File | Installs to | Controls |
| --- | --- | --- |
| `zram-generator.conf` | `/etc/systemd/zram-generator.conf` | device size, resident RAM limit, algorithm |
| `99-zram-sysctl.conf` | `/etc/sysctl.d/99-zram-sysctl.conf` | how the VM reclaims into it |

The size knobs are the pair that trips people up:

- `zram-size = ram` is the **virtual** (uncompressed) capacity: how much swap
  `swapon --show` reports. It costs nothing until used; it is an address-space
  ceiling, not an allocation. Stock `zram-generator-defaults` ships
  `min(ram, 8192)`, sized for small machines.
- `zram-resident-limit = ram / 2` is the **resident RAM cost** ceiling: the
  compressed footprint (`/sys/block/zram0/mem_limit`). This is the one that
  bounds memory. Without it, an incompressible workload could grow zram until
  nothing is left to reclaim into. With zstd's ~3:1 on a desktop working set, a
  1:1-with-RAM disksize stays well under half of RAM in practice.

sysctl side, all four deviating from Fedora's defaults because the defaults
assume swap is a disk:

| Knob | Fedora | Here | Why |
| --- | --- | --- | --- |
| `vm.swappiness` | 60 | 180 | anon reclaim into RAM is cheaper than evicting page cache |
| `vm.page-cluster` | 3 | 0 | 3 means 8 pages per fault — decompress 32K to use 4K. Readahead only pays for a seek, and zram has none |
| `vm.watermark_scale_factor` | 10 | 125 | wake kswapd earlier so compression happens in the background, not in a direct-reclaim stall |
| `vm.watermark_boost_factor` | 15000 | 0 | fragmentation boosting plus high swappiness evicts in oversized bursts |

No `writeback-device`: incompressible pages stay in RAM rather than landing on
the NVMe, since the point of a disk-swap-free setup is keeping that write
amplification off the SSD.

sysctls apply immediately; the device is only re-created at boot, because
resizing a live zram swap means `swapoff` and the swapped-out pages have to fit
back in RAM first. Verify with `zramctl`, `swapon --show`, and
`sysctl vm.swappiness vm.page-cluster`.

## GTK Theme

`gtk-3.0/.config/gtk-3.0/settings.ini` ships an `Adwaita-dark` default for GTK3
apps because Sway does not set a GTK theme. If GTK4 apps disagree with GTK3,
run `gsettings set org.gnome.desktop.interface color-scheme prefer-dark` once
per machine. The setting is per-user state and is not linked.

## Packages

Baseline Fedora packages: `bin`, `gtk-3.0`, `mise`, `systemd`. From the repo
root, `./dotfiles-sync --apply` handles the Fedora-only logic; scope it to one
package with `./dotfiles-sync --apply bin`. There is no manual equivalent;
the planner owns the symlinks (see `_dotfiles_sync/link.py`).

The gaming layer adds one more package (`gaming/home`), linked by default and
skipped with `--skip-gaming`; see [`gaming/README.md`](./gaming/README.md).

## Migrate clipboard history

Fresh installs receive `cliphist` from `os/sway-packages.sh`. On a system that
already has `clipman` layered, create a replacement deployment and reboot:

```bash
sudo rpm-ostree uninstall clipman --install cliphist
systemctl reboot
```

After login, `sway-session.target` starts separate text and image watchers.

## User Services

`systemd/.config/systemd/user/` contains `sway-session.target` and the
`lmstudio-server`, `sway-cliphist-watcher@image`,
`sway-cliphist-watcher@text`, `sway-foot-server`, `sway-kanshi`, `sway-mako`,
`sway-waybar`, `swaybg`, and `swayidle` services.

Flow: link → reload → enable units:

```bash
./dotfiles-sync --apply
systemctl --user daemon-reload
systemctl --user enable --now lmstudio-server.service
```

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
