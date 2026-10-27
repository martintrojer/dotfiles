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
  after a major-version rebase, when rebuilding the system clean, or after
  adding packages to a `*-packages.sh` list (`--idempotent` skips ones already
  layered). They need a reboot to take effect. Both finish by running
  `os/check-layered.sh`, which compares layered packages with every
  `*-packages.sh` array (including `gaming/os/`) and lists both sides: listed
  but not layered, and layered but not listed, each with the `rpm-ostree`
  command that fixes it. It only reports; it changes nothing.
- **`config/`** — system configuration under `/etc` (writable and persistent on
  Atomic). Idempotent; re-run only when the tracked file changes.
- Userland CLI tools come from mise; see [`mise/README.md`](../mise/README.md).

Order for a fresh install:

1. `os/setup-base.sh` — layer base packages (`rpm-ostree`).
2. `os/setup-sway.sh` — layer extra Sway session packages.
3. `../mise/setup-mise.sh` — install userland tools with `mise`.
4. `config/setup-zram.sh` — install the zram swap + VM tuning.
5. `config/setup-resolved.sh` — allow single-label LAN hostname lookups.
6. `config/setup-tmp.sh` — put `/tmp` on disk (bind of `/var/tmp`); reboot after.
7. `os/setup-toolbox.sh` (optional) — run inside a Fedora toolbox.

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
  (`binutils`, `gcc`, `gcc-c++`, `make`). `git`, `git-lfs`, `zsh`
  are baseline; comfort CLIs that don't need host-layering live in the
  shared mise manifest ([`mise/README.md`](../mise/README.md)).
- The tmux/zsh session flow uses local scripts + `fzf`, `zoxide`, `fd`, `eza`
  rather than a session-manager binary.
- Wallpapers: `wallpaper set <url-or-file>` (stores under
  `~/.local/share/wallpapers/`, restarts `swaybg.service`). The Linux mise
  extras include ImageMagick for wallpaper renders. Fedora toolboxes share the
  host's mise installs and Luacheck rock through the shared home directory.

## /etc Inventory

Atomic keeps `/usr` identical to the image, so `/etc` is where a host drifts.
`sudo ostree admin config-diff` lists every change against the image. Everything
it reports as added (`A`) or deliberately modified is listed here; if it is not
in this table, it is drift and gets removed or added here.

| Path under `/etc` | Source | Why |
| --- | --- | --- |
| `systemd/zram-generator.conf`, `sysctl.d/99-zram-sysctl.conf` | `config/setup-zram.sh` | zram-only swap; see [Swap](#swap-zram) |
| `systemd/resolved.conf.d/90-single-label.conf` | `config/setup-resolved.sh` | bare LAN hostnames; see [LAN Hostnames](#lan-hostnames-systemd-resolved) |
| `systemd/system/tmp.mount.d/90-bind-var-tmp.conf` | `config/setup-tmp.sh` | disk-backed `/tmp`; see [/tmp on disk](#tmp-on-disk) |
| `systemd/system/flatpak-update.{service,timer}` + `timers.target.wants` link | `flatpak/setup-flatpak-update.sh` | no stock flatpak update timer on Sway Atomic |
| `systemd/system/multi-user.target.wants/sshd.service` | manual `systemctl enable sshd` | ssh into this box from the LAN |
| `udev/rules.d/10-trim.rules` | `~/hacking/rpi` bootstrap (`14-install-default-trim-udev-rule.sh`) | TRIM on RTL9210 USB-NVMe bridges, for the backup drive |
| `udev/rules.d/99-powercap.rules` | Mission Center's `missioncenter-magpie-setup` (2026-09-03) | makes RAPL `energy_uj` world-readable so Mission Center shows CPU power. Deliberately undoes the kernel's Platypus lockdown; acceptable on a single-user desktop |
| `yum.repos.d/google-chrome.repo` (modified) | stock `fedora-workstation-repositories` file, enabled | Chrome repo, the one non-Fedora baseline repo |
| `yum.repos.d/rpmfusion-nonfree-{steam,nvidia-driver}.repo` (modified) | stock `fedora-workstation-repositories` files, enabled | Steam repo for the gaming layer; the NVIDIA one is unused on this AMD box and can be disabled |
| `systemd/logind.conf.d/10-power-key-suspend.conf` | `gaming/config/setup-power-key.sh` | power button suspends |
| `systemd/system/steam-pause-games.service` + `sleep.target.wants` link | `gaming/config/setup-steam-pause.sh` | pause games across suspend (`/usr/lib` sleep hooks are read-only) |
| `udev/rules.d/99-disable-usb-wakeup.rules` | `gaming/config/setup-wake-usb.sh` | only the power button wakes the tower |
| `modules-load.d/i2c-dev.conf`, `udev/rules.d/99-i2c.rules`, `i2c` group | `gaming/config/setup-openrgb.sh` | OpenRGB SMBus access |
| `openrgb/` (empty) | created at runtime by OpenRGB, no package owns it | harmless |
| `firmware/rtl_bt/*`, SELinux `file_contexts.local` (`lib_t`), kargs | `gaming/config/setup-bt-firmware.sh` | Xbox controller BT firmware fix |
| `gamemode` line in `group`/`gshadow` | manual, see [GameMode](./gaming/README.md#gamemode-feral) | group exists only in `/usr/lib/group` on rpm-ostree |

The rest of `config-diff` is per-machine state that every install has and the
repo does not own: users and groups, `machine-id`, `hostname`, locale and
timezone, `fstab`/`crypttab`, ssh host keys, `nvme/host*`, anaconda's
`X11/xorg.conf.d/00-keyboard.conf` and `sysconfig/anaconda`, `audit/audit.rules`,
authselect, SELinux store and lock files, tuned and chrony state,
`systemd/system/default.target`, and the `systemd/system.control/` drop-ins that
systemd writes for user slices. `firewalld/zones/` stays empty: the stock
`public` zone (ssh, mdns, dhcpv6-client) is the baseline. If Steam Remote Play
fails to connect, `firewall-cmd --permanent --add-service=steam-streaming` (a
stock firewalld service) is the fix, and it gets a row here.

Outside `/etc`, the gaming layer also writes `/usr/local` (`/var/usrlocal`):
`steam-session`, `steamos-session-select`, `steam-pause` and the SDDM session
file. See [`gaming/docs/GAMESCOPE-SESSION.md`](./gaming/docs/GAMESCOPE-SESSION.md).

Removed 2026-10-20: Decky Loader (`plugin_loader.service`, running as root from
`~/homebrew`, left over from the SDH-PauseGames evaluation that steam-pause
replaced) and a firewalld zone override with stale ports.

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

## /tmp on disk

Fedora mounts `/tmp` as a tmpfs at 50% of RAM. Every byte in it is RAM, and
under pressure it goes to zram, which is RAM too. Agents park worktrees and
cargo `target/` dirs in `/tmp`, so a tmpfs either eats memory or, capped
smaller, fills and fails builds. `config/tmp/90-bind-var-tmp.conf` keeps
`tmp.mount` but changes what it mounts: `/var/tmp` bind-mounted over `/tmp`,
`nosuid,nodev` as before. `/tmp` is then the `/var` btrfs, with no RAM cost.

Why a bind and not the usual fixes:

- `systemctl mask tmp.mount` leaves `/tmp` on the root, and the root here is
  a read-only composefs, so `/tmp` becomes unwritable.
- A `/tmp -> /var/tmp` symlink can only be made inside the image, and
  `tmp.mount` silently skips itself when `/tmp` is a symlink.

The same drop-in is what Fedora Atomic's cloud images used
([rpm-ostree#820](https://github.com/coreos/rpm-ostree/issues/820)) and what
[agenticlinux](https://github.com/ericcurtin/agenticlinux/pull/27) ships, for
the same reason. SELinux needs nothing: `/var/tmp` is already `tmp_t`.

What changes:

- `/tmp` survives reboots; it is no longer emptied at boot.
- `/tmp` and `/var/tmp` are one directory, so both tmpfiles rules (`q /tmp
  10d`, `q /var/tmp 30d`) apply to it and the 10-day one wins. Stale agent
  scratch goes in 10 days instead of piling up.
- Builds write to the NVMe instead of RAM.

The swap takes a reboot; the script installs the drop-in and stops there.
Verify with `findmnt /tmp` (source the `/var` device, type btrfs). If `/tmp`
ever fails to mount, delete the drop-in and reboot to get the tmpfs back.

## LAN Hostnames (systemd-resolved)

Bare LAN names (`bubba`, `pizero2`) fail to resolve out of the box while macOS
and Android on the same network resolve them fine. Nothing is wrong with the
DNS server: `dig @192.168.0.100 bubba` answers, but `getent hosts bubba` does
not. systemd-resolved refuses to forward **single-label** A/AAAA queries to
classic DNS (`ResolveUnicastSingleLabel=false`), on the grounds that a bare
name is ambiguous and could be hijacked by a hostile upstream. The query never
leaves the stub resolver, so the Pi-hole is never asked.

`config/resolved/90-single-label.conf` flips that knob, installed by
`config/setup-resolved.sh` to
`/etc/systemd/resolved.conf.d/90-single-label.conf`. The Pi-hole is
authoritative for bare hostnames on this network, so the ambiguity the default
guards against does not exist here.

The alternative — a search domain (`nmcli con mod <con> ipv4.dns-search lan`),
which turns `bubba` into the two-label `bubba.lan` — needs the DNS server to
serve those FQDNs. Pi-hole here only holds the bare names, so it would answer
nothing. Verify with `resolvectl query bubba` (`Data from: network` means it
reached upstream).

## GTK Theme

`gtk-3.0/.config/gtk-3.0/settings.ini` ships an `Adwaita-dark` default for GTK3
apps because Sway does not set a GTK theme. If GTK4 apps disagree with GTK3,
run `gsettings set org.gnome.desktop.interface color-scheme prefer-dark` once
per machine. The setting is per-user state and is not linked.

## Packages

Baseline Fedora packages: `bin`, `gtk-3.0`, `systemd`. From the repo
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
