# Flatpak Auto-Update

Fedora Sway Atomic ships no stock flatpak update timer, and Sway has no GNOME
Software doing background updates. This adds a daily system timer so flatpaks
stay current without manual `flatpak update` runs.

All flatpaks on this host are system-scoped (`flatpak list --system`), so
updates require root. The units are therefore **system** units, not `--user`
ones, installed into the writable `/etc` tree because Atomic `/usr/lib` is
read-only ostree — the same constraint as the gaming `steam-pause` unit.

## Install

```bash
fedora/flatpak/setup-flatpak-update.sh
```

This copies `flatpak-update.service` and `flatpak-update.timer` into
`/etc/systemd/system/`, reloads systemd, and enables the timer.

## Units

- `flatpak-update.service` — oneshot `flatpak update --system --noninteractive`,
  ordered after `network-online.target`.
- `flatpak-update.timer` — `OnCalendar=daily`, `Persistent=true` (catches up
  after downtime), `RandomizedDelaySec=1h`.

## Per-app fixes

- [`cider/`](cider/README.md) — install + fix for the Cider (`sh.cider.Cider`)
  Apple Music flatpak: download from Taproom, `flatpak install --user`, then run
  `cider/setup-cider.sh` to add the `xdg-settings` shim + overrides that clear
  the `write EPIPE` JavaScript error on launch.

## Check / Run

```bash
systemctl list-timers flatpak-update.timer   # next scheduled run
systemctl start flatpak-update.service       # update now
journalctl -u flatpak-update.service         # last run log
```
