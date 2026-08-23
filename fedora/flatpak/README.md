# Flatpak Auto-Update

Fedora Sway Atomic has no stock Flatpak update timer, and Sway does not run
GNOME Software in the background. These units update system-scoped Flatpaks
once a day without a manual `flatpak update` run.

This host uses mixed scopes: remote-backed apps are system-scoped, while Cider
is installed per-user from a bundle with no Flatpak remote. The system apps
therefore update through these root-run units. Cider upgrades require manually
downloading and reinstalling its bundle; a user update timer would currently
have nothing to update.

Install the units in writable `/etc` because Atomic's `/usr/lib` OSTree is
read-only. The gaming `steam-pause` unit has the same constraint.

## Install

```bash
fedora/flatpak/setup-flatpak-update.sh
```

This copies `flatpak-update.service` and `flatpak-update.timer` into
`/etc/systemd/system/`, reloads systemd, and enables the timer.

## Units

- `flatpak-update.service`: runs `flatpak update --system --noninteractive`
  after `nm-online -s`. `network-online.target` alone is not enough because it stays
  "reached" across suspend/resume, so a `Persistent=true` catch-up run fires
  seconds after wake with no DHCP lease yet. flatpak treats every failed ref
  lookup as non-fatal, reports "Nothing to update.", and exits 0, so the failure
  is silent.
- `flatpak-update.timer`: `OnCalendar=daily`, `Persistent=true` (catches up
  after downtime), `RandomizedDelaySec=1h`.

## Per-app fixes

- [`cider/`](cider/README.md): install, manual upgrade, and safe URL-handler
  fix for the user-scoped Cider (`sh.cider.Cider`) Apple Music flatpak: download
  its bundle from Taproom, install it for the user, then run
  `cider/setup-cider.sh`. It configures the host URL handlers and replaces the
  unfiltered session bus socket with Flatpak's filtered proxy.

## Check / Run

```bash
systemctl list-timers flatpak-update.timer   # next scheduled run
systemctl start flatpak-update.service       # update now
journalctl -u flatpak-update.service         # last run log
```
