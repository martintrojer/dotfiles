# Flatpak Auto-Update

Fedora Sway Atomic ships no stock flatpak update timer, and Sway has no GNOME
Software doing background updates. This adds a daily system timer so
system-scoped flatpaks stay current without manual `flatpak update` runs.

This host uses mixed scopes: remote-backed apps are system-scoped, while Cider
is installed per-user from a bundle with no Flatpak remote. The system apps
therefore update through these root-run units. Cider upgrades require manually
downloading and reinstalling its bundle; a user update timer would currently
have nothing to update.

The units are installed into the writable `/etc` tree because Atomic `/usr/lib`
is read-only ostree — the same constraint as the gaming `steam-pause` unit.

## Install

```bash
fedora/flatpak/setup-flatpak-update.sh
```

This copies `flatpak-update.service` and `flatpak-update.timer` into
`/etc/systemd/system/`, reloads systemd, and enables the timer.

## Units

- `flatpak-update.service` — oneshot `flatpak update --system --noninteractive`,
  gated on `nm-online -s`. `network-online.target` alone is not enough: it stays
  "reached" across suspend/resume, so a `Persistent=true` catch-up run fires
  seconds after wake with no DHCP lease yet. flatpak treats every failed ref
  lookup as non-fatal, reports "Nothing to update.", and exits 0, so the failure
  is silent.
- `flatpak-update.timer` — `OnCalendar=daily`, `Persistent=true` (catches up
  after downtime), `RandomizedDelaySec=1h`.

## Per-app fixes

- [`cider/`](cider/README.md) — install, manual upgrade, and safe URL-handler
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
