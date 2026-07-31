#!/bin/bash
set -euo pipefail

# Install and enable a daily system-flatpak auto-update timer.
#
# Fedora Sway Atomic ships no stock flatpak update timer and has no GNOME
# Software running background updates. Remote-backed flatpaks here are
# system-scoped, so their updates need root. The user-scoped Cider bundle has no
# remote and is upgraded manually. This installs a oneshot service + daily
# timer into the writable /etc tree (Atomic /usr/lib is read-only ostree). See
# fedora/flatpak/README.md.
#
# Run as your normal user; it uses sudo for the system paths.

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
unit_dir="$script_dir/systemd-system"

for unit in flatpak-update.service flatpak-update.timer; do
  if [[ ! -f "$unit_dir/$unit" ]]; then
    echo "error: $unit_dir/$unit not found." >&2
    exit 1
  fi
  sudo install -m 0644 "$unit_dir/$unit" "/etc/systemd/system/$unit"
done

sudo systemctl daemon-reload
sudo systemctl enable --now flatpak-update.timer

echo "Installed and enabled flatpak-update.timer (daily)."
echo "Next run:"
systemctl list-timers flatpak-update.timer --no-pager 2>/dev/null || true
