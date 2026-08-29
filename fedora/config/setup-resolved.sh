#!/bin/bash
set -euo pipefail

# Install the tracked systemd-resolved drop-in:
#
#   /etc/systemd/resolved.conf.d/90-single-label.conf
#
# /etc is writable and persistent on Atomic, so this survives rpm-ostree
# upgrades and rebases. Run as your normal user; it uses sudo for the system
# path. See fedora/README.md for the rationale.

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
src="$script_dir/resolved/90-single-label.conf"
dest=/etc/systemd/resolved.conf.d/90-single-label.conf

if [[ ! -f "$src" ]]; then
  echo "error: $src not found." >&2
  exit 1
fi

sudo install -d -m 0755 /etc/systemd/resolved.conf.d
sudo install -m 0644 "$src" "$dest"
sudo systemctl restart systemd-resolved

echo "Installed $dest. Resolving the LAN hosts:"
for host in bubba pizero2; do
  printf '  %-10s %s\n' "$host" "$(resolvectl query "$host" 2>&1 |
    awk -v h="$host" '$1 == h": " || $1 == h":" { print $2; exit }')"
done
