#!/bin/bash
set -euo pipefail

# Install the tracked tmp.mount drop-in that caps the /tmp tmpfs:
#
#   /etc/systemd/system/tmp.mount.d/90-size.conf
#
# /etc is writable and persistent on Atomic, so this survives rpm-ostree
# upgrades and rebases. Run as your normal user; it uses sudo for the system
# path. See fedora/README.md for the rationale.

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
src="$script_dir/tmp/90-size.conf"
dest=/etc/systemd/system/tmp.mount.d/90-size.conf

if [[ ! -f "$src" ]]; then
  echo "error: $src not found." >&2
  exit 1
fi

sudo install -d -m 0755 /etc/systemd/system/tmp.mount.d
sudo install -m 0644 "$src" "$dest"
sudo systemctl daemon-reload

# Resize the live tmpfs too; files stay. Fails if /tmp already holds more
# than the new size, in which case the cap applies from the next boot.
size=$(sed -n 's/^Options=.*size=\([^,]*\).*/\1/p' "$src")
if sudo mount -o remount,size="$size" /tmp; then
  echo "Installed $dest; /tmp resized live:"
else
  echo "Installed $dest; live resize failed, the cap applies after reboot:"
fi
df -h /tmp | sed 's/^/  /'
