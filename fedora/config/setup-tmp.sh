#!/bin/bash
set -euo pipefail

# Install the tracked tmp.mount drop-in that puts /tmp on disk:
#
#   /etc/systemd/system/tmp.mount.d/90-bind-var-tmp.conf
#
# It replaces the tmpfs with a bind mount of /var/tmp. /etc is writable and
# persistent on Atomic, so this survives rpm-ostree upgrades and rebases. Run
# as your normal user; it uses sudo for the system path. See fedora/README.md
# for the rationale.

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
src="$script_dir/tmp/90-bind-var-tmp.conf"
dropin_dir=/etc/systemd/system/tmp.mount.d
dest="$dropin_dir/90-bind-var-tmp.conf"

if [[ ! -f "$src" ]]; then
  echo "error: $src not found." >&2
  exit 1
fi

sudo install -d -m 0755 "$dropin_dir"
sudo install -m 0644 "$src" "$dest"

# Superseded by the bind mount: the earlier 4G tmpfs size cap.
if [[ -f "$dropin_dir/90-size.conf" ]]; then
  sudo rm -f "$dropin_dir/90-size.conf"
  echo "Removed the superseded $dropin_dir/90-size.conf."
fi

sudo systemctl daemon-reload

echo "Installed $dest. tmp.mount will now mount:"
systemctl show tmp.mount -p What -p Type -p Options | sed 's/^/  /'
echo
echo "The live /tmp is still the tmpfs: swapping it under running sessions"
echo "would strand their sockets. Reboot, then verify with:"
echo "  findmnt /tmp    # SOURCE should be the /var device, FSTYPE btrfs"
echo "If /tmp fails to mount, remove $dest and reboot to get the tmpfs back."
