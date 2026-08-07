#!/bin/bash
set -euo pipefail

# Install the tracked zram swap configuration:
#
#   /etc/systemd/zram-generator.conf     device size, resident limit, algorithm
#   /etc/sysctl.d/99-zram-sysctl.conf    swappiness / page-cluster / watermarks
#
# Both live under /etc, which is writable and persistent on Atomic, so they
# survive rpm-ostree upgrades and rebases. Run as your normal user; it uses
# sudo for the system paths. See fedora/README.md for the rationale.

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)

generator_src="$script_dir/zram/zram-generator.conf"
sysctl_src="$script_dir/zram/99-zram-sysctl.conf"

for src in "$generator_src" "$sysctl_src"; do
  if [[ ! -f "$src" ]]; then
    echo "error: $src not found." >&2
    exit 1
  fi
done

sudo install -m 0644 "$generator_src" /etc/systemd/zram-generator.conf
sudo install -m 0644 "$sysctl_src" /etc/sysctl.d/99-zram-sysctl.conf

# Supersedes the earlier hand-written swappiness-only drop-in.
if [[ -f /etc/sysctl.d/99-zram-swappiness.conf ]]; then
  sudo rm -f /etc/sysctl.d/99-zram-swappiness.conf
  echo "Removed the superseded /etc/sysctl.d/99-zram-swappiness.conf."
fi

sudo sysctl --system >/dev/null

echo "Installed zram config. sysctls are live now:"
sysctl vm.swappiness vm.page-cluster vm.watermark_scale_factor \
  vm.watermark_boost_factor | sed 's/^/  /'
echo
echo "The device itself is only re-created at boot (resizing a live zram swap"
echo "means swapoff, which needs the swapped-out pages to fit in RAM). Reboot,"
echo "then verify with:"
echo "  zramctl && swapon --show"
