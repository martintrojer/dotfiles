#!/bin/bash
set -euo pipefail

# Make the tower's power button suspend instead of power off: short press =
# sleep, long press = power off. This is a global logind setting (not
# per-session), so it applies in Sway, the gamescope session, and at the SDDM
# greeter alike.
#
# It installs a logind drop-in under /etc (writable + persistent on Atomic):
#
#   /etc/systemd/logind.conf.d/10-power-key-suspend.conf
#
# Run as your normal user; it uses sudo for the system path. Independent of
# setup-steam-pause.sh, but pairs with it (that pauses running games on suspend).

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)

logind_src="$script_dir/logind.conf.d/10-power-key-suspend.conf"

if [[ ! -f "$logind_src" ]]; then
  echo "error: $logind_src not found." >&2
  exit 1
fi

sudo install -d -m 0755 /etc/systemd/logind.conf.d
sudo install -m 0644 "$logind_src" \
  /etc/systemd/logind.conf.d/10-power-key-suspend.conf
sudo systemctl kill -s HUP systemd-logind

echo "Installed the power-key drop-in: short press suspends, long press powers off."
echo "Applies to new sessions; verify with:"
echo "  systemd-analyze cat-config systemd/logind.conf | grep HandlePowerKey"
