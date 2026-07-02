#!/bin/bash
set -euo pipefail

# Stop the desktop keyboard/mouse (and BT dongle) from waking the tower out of
# suspend, so only the power button and Steam's "Suspend" drive sleep/resume.
#
# Installs a udev rule that pins power/wakeup=disabled for the listed USB
# vendor:product IDs, then applies it to the already-plugged devices:
#
#   /etc/udev/rules.d/99-disable-usb-wakeup.rules
#
# The rule matches by ID so it persists across reboots and re-plugging (the
# sysfs value otherwise resets each boot). To add a device, find its IDs with
# lsusb and add a line to wake-usb/99-disable-usb-wakeup.rules.
#
# Run as your normal user; it uses sudo for the system path.

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)

rule_src="$script_dir/wake-usb/99-disable-usb-wakeup.rules"

if [[ ! -f "$rule_src" ]]; then
  echo "error: $rule_src not found." >&2
  exit 1
fi

sudo install -m 0644 "$rule_src" /etc/udev/rules.d/99-disable-usb-wakeup.rules
sudo udevadm control --reload-rules
sudo udevadm trigger --subsystem-match=usb --action=add

echo "Installed USB wake rule and applied it to plugged devices."
echo "Verify (expect 'disabled' for each):"
echo "  for f in /sys/bus/usb/devices/*/power/wakeup; do d=\$(dirname \$(dirname \$f)); echo \"\$(cat \$f) \$(cat \$d/product 2>/dev/null)\"; done"
