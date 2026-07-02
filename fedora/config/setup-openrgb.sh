#!/bin/bash
set -euo pipefail

# Set up OpenRGB SMBus/i2c access so it can drive motherboard / RAM / GPU RGB
# without root. OpenRGB itself comes from the `openrgb` rpm (steam-packages.sh).
#
# This wires the system bits OpenRGB needs for i2c devices:
#   1. load the i2c-dev kernel module at boot (/dev/i2c-* nodes)
#   2. create the `i2c` system group
#   3. a udev rule giving that group rw on the i2c-dev nodes
#   4. add the invoking user to the group
#   5. install + enable rgb.service (system unit) to set the color on boot
#
# Without 1-4 the i2c-* nodes stay root:root 0600 and OpenRGB sees no SMBus
# controllers. rgb.service is the early-boot RGB setter. Run as your normal
# user; it uses sudo for the system paths.

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)

# 1. Load i2c-dev now and at every boot.
sudo modprobe i2c-dev
sudo install -m 0644 "$script_dir/openrgb/i2c-dev.conf" /etc/modules-load.d/i2c-dev.conf

# 2. Create the i2c group (no-op if it already exists).
getent group i2c >/dev/null || sudo groupadd -r i2c

# 3. Install the udev rule and apply it to the live nodes.
sudo install -m 0644 "$script_dir/openrgb/99-i2c.rules" /etc/udev/rules.d/99-i2c.rules
sudo udevadm control --reload-rules
sudo udevadm trigger --subsystem-match=i2c-dev --action=change

# 4. Add the current user to the group.
sudo usermod -aG i2c "$USER"

# 5. Install and enable the boot-time RGB setter (system service).
sudo install -m 0644 "$script_dir/openrgb/rgb.service" /etc/systemd/system/rgb.service
sudo systemctl daemon-reload
sudo systemctl enable --now rgb.service

echo "OpenRGB i2c access + rgb.service configured."
echo "Log out/in (or run 'newgrp i2c') for the group membership to take effect."
echo "Verify: getent group i2c && ls -l /dev/i2c-*  (expect root i2c, crw-rw----)"
