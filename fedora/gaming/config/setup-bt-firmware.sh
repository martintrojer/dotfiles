#!/bin/bash
set -euo pipefail

# Downgrade Realtek RTL8761BU BT firmware (bad 0xdfc6d922 -> good 0x09a98a6b) to
# fix mid-session Xbox controller disconnects. See ../README.md "Bluetooth
# Controller" for the why (ostree read-only /usr, SELinux lib_t, boot-time kargs).
# Run as your normal user; uses sudo for system paths.

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)

fw_src="$script_dir/firmware/rtl_bt"
fw_dst="/etc/firmware/rtl_bt"

for f in rtl8761bu_fw.bin.xz rtl8761bu_config.bin.xz; do
  if [[ ! -f "$fw_src/$f" ]]; then
    echo "error: $fw_src/$f not found." >&2
    exit 1
  fi
done

echo "Installing known-good RTL8761BU firmware (0x09a98a6b) to $fw_dst ..."
sudo install -d -m 0755 /etc/firmware /etc/firmware/rtl_bt
sudo install -m 0644 "$fw_src/rtl8761bu_fw.bin.xz" "$fw_dst/rtl8761bu_fw.bin.xz"
sudo install -m 0644 "$fw_src/rtl8761bu_config.bin.xz" "$fw_dst/rtl8761bu_config.bin.xz"

# Persistent lib_t label so the loader can read them under SELinux Enforcing.
echo "Applying SELinux firmware label (lib_t) to $fw_dst ..."
sudo semanage fcontext -a -t lib_t "/etc/firmware(/.*)?" 2>/dev/null \
  || sudo semanage fcontext -m -t lib_t "/etc/firmware(/.*)?"
sudo restorecon -Rv /etc/firmware

# Search /etc/firmware first; must be a kernel arg since BT firmware loads at boot.
if rpm-ostree kargs 2>/dev/null | grep -q 'firmware_class.path='; then
  echo "firmware_class.path already set in kernel args; leaving as-is:"
  rpm-ostree kargs | tr ' ' '\n' | grep 'firmware_class.path=' || true
else
  echo "Adding firmware_class.path=/etc/firmware to kernel args ..."
  sudo rpm-ostree kargs --append=firmware_class.path=/etc/firmware
fi

echo
echo "Done. Reboot to apply, then verify with:"
echo "  dmesg | grep 'RTL: fw version'   # expect 0x09a98a6b, not 0xdfc6d922"
echo
echo "Then boot 'Steam (gamescope)' and play a session; check for re-attaches:"
echo "  journalctl -b -k | grep -i 'Xbox Wireless Controller as'"
