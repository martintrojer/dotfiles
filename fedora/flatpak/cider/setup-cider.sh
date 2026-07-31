#!/bin/bash
set -euo pipefail

# Fix the `write EPIPE` JavaScript error when launching the Cider flatpak
# (sh.cider.Cider) on Fedora Sway Atomic. See fedora/flatpak/cider/README.md.
#
# Cider spawns `xdg-settings` to register URL-scheme handlers, but the flatpak
# runtime lacks that binary; the failed spawn throws EPIPE on startup. We drop a
# shim on the sandbox PATH that forwards to the host, and grant the app the
# permissions it needs to use it.
#
# Idempotent; safe to re-run. Run as your normal user.

app=sh.cider.Cider
shim_dir="$HOME/.local/share/cider-shims"
script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)

if ! flatpak info "$app" >/dev/null 2>&1; then
  echo "error: $app is not installed. Install the flatpak first." >&2
  exit 1
fi

install -d "$shim_dir"
install -m 0755 "$script_dir/xdg-settings" "$shim_dir/xdg-settings"

flatpak override --user \
  --filesystem="$shim_dir:ro" \
  --talk-name=org.freedesktop.Flatpak \
  --env=PATH="$shim_dir:/app/bin:/usr/bin" \
  "$app"

echo "Installed xdg-settings shim to $shim_dir and applied flatpak overrides."
echo "Current overrides:"
flatpak override --user --show "$app"
