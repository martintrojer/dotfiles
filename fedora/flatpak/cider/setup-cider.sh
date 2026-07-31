#!/bin/bash
set -euo pipefail

# Configure Cider's host URL handlers once and install a narrow compatibility
# shim for releases that call xdg-settings at every launch. See README.md.
# Idempotent; safe to re-run as a normal user.

app=sh.cider.Cider
desktop=sh.cider.Cider.desktop
shim_dir="$HOME/.local/share/cider-shims"
script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
protocols=(cider itms itmss music itunes)

if ! flatpak info "$app" >/dev/null 2>&1; then
  echo "error: $app is not installed. Install the flatpak first." >&2
  exit 1
fi

if ! command -v xdg-settings >/dev/null 2>&1; then
  echo "error: host xdg-settings is not installed." >&2
  exit 1
fi

for protocol in "${protocols[@]}"; do
  if [[ $(xdg-settings get default-url-scheme-handler "$protocol") != "$desktop" ]]; then
    xdg-settings set default-url-scheme-handler "$protocol" "$desktop"
  fi
done

install -d "$shim_dir"
install -m 0755 "$script_dir/xdg-settings" "$shim_dir/xdg-settings"

flatpak override --user \
  --nosocket=session-bus \
  --filesystem="$shim_dir:ro" \
  --no-talk-name=org.freedesktop.Flatpak \
  --env=PATH="$shim_dir:/app/bin:/usr/bin" \
  "$app"

echo "Configured Cider URL handlers and installed the compatibility shim."
echo "Current overrides:"
flatpak override --user --show "$app"
