#!/bin/bash
set -euo pipefail

# Configure Cider's host URL handlers and tighten its session bus access.
# Idempotent; safe to re-run as a normal user. See README.md.

app=sh.cider.Cider
desktop=sh.cider.Cider.desktop
shim_dir="$HOME/.local/share/cider-shims"
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

# Also clear the overrides left by the old xdg-settings shim.
#
# PATH is reset with --env, not --unset-env: `--unset-env=PATH` does not hand
# the runtime default back, it removes the variable, and the app then falls
# back to a host-style PATH with no /app/bin at all (measured: unset yields
# /usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin:. while an
# un-overridden app gets /app/bin:/usr/bin). Cider's own binaries live in
# /app/bin, so restore the stock value explicitly.
flatpak override --user \
  --nosocket=session-bus \
  --nofilesystem="$shim_dir" \
  --no-talk-name=org.freedesktop.Flatpak \
  --env=PATH=/app/bin:/usr/bin \
  "$app"

rm -f "$shim_dir/xdg-settings"
rmdir "$shim_dir" 2>/dev/null || true

echo "Configured Cider URL handlers and sandbox hardening."
echo "Current overrides:"
flatpak override --user --show "$app"
