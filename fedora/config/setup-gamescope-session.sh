#!/bin/bash
set -euo pipefail

# Install the SteamOS-style gamescope sessions so they can be picked from the
# SDDM login screen. Both are embedded/DRM gamescope sessions that exec the same
# stowed launcher; only env knobs differ. See fedora/docs/HDR-GAMING.md.
#
#   steam.desktop         4K HDR for couch PC gaming (launcher defaults)
#   steam-stream.desktop  1080p SDR + Sunshine for streaming to a handheld
#                         (GS_OUT_W=1920 GS_OUT_H=1080 GS_HDR=0 GS_SUNSHINE=1)
#
# It wires system paths (writable + persistent on Atomic via the
# /usr/local -> /var/usrlocal symlink), which SDDM already searches
# (SessionDir=/usr/local/share/wayland-sessions,...):
#
#   /usr/local/bin/steam-session                        -> the stowed launcher
#   /usr/local/share/wayland-sessions/steam.desktop        couch HDR entry
#   /usr/local/share/wayland-sessions/steam-stream.desktop streaming entry
#
# Prereqs: steam + gamescope layered (os/setup-steam.sh) and the dotfiles stowed
# (dotfiles-sync --apply), so ~/.local/bin/steam-session exists. Run as your
# normal user; it uses sudo for the system paths.

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)

launcher="$HOME/.local/bin/steam-session"
desktop_srcs=(
  "$script_dir/gamescope-session/steam.desktop"
  "$script_dir/gamescope-session/steam-stream.desktop"
)

if [[ ! -x "$launcher" ]]; then
  echo "error: $launcher not found or not executable." >&2
  echo "Run 'dotfiles-sync --apply' first to stow ~/.local/bin." >&2
  exit 1
fi

for desktop_src in "${desktop_srcs[@]}"; do
  if [[ ! -f "$desktop_src" ]]; then
    echo "error: $desktop_src not found." >&2
    exit 1
  fi
done

sudo install -d -m 0755 /usr/local/bin /usr/local/share/wayland-sessions
sudo ln -sf "$launcher" /usr/local/bin/steam-session
for desktop_src in "${desktop_srcs[@]}"; do
  sudo install -m 0644 "$desktop_src" \
    "/usr/local/share/wayland-sessions/$(basename "$desktop_src")"
done

echo "Installed 'Steam (gamescope)' and 'Steam (gamescope stream)' sessions."
echo "Pick one at the SDDM login screen; use gamepad UI 'Desktop Mode' to return to SDDM."
