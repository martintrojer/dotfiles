#!/bin/bash
set -euo pipefail

# Install the Steam pause-on-suspend integration for the "Steam (gamescope)"
# session: a oneshot unit ordered around sleep.target that pauses running games
# before suspend and resumes them on wake (avoids crackling audio and frozen
# emulators). Logic is extracted from the SDH-PauseGames Decky plugin, no Decky
# needed. See fedora/bin/README.md. It wires:
#
#   /usr/local/bin/steam-pause                       a copy of the stowed script
#   /etc/systemd/system/steam-pause-games.service        the sleep-ordered unit
#
# A /usr/lib/systemd/system-sleep/ hook can't be used (Atomic /usr/lib is
# read-only and systemd-sleep reads only that dir), hence a unit in writable
# /etc. steam-pause is copied, not symlinked to ~/.local/bin, since the root
# unit can't exec under $HOME (SELinux user_home_t, 203/EXEC).
#
# For the power button as a "go to sleep" key, see setup-power-key.sh.
#
# Prereq: dotfiles stowed (dotfiles-sync --apply) so ~/.local/bin/steam-pause
# exists. Run as your normal user; it uses sudo for the system paths.

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)

steam_pause="$HOME/.local/bin/steam-pause"
unit_src="$script_dir/systemd-system/steam-pause-games.service"

if [[ ! -x "$steam_pause" ]]; then
  echo "error: $steam_pause not found or not executable." >&2
  echo "Run 'dotfiles-sync --apply' first to stow ~/.local/bin." >&2
  exit 1
fi

if [[ ! -f "$unit_src" ]]; then
  echo "error: $unit_src not found." >&2
  exit 1
fi

sudo install -d -m 0755 /usr/local/bin
sudo install -m 0755 "$steam_pause" /usr/local/bin/steam-pause
sudo restorecon -v /usr/local/bin/steam-pause 2>/dev/null || true
sudo install -m 0644 "$unit_src" /etc/systemd/system/steam-pause-games.service
sudo systemctl daemon-reload
sudo systemctl enable steam-pause-games.service

echo "Installed steam-pause and enabled steam-pause-games.service."
echo "Games will pause before suspend and resume on wake."
