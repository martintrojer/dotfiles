#!/bin/bash
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
source "$script_dir/steam-packages.sh"

# Requires RPM Fusion (free + nonfree) to be enabled first.
# See the header of steam-packages.sh for the repo setup commands, and for why
# base-image packages (gamemode, 7zip) are deliberately not in the array.
rpm-ostree install "${steam_packages[@]}"
