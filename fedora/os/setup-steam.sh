#!/bin/bash
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
source "$script_dir/steam-packages.sh"

# Requires RPM Fusion (free + nonfree) and the LACT COPR to be enabled first.
# See the header of steam-packages.sh for the repo setup commands.
rpm-ostree install "${steam_packages[@]}"
