#!/bin/bash
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
source "$script_dir/steam-packages.sh"

# Requires RPM Fusion (free + nonfree) to be enabled first.
# See the header of steam-packages.sh for the repo setup commands.
#
# --allow-inactive: some of these (gamemode, 7zip) already ship in the Sericea
# base image. Without the flag rpm-ostree errors with "already provided by" and
# installs nothing, taking the whole array down with it. With it, they are
# recorded as explicitly requested, which is the point: a base-image change must
# not silently drop a package this stack depends on.
rpm-ostree install --allow-inactive "${steam_packages[@]}"
