#!/bin/bash
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
# shellcheck source=fedora/os/sway-packages.sh
source "$script_dir/sway-packages.sh"

rpm-ostree install --idempotent "${sway_packages[@]}"

# Warn (not fail) on layering drift: missing or unlisted packages.
"$script_dir/check-layered.sh" || true
