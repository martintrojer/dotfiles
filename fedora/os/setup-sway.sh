#!/bin/bash
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
# shellcheck source=fedora/os/sway-packages.sh
source "$script_dir/sway-packages.sh"

rpm-ostree install "${sway_packages[@]}"
