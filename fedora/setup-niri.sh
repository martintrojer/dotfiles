#!/bin/bash
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
source "$script_dir/niri-packages.sh"

rpm-ostree install "${niri_packages[@]}"
