#!/bin/bash
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
source "$script_dir/base-packages.sh"

dnf install -y "${base_packages[@]}" "${workstation_extra_packages[@]}"
