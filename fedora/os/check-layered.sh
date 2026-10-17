#!/bin/bash
# Compare layered packages with the *-packages.sh arrays, both ways:
# - missing: listed but not layered
# - extra: layered but not listed, e.g. a package dropped from a list or a
#   one-off `rpm-ostree install`. rpm-ostree has no "sync to this list" mode,
#   so removal stays a manual, reviewed step.
# Checks the default (next-boot) deployment, so a fresh staged install counts.
# Local RPMs (e.g. the RPM Fusion release packages) are not checked.
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
wanted=()
for list in "$script_dir"/*-packages.sh "$script_dir"/../gaming/os/*-packages.sh; do
  name=$(basename "$list" -packages.sh)
  # shellcheck source=/dev/null
  source "$list"
  declare -n arr="${name}_packages"
  wanted+=("${arr[@]}")
  unset -n arr
done

layered=$(rpm-ostree status --json | jq -r '.deployments[0]."requested-packages"[]' | sort -u)
listed=$(printf '%s\n' "${wanted[@]}" | sort -u)
mapfile -t missing < <(comm -13 <(echo "$layered") <(echo "$listed"))
mapfile -t extra < <(comm -23 <(echo "$layered") <(echo "$listed"))

if ((${#missing[@]})); then
  echo "Listed but not layered:" >&2
  printf '  %s\n' "${missing[@]}" >&2
  echo "Install with: sudo rpm-ostree install ${missing[*]}" >&2
fi
if ((${#extra[@]})); then
  echo "Layered but not in any *-packages.sh list:" >&2
  printf '  %s\n' "${extra[@]}" >&2
  echo "Remove with: sudo rpm-ostree uninstall ${extra[*]}" >&2
fi
if ((${#missing[@]} + ${#extra[@]})); then
  exit 1
fi
echo "Layered packages match the *-packages.sh lists."
