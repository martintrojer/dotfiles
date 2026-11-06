#!/bin/bash
# Compare layered packages with the *-packages.sh arrays, both ways:
# - missing: listed but not layered
# - extra: layered but not listed, e.g. a package dropped from a list or a
#   one-off `rpm-ostree install`. rpm-ostree has no "sync to this list" mode,
#   so removal stays a manual, reviewed step.
# Checks the default (next-boot) deployment, so a fresh staged install counts.
#
# RPM Fusion release packages stay out of *-packages.sh: they enable the repos
# the gaming list needs and are first installed by URL (see steam-packages.sh),
# not by name. Once layered they show up in requested-packages; ignore them as
# extras so a major-version rebase that keeps them layered stays quiet.
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

# Repo enablers for the gaming layer; not content packages in any list.
ignore_extra=(
  rpmfusion-free-release
  rpmfusion-nonfree-release
)

layered=$(rpm-ostree status --json | jq -r '.deployments[0]."requested-packages"[]' | sort -u)
listed=$(printf '%s\n' "${wanted[@]}" | sort -u)
mapfile -t missing < <(comm -13 <(echo "$layered") <(echo "$listed"))
mapfile -t extra < <(
  comm -23 <(echo "$layered") <(echo "$listed") |
    grep -vxF -f <(printf '%s\n' "${ignore_extra[@]}") || true
)
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
