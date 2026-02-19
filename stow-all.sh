#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TARGET="${TARGET:-$HOME}"
MODE="restow"
DRY_RUN=1
VERBOSE=1

usage() {
  cat <<USAGE
Usage: $(basename "$0") [options]

Stow all relevant dotfile packages in this repository.

Options:
  -n, --dry-run      Show what would be changed (default behavior; no filesystem writes)
  -a, --apply        Apply changes (disables dry-run)
  -S, --stow         Stow only (operation mode)
  -R, --restow       Re-stow packages (default operation mode)
  -D, --delete       Unstow packages
  -q, --quiet        Disable verbose stow output
  -t, --target PATH  Override target directory (default: \$HOME)
  -h, --help         Show this help
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    -n|--dry-run)
      DRY_RUN=1
      ;;
    -a|--apply)
      DRY_RUN=0
      ;;
    -S|--stow)
      MODE="stow"
      ;;
    -R|--restow)
      MODE="restow"
      ;;
    -D|--delete)
      MODE="delete"
      ;;
    -q|--quiet)
      VERBOSE=0
      ;;
    -t|--target)
      shift
      TARGET="${1:?missing value for --target}"
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
  shift
done

if ! command -v stow >/dev/null 2>&1; then
  echo "GNU Stow is required but not installed." >&2
  exit 1
fi

MODE_FLAG="--${MODE}"
COMMON_ARGS=("$MODE_FLAG")

if [[ "$DRY_RUN" -eq 1 ]]; then
  COMMON_ARGS+=("--no")
fi
if [[ "$VERBOSE" -eq 1 ]]; then
  COMMON_ARGS+=("--verbose")
fi

# Package groups.
ROOT_PACKAGES=(
  bat
  btop
  gdu
  ghostty
  git
  jj
  nvim
  ssh
  starship
  tmux
  vale
  yazi
  zsh
)

DARWIN_PACKAGES=(
  codex
  hammerspoon
)

LINUX_PACKAGES=(
  fuzzel
  hypr
  kanshi
  mako
  niri
  waybar
)

FEDORA_PACKAGES=(
  containers
  systemd
)

OS="$(uname -s)"
DISTRO_ID=""
DISTRO_LIKE=""

if [[ "$OS" == "Linux" && -r /etc/os-release ]]; then
  # shellcheck disable=SC1091
  . /etc/os-release
  DISTRO_ID="${ID:-}"
  DISTRO_LIKE="${ID_LIKE:-}"
fi

DISTRO_ID_LC="$(printf '%s' "$DISTRO_ID" | tr '[:upper:]' '[:lower:]')"
DISTRO_LIKE_LC="$(printf '%s' "$DISTRO_LIKE" | tr '[:upper:]' '[:lower:]')"

is_fedora_family=0
is_debian_family=0

if [[ "$DISTRO_ID_LC" == *fedora* || "$DISTRO_LIKE_LC" == *fedora* || "$DISTRO_LIKE_LC" == *rhel* ]]; then
  is_fedora_family=1
fi
if [[ "$DISTRO_ID_LC" == *debian* || "$DISTRO_ID_LC" == *ubuntu* || "$DISTRO_LIKE_LC" == *debian* ]]; then
  is_debian_family=1
fi

echo "Using target: $TARGET"
echo "Detected OS: $OS"
if [[ -n "$DISTRO_ID" ]]; then
  echo "Detected distro: $DISTRO_ID${DISTRO_LIKE:+ (like: $DISTRO_LIKE)}"
fi

run_stow() {
  local stow_dir="$1"
  shift
  local packages=("$@")

  if [[ "${#packages[@]}" -eq 0 ]]; then
    return
  fi

  (
    cd "$SCRIPT_DIR"
    stow "${COMMON_ARGS[@]}" -d "$stow_dir" -t "$TARGET" "${packages[@]}"
  )
}

run_stow "$SCRIPT_DIR" "${ROOT_PACKAGES[@]}"

if [[ "$OS" == "Darwin" ]]; then
  run_stow "$SCRIPT_DIR" "${DARWIN_PACKAGES[@]}"
elif [[ "$OS" == "Linux" ]]; then
  run_stow "$SCRIPT_DIR" "${LINUX_PACKAGES[@]}"

  if [[ "$is_fedora_family" -eq 1 ]]; then
    run_stow "$SCRIPT_DIR/fedora" "${FEDORA_PACKAGES[@]}"
  elif [[ "$is_debian_family" -eq 1 ]]; then
    echo "Skipping fedora/* packages on Debian-family distro."
  else
    echo "Skipping fedora/* packages on non-Fedora Linux distro."
  fi
else
  echo "Skipping OS-specific packages for unsupported OS: $OS"
fi

echo "Done."
