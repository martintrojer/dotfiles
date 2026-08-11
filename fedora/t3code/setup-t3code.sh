#!/bin/bash
set -euo pipefail

# Install or update T3 Code from its upstream AppImage. Idempotent; safe to
# re-run as a normal user. See README.md.
#
# The AppImage is *extracted* rather than executed: Sway Atomic ships FUSE 3
# only (fusermount3, no libfuse.so.2), and layering fuse-libs with rpm-ostree
# would cost a reboot to gain nothing. Extraction needs no FUSE at all.

app=t3code
repo=pingdotgg/t3code
prefix="$HOME/.local/opt/$app"
root="$prefix/squashfs-root"
desktop_dir="$HOME/.local/share/applications"
icon_dir="$HOME/.local/share/icons/hicolor/512x512/apps"
# shellcheck disable=SC1007  # `CDPATH= cd` is the intended prefix guard, not an assignment.
script_dir="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
version_file="$prefix/.version"

force=0
[[ ${1-} == --force ]] && force=1

for tool in curl python3 update-desktop-database; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "error: $tool is required but not installed." >&2
    exit 1
  fi
done

release_json="$(curl -fsSL "https://api.github.com/repos/$repo/releases/latest")"

read -r tag url <<<"$(printf '%s' "$release_json" | python3 -c '
import json, sys

release = json.load(sys.stdin)
assets = release.get("assets", [])
appimage = next(
    (a for a in assets if a["name"].endswith("x86_64.AppImage")), None
)
if appimage is None:
    sys.exit("error: no x86_64 AppImage in the latest release")
print(release["tag_name"], appimage["browser_download_url"])
')"

installed=""
[[ -f $version_file ]] && installed="$(<"$version_file")"

if [[ $installed == "$tag" && $force -eq 0 ]]; then
  echo "T3 Code $tag already installed. Re-run with --force to reinstall."
  exit 0
fi

echo "Installing T3 Code $tag${installed:+ (over $installed)}"

# Upstream's in-app updater cannot patch an extracted tree, so the whole
# squashfs-root is replaced. User data lives in ~/.config/t3code and is
# untouched.
if pgrep -f "$root/$app" >/dev/null 2>&1; then
  echo "error: T3 Code is running. Quit it first." >&2
  exit 1
fi

mkdir -p "$prefix"
workdir="$(mktemp -d)"
trap 'rm -rf "$workdir"' EXIT

curl -fL --progress-bar -o "$workdir/$app.AppImage" "$url"
chmod +x "$workdir/$app.AppImage"
(cd "$workdir" && ./"$app.AppImage" --appimage-extract >/dev/null)

rm -rf "$root"
mv "$workdir/squashfs-root" "$root"

mkdir -p "$icon_dir" "$desktop_dir"
install -m 0644 "$root/$app.png" "$icon_dir/$app.png"
install -m 0644 "$script_dir/$app.desktop" "$desktop_dir/$app.desktop"
printf '%s\n' "$tag" >"$version_file"

# T3 Code re-registers x-scheme-handler/t3code on every launch (Electron's
# setAsDefaultProtocolClient), writing t3code-url-handler.desktop that execs
# the binary directly. A cold URL launch through that entry gets neither the
# wrapper's PATH (no codex) nor the Wayland flags, so reclaim the default.
rm -f "$desktop_dir/$app-url-handler.desktop"

update-desktop-database "$desktop_dir"
if command -v xdg-mime >/dev/null 2>&1; then
  xdg-mime default "$app.desktop" "x-scheme-handler/$app"
fi
command -v gtk-update-icon-cache >/dev/null 2>&1 &&
  gtk-update-icon-cache -f -t "$HOME/.local/share/icons/hicolor" >/dev/null 2>&1

echo "Installed T3 Code $tag to $root"
echo "Launcher: ~/.local/bin/t3code   Desktop entry: $desktop_dir/$app.desktop"
echo "URL scheme handler: $(xdg-mime query default x-scheme-handler/t3code)"
