# T3 Code

[T3 Code](https://github.com/pingdotgg/t3code) is an "agent harness control
surface" — a desktop/web UI that drives agent CLIs already installed on the
host (Codex, Claude Code, Cursor, Grok Build, OpenCode).

Upstream ships packages for macOS (Homebrew), Windows (winget) and Arch (AUR).
Fedora gets the raw `x86_64` AppImage, so this directory carries the glue:
an installer/updater, a desktop entry, and a launcher wrapper (in
[`fedora/bin`](../bin/README.md), which owns `~/.local/bin`).

## Install / update

```bash
fedora/t3code/setup-t3code.sh          # install, or update if a newer release exists
fedora/t3code/setup-t3code.sh --force  # reinstall the current version
```

The script queries the GitHub releases API, downloads the latest
`*-x86_64.AppImage`, extracts it to `~/.local/opt/t3code/squashfs-root/`, and
installs the icon and desktop entry. It records the installed tag in
`~/.local/opt/t3code/.version` and exits early when already current, so it is
cheap to re-run.

The app must be quit before updating: the script replaces the whole extracted
tree, and swapping it under a running Electron process breaks the app. User
data in `~/.config/t3code` is never touched.

## Decisions

- **Extracted, not run as an AppImage.** Sway Atomic ships FUSE 3 only
  (`fusermount3`, no `libfuse.so.2`), so the AppImage refuses to launch with
  `dlopen(): error loading libfuse.so.2`. Layering `fuse-libs` with rpm-ostree
  would cost a reboot to gain nothing, and `--appimage-extract` needs no FUSE
  at all.
- **No distrobox.** T3 Code's whole job is to spawn agent CLIs against local
  repos. In a container it would see the container's PATH, the container's
  auth state, and no working trees without bind mounts. It belongs on the host.
- **Upstream's in-app updater does not work here** and will complain — it
  cannot patch an extracted tree. `setup-t3code.sh` is the update path.
- **The tracked desktop entry replaces the bundled one.** Upstream's
  `t3code.desktop` uses `Exec=AppRun --no-sandbox %U`, a bare relative name
  that only resolves inside a mounted AppImage. Ours calls the `t3code`
  wrapper. `StartupWMClass=t3code` matches the Wayland `app_id`, so Sway
  associates the window with the launcher entry.
- **`t3code://` is registered** (`MimeType=x-scheme-handler/t3code`) because
  the Clerk sign-in flow returns through that scheme.

## The launcher wrapper

[`fedora/bin/.local/bin/t3code`](../bin/.local/bin/t3code) exists for two
reasons beyond pointing at the extracted tree:

**PATH.** T3 Code spawns `codex`/`claude`/`opencode` as subprocesses. Launched
from fuzzel it inherits the systemd user manager's PATH, which includes
`~/.local/bin` (via
[`10-local-bin.conf`](../systemd/.config/environment.d/10-local-bin.conf)) but
not mise's toolchain — mise is activated by zsh, and a GUI launch never runs
zsh. The wrapper prepends `~/.local/share/mise/shims`. Shims rather than
`installs/node/<version>/bin`: the shim path is version-agnostic, so a mise
upgrade cannot silently strip `codex` from the app's PATH.

**Environment.** The agent CLIs also need the API keys and base URLs in
`~/.zsh/zz-local-env.zsh` (untracked; it holds secrets). zsh sources that file
for interactive shells only, so a GUI launch used to start opencode with no
`MODELBRIDGE_BASE_URL`/`MODELBRIDGE_API_KEY`. The modelbridge plugin then fell
back to `http://127.0.0.1:3000`, could not reach it, warned, and registered
zero models — so every `modelbridge/*` model failed inside T3 Code while the
same model worked from a terminal. The wrapper sources the file.

**Wayland.** `--ozone-platform-hint=auto` selects native Wayland instead of
XWayland; `WaylandFractionalScaleV1` is what actually makes text crisp under
fractional scaling. Verify with:

```bash
swaymsg -t get_tree | grep -o '"app_id": "t3code"'   # app_id ⇒ Wayland-native
pgrep -af 'type=gpu-process' | grep -o 'ozone-platform=[a-z]*'
```

An XWayland client would report a `class` instead of an `app_id`. Running
under XWayland also produced recurring
`GetVSyncParametersIfAvailable() failed` errors that disappear under Wayland.

### Why not WaylandWindowDecorations

That feature is the obvious companion flag and is deliberately **omitted**. It
makes Electron draw its own client-side decorations, so the window requests CSD
via `xdg-decoration`. Tiled, Sway still draws its border; floating, it honours
the request and the frame vanishes:

```
floating: True  border: csd
```

Sway's config pins `for_window [app_id="^t3code$"] border pixel 2` as a second
line of defence — same treatment VS Code and Cider already get. With both in
place a floating window reports `border: pixel  width: 2`.

## Alternative: no install at all

The desktop app is an Electron shell around a local server plus web UI. That
server runs standalone:

```bash
npx t3@latest          # http://localhost:3773
npx t3 serve --host 127.0.0.1   # headless
```

Worth preferring on a server or when you'd rather not manage an Electron
bundle. It needs Node 22.16+/23.11+/24.10+, which mise already provides.

## Verify

```bash
cat ~/.local/opt/t3code/.version
tr '\0' '\n' < /proc/$(pgrep -f 'squashfs-root/t3code --no-sandbox')/environ | grep MODELBRIDGE
t3code &                      # window should appear with a 2px sway border
curl -s -o /dev/null -w '%{http_code}\n' http://127.0.0.1:3773/   # 200
xdg-mime query default x-scheme-handler/t3code                    # t3code.desktop
```

Then float it (`$mod+Shift+space`) and confirm the border survives.

## Known noise

- `Failed to connect to the bus` — appears when launched from a context with no
  `DBUS_SESSION_BUS_ADDRESS`. Harmless in a normal graphical session.
- `Clerk: OS encryption is unavailable ... user will be signed out on the next
  launch` — appears when the app cannot reach the Secret Service. gnome-keyring
  owns `org.freedesktop.secrets` here, so a normal session persists tokens;
  this only showed up in a sanitized `env -i` test run.
