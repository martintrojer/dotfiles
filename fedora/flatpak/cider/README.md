# Cider Flatpak EPIPE Fix

Cider (`sh.cider.Cider`, the Apple Music client) throws a `write EPIPE`
JavaScript error dialog on launch under this flatpak runtime.

## Cause

On startup Cider shells out to `xdg-settings` to register itself as the default
handler for the `music`/`itms` URL schemes. The flatpak runtime does not ship
`xdg-settings`, so the spawn fails and Cider's write to the child's stdin throws
`write EPIPE`, which surfaces as the error dialog.

## Fix

Put an `xdg-settings` shim on the sandbox PATH that forwards to the host binary
via `flatpak-spawn --host`, and grant the app the permissions to use it:

- filesystem (ro) access to the shim dir
- `--talk-name=org.freedesktop.Flatpak` so `flatpak-spawn` works
- PATH prepended with the shim dir

The shim installs to `~/.local/share/cider-shims/` (outside stow, since it must
land in an XDG data dir the sandbox can read) and the overrides are written to
`~/.local/share/flatpak/overrides/sh.cider.Cider`.

## Install on a new machine

Cider is distributed as a single-file flatpak **bundle** (no flatpak remote),
gated behind a login, so the bundle is **not** vendored in this repo.

1. Download the Linux flatpak bundle from Taproom (needs a Cider account):
   <https://taproom.cider.sh> — grab `cider-vX.Y.Z-linux-x64.flatpak`.
2. Install it (user scope):
   ```bash
   flatpak install --user -y ~/Downloads/cider-vX.Y.Z-linux-x64.flatpak
   ```
   The bundle requires `org.freedesktop.Platform//25.08`, already present on a
   baseline Sway Atomic host.
3. Apply the EPIPE fix:
   ```bash
   fedora/flatpak/cider/setup-cider.sh
   ```

The setup script is idempotent; re-run it after reinstalling or upgrading the
Cider flatpak. It errors out early if the flatpak isn't installed yet.

## Verify

```bash
flatpak run --command=sh sh.cider.Cider -c 'which xdg-settings && xdg-settings get default-url-scheme-handler music'
```

Should print the shim path and `sh.cider.Cider.desktop` rather than
`command not found`.

## Upgrading Cider

Cider ships single-file flatpak bundles (no remote). To update:

```bash
flatpak install --user -y ~/Downloads/cider-vX.Y.Z-linux-x64.flatpak
fedora/flatpak/cider/setup-cider.sh   # re-assert overrides (harmless if unchanged)
```
