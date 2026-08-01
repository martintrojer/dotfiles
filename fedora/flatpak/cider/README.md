# Cider Flatpak setup

Cider (`sh.cider.Cider`) registers the `cider`, `itms`, `itmss`, `music`, and
`itunes` URL schemes at startup. Configure those defaults on the host because a
sandboxed application cannot reliably update the host's MIME associations.

The setup also replaces Cider's unfiltered session bus socket with Flatpak's
filtered proxy and denies access to `org.freedesktop.Flatpak`. Cider's MPRIS
names and standard portal access remain available under the bundle and
Flatpak's normal policy.

## Install on a new machine

Cider is distributed as a single-file Flatpak **bundle** (no Flatpak remote),
gated behind a login, so the bundle is not vendored here.

1. Download `cider-vX.Y.Z-linux-x64.flatpak` from
   [Taproom](https://taproom.cider.sh).
2. Install it for the current user:
   ```bash
   flatpak install --user -y ~/Downloads/cider-vX.Y.Z-linux-x64.flatpak
   ```
3. Configure it:
   ```bash
   fedora/flatpak/cider/setup-cider.sh
   ```

The script is idempotent. It also removes files and overrides from the retired
`xdg-settings` compatibility shim. Cider 4.0.9.1 ships Electron 43, whose Linux
protocol registration uses GIO rather than the `xdg-settings` command used by
Electron 42 and earlier.

## Verify

Check the host handlers:

```bash
for scheme in cider itms itmss music itunes; do
  printf '%-7s ' "$scheme"
  xdg-settings get default-url-scheme-handler "$scheme"
done
```

Each line should end with `sh.cider.Cider.desktop`.

Inspect the sandbox boundary without changing it:

```bash
flatpak override --user --show sh.cider.Cider
```

The output should show `session-bus` in `sockets=!session-bus` and
`org.freedesktop.Flatpak=none` under session-bus policy. `PATH` should read
`/app/bin:/usr/bin` — the runtime default — and there should be no read-only
`cider-shims` filesystem grant.

That `PATH` line is set explicitly rather than unset. `--unset-env=PATH` does
not restore the runtime default; it drops the variable and leaves the app with
a host-style `PATH` containing no `/app/bin`, where Cider's own binaries live.

Then perform the functional check that the structural override listing cannot
cover: launch Cider, play a track, confirm the Waybar MPRIS module updates, and
use the play/pause, previous, and next media keys. Also open an `itms://` or
`music://` link and confirm that Cider receives it.

Verified working on 2026-07-31 after removing the shim and the unfiltered
session bus, including across a reboot. `org.mpris.MediaPlayer2.cider` is owned
by `xdg-dbus-proxy` rather than by Cider itself — the app reaches D-Bus only
through Flatpak's filtered proxy, which is the hardening working as intended.
Check it with:

```bash
busctl --user list | grep mpris   # name owned by xdg-dbus-proxy
playerctl -l                      # cider listed
playerctl metadata
```

That ownership change is a success signal, but it also breaks host tooling that
identifies the player by PID or `/proc/<pid>/comm`: the bus name resolves to the
proxy, never to Cider. Identify the player by its MPRIS bus name (`cider`, plus
any `.instanceN` suffix) instead. Cider also exports neither `mpris:identity`
nor `xesam:url` nor `mpris:desktopEntry`, so metadata probes are not a
substitute.

## Limitations

- Setup deliberately makes Cider the host default for all five schemes.
- Replacing the unfiltered session bus is stricter than Cider's bundle. An
  unlisted direct D-Bus integration would need its own narrow name grant.
- Cider's exported desktop file currently omits `x-scheme-handler/itunes`.
  Desktops that require the declared association may ignore that scheme.

## Upgrading Cider

```bash
flatpak install --user -y ~/Downloads/cider-vX.Y.Z-linux-x64.flatpak
fedora/flatpak/cider/setup-cider.sh
```

Re-running setup reasserts the host defaults and sandbox hardening.
