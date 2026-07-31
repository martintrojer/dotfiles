# Cider Flatpak URL Handlers

Cider (`sh.cider.Cider`, the Apple Music client) registers custom URL schemes at
startup. Older Cider bundles can show a `write EPIPE` JavaScript error when that
registration tries to run the `xdg-settings` command missing from the Flatpak
runtime.

## Exact interaction

The installed Cider 4.0.9.1 bundle calls Electron's
`app.setAsDefaultProtocolClient()` once for each of these schemes:

- `cider`
- `itms`
- `itmss`
- `music`
- `itunes`

Electron 42 and earlier implemented each call on Linux as exactly:

```text
xdg-settings set default-url-scheme-handler SCHEME "$CHROME_DESKTOP"
```

See Electron's [v42 Linux implementation][electron-42] and
[`setAsDefaultProtocolClient` API][electron-api]. Electron 43 replaced that
command with GIO's `g_app_info_set_as_default_for_type()`; see the
[v43 implementation][electron-43]. Cider 4.0.9.1 declares Electron 43, but the
compatibility shim remains for older bundles and upgrades that retain old client
code.

## Safe fix

`setup-cider.sh` does two things:

1. It runs the host's `xdg-settings` itself to configure Cider as the host
   handler for those five schemes. Re-running it skips handlers that are already
   correct.
2. It puts a read-only `xdg-settings` compatibility shim on Cider's sandbox
   `PATH`. The shim only accepts the five exact `set` calls above and returns
   success; every other argument list fails. It never runs a host command.

The Cider bundle grants the unfiltered `session-bus` socket. A
`--no-talk-name=org.freedesktop.Flatpak` rule alone cannot undo that unfiltered
socket, so setup removes the socket and explicitly denies the Flatpak service.
Cider's own MPRIS names and standard portal names remain available under
Flatpak's default sandbox policy. `flatpak-spawn --host` then fails because the
sandbox cannot reach `org.freedesktop.Flatpak`. Flatpak documents the unfiltered
session bus as a security risk and recommends granting only required D-Bus
names; see [Sandbox Permissions][flatpak-permissions]. The only added filesystem
permission is read-only access to `~/.local/share/cider-shims/`.

The host defaults are stored through the standard `mimeapps.list` mechanism.
The [MIME Applications specification][mime-apps] describes how these defaults
are resolved.

## Install on a new machine

Cider is distributed as a single-file Flatpak **bundle** (no Flatpak remote),
gated behind a login, so the bundle is **not** vendored in this repo.

1. Download the Linux Flatpak bundle from Taproom (needs a Cider account):
   <https://taproom.cider.sh> — grab `cider-vX.Y.Z-linux-x64.flatpak`.
2. Install it for the current user:
   ```bash
   flatpak install --user -y ~/Downloads/cider-vX.Y.Z-linux-x64.flatpak
   ```
3. Configure the handlers and compatibility shim:
   ```bash
   fedora/flatpak/cider/setup-cider.sh
   ```

The setup script is idempotent and errors out before changing anything if Cider
or the host `xdg-settings` command is missing.

## Verify

Check the host handlers:

```bash
for scheme in cider itms itmss music itunes; do
  printf '%-7s ' "$scheme"
  xdg-settings get default-url-scheme-handler "$scheme"
done
```

Each line should end with `sh.cider.Cider.desktop`.

Check the sandbox boundary without changing it:

```bash
flatpak override --user --show sh.cider.Cider
```

The output should show the read-only shim directory, the shim-prefixed `PATH`,
`session-bus` in `sockets=!session-bus`, the MPRIS names from Cider's manifest,
and `org.freedesktop.Flatpak=none` under session-bus policy. It must not show
`org.freedesktop.Flatpak=talk`; standard portal names remain implicitly allowed.

## Limitations

- Setup deliberately makes Cider the host default for the five schemes above.
  Change a scheme with the desktop settings or `xdg-settings` if another app
  should own it.
- The compatibility shim is not a general `xdg-settings`: `get`, `check`, other
  properties, other schemes, and other desktop IDs fail closed.
- Replacing the unfiltered session bus is intentionally stricter than Cider's
  bundle. Portal settings and MPRIS remain available; an unlisted direct D-Bus
  integration would need its own narrow name grant.
- Cider's exported desktop file currently omits `x-scheme-handler/itunes` even
  though its client code registers `itunes`. Setup records the default, but
  desktops that strictly require the declared association may ignore that one
  scheme until Cider fixes its bundle metadata.
- The shim only addresses the legacy startup registration failure. It does not
  proxy unrelated host integration from the sandbox.

## Upgrading Cider

```bash
flatpak install --user -y ~/Downloads/cider-vX.Y.Z-linux-x64.flatpak
fedora/flatpak/cider/setup-cider.sh
```

Re-running setup reasserts the five host defaults and the narrow sandbox
override without broadening permissions.

[electron-api]: https://www.electronjs.org/docs/latest/api/app#appsetasdefaultprotocolclientprotocol-path-args
[electron-42]: https://github.com/electron/electron/blob/v42.0.0/shell/browser/browser_linux.cc#L34-L91
[electron-43]: https://github.com/electron/electron/blob/v43.0.0/shell/browser/browser_linux.cc#L48-L68
[flatpak-permissions]: https://docs.flatpak.org/en/latest/sandbox-permissions.html
[mime-apps]: https://specifications.freedesktop.org/mime-apps/latest-single/
