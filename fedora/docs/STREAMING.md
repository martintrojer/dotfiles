# Streaming to a handheld

Box: AMD RX 7800 XT + LG HDR 4K on `DP-1`. Stream Steam games to a SteamOS
handheld over the LAN via the `Steam (gamescope stream)` SDDM session.

Handheld-as-thin-client is strictly better than local play here, which is the
whole point:

- **Native 1080p, no upscaling.** The 7800XT drives a fixed 1080p SDR target at
  native res with headroom -- no FSR/OptiScaler/DLSS, none of the `optirun`
  apparatus the 4K HDR panel needs. 1080p is pixel-perfect on a ~7" screen.
- **Handheld TDP to ~3W.** It only decodes AV1/HEVC and sends input, so battery
  goes from ~2h local AAA to 6-8h+, near-silent and cool. The desk GPU does the
  heavy lifting on wall power.

## Transport: Sunshine, not Remote Play

Steam Remote Play was tried first (built into `steam`, native res negotiation,
proper frame pacing) and proved unreliable here. Sunshine (KMS capture of the
gamescope DRM scanout) is the adopted transport. Flatpak Sunshine can't do KMS
capture (sandbox can't hold `CAP_SYS_ADMIN`) -- must be the COPR rpm.

## Resolution: 1080p, not 1920x1200

The LG panel is **16:9 only** (modes: 2160p, 1440p, 1080p, 720p). No native
1920x1200, so neither gamescope `--backend drm` nor a host-side swap can produce
the handheld's 16:10 mode. Target **1080p**; the handheld letterboxes it. True
1920x1200 needs an EDID/virtual-display hack -- not worth the Atomic upkeep.

## The session

`Steam (gamescope stream)` runs the `steam-session` launcher with env overrides
(installed by `setup-gamescope-session.sh`):

```bash
GS_OUT_W=1920 GS_OUT_H=1080 GS_HDR=0 GS_SUNSHINE=1 /usr/local/bin/steam-session
```

Fixed resolution at launch (not per-client) avoids the gamescope-restart
segfault dynamic-resolution approaches hit. `GS_SUNSHINE=1` makes
`steam-session` start the rpm's user unit before gamescope and stop it on exit.
The unit is never `enable`d, so Sunshine runs only in this session -- not on the
Sway desktop or HDR session.

A tracked systemd drop-in (`app-dev.lizardbyte.app.Sunshine.service.d/`) forces
`capture=kms` and clears `WAYLAND_DISPLAY`/`DISPLAY`. Without it Sunshine
inherits the stale `WAYLAND_DISPLAY=wayland-1` left in the shared per-user
systemd env by Sway, auto-picks wlr (Wayland) capture, and dies with
`Couldn't connect to Wayland display: wayland-1`. gamescope owns the DRM scanout
and exposes no wlr-screencopy, so KMS (the binary's `cap_sys_admin` file-cap) is
the right path.

## Install

`Sunshine` is a `steam-packages.sh` entry layered by `setup-steam.sh`, like
`lact`: drop the COPR repo file at `/etc/yum.repos.d/sunshine.repo` (group COPR
`lizardbyte/beta` -- stable lags the newest Fedora, #4395), then `rpm-ostree
install` + reboot.

The current rpm is Atomic-correct (older guides describe a workaround for stale
versions): `%post` skips its setcap/uinput steps under rpm-ostree; the caps ship
as **file capabilities** rpm-ostree preserves into `/usr` (no `setcap`, no
copy-to-`/usr/local`); the rpm ships its own udev rule + user unit. Verify with
`getcap /usr/bin/sunshine` (expect `cap_sys_admin`). Do **not** `systemctl
--user enable` it.

Hardware encode also needs `mesa-va-drivers-freeworld` (also in
`steam-packages.sh`): stock mesa ships VAAPI with the H264/HEVC/AV1 encoders
stripped, so without it Sunshine falls back to software x264 -- unusable at 4K.
Verify with `vainfo` showing `VAEntrypointEncSlice`.

Then `setup-sunshine.sh` opens the firewall: the rpm ships no firewalld rule and
the default `public` zone blocks the stream ports, so Moonlight can't reach the
host until they're opened (TCP 47984/47989/47990/48010, UDP
47998/47999/48000/48002/48010, plus the `mdns` service for auto-discovery).

## Pairing (one-time, manual)

Sunshine's config + pairing happens through its **web UI on TCP 47990**
(https://localhost:47990, self-signed TLS -- accept the cert warning). Easiest
from the Sway desktop (the gamescope session has no browser): start the unit
(`systemctl --user start app-dev.lizardbyte.app.Sunshine.service`), open the UI,
set the admin username/password on first run.

Then on the client: open Moonlight, pick the host (or add its IP manually if
auto-discovery fails), and it shows a 4-digit PIN. Enter that PIN on the web
UI's **PIN** page to pair. Pairing persists in
`~/.config/sunshine/sunshine_state.json` across reboots.

## Washed-out colors fix (client-side BT.601)

Colors looked desaturated/washed over the stream but vibrant on the handheld
locally. Cause: Moonlight's VAAPI renderer **hardcodes BT.601**
(`vaapi.cpp: getDecoderColorspace() return COLORSPACE_REC_601`, citing a 2019
Mesa bug that's long-fixed on Mesa 26). Moonlight requests 601 via
`encoderCscMode`; Sunshine's colorspace is **100% client-driven** (no host
override -- `sunshine.conf color_range` is inert), so it encodes 601 and the
colors shift. Confirmed in the host log: `Color coding: SDR (Rec. 601)`.

Fix on the **handheld** (Moonlight is the Flatpak), force BT.709:

```bash
flatpak override --user --env=COLOR_SPACE_OVERRIDE=1 com.moonlight_stream.Moonlight
```

(`COLOR_SPACE_OVERRIDE`: 0=601, 1=709, 2=2020; there's also
`COLOR_RANGE_OVERRIDE` 0=limited/1=full if blacks look grey.) After relaunch the
host log shows `Color coding: SDR (Rec. 709)` and colors are correct. Host needs
nothing.

## Test

Verified butter-smooth with a Legion Go S paired: KMS capture, `hevc_vaapi`,
~15 Mbps, 1080p. Host (in the stream session): web UI at https://localhost:47990;
confirm VAAPI hardware encoder, host wired to ethernet.

Handheld (Moonlight): 1080p, ~15-50 Mbps LAN, perf overlay on, plus the
`COLOR_SPACE_OVERRIDE=1` flatpak override (see above -- else colors wash out).
Codec is client-negotiated -- the Go S picked HEVC on Auto; force AV1 in Moonlight
if you want marginally better quality-per-bit. Verify stream is 1080p (not 4K
boxed), low single-digit ms network latency, VAAPI (not software) encode, and OK
frame pacing (cap fps to client refresh; Sunshine needs manual vsync/gsync-off
tuning).
