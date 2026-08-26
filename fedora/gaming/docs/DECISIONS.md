# Gaming Layer Decisions

Decisions specific to the Fedora **gaming layer** ([`README.md`](../README.md)) —
why it breaks the baseline's minimal-overlay rules, and what the streaming stack
used to be.
The *partitioning* decision (why this lives in its own quarantined scope) is in
the [root DECISIONS](../../../docs/DECISIONS.md#quarantine-the-fedora-gaming-layer-into-fedoragaming-as-an-opt-out-scope-accepted-2026-07-12).

Each entry: context, key points, what would justify revisiting.

---

## Accepted (non-obvious)


### RDNA3 FSR4 no longer carries the WMMA workaround (accepted 2026-07-31)

Commit `202012a` removed `DXIL_SPIRV_CONFIG=wmma_rdna3_workaround` from
`optirun` and its documentation. That is intentional for this RX 7800 XT, but
only while its OptiScaler path uses FSR 4.1.1 or newer. Upstream says RDNA3 on
Linux needed the variable with older FSR4 models; FSR 4.1.1 officially supports
RDNA3 desktop GPUs through its INT8 model and no longer needs the workaround.

Check the FSR version in use on the OptiScaler overlay's status line; it must
show FSR 4.1.1 or newer. Then verify the active model rather than inferring it
from that version: enable **Watermark** under the upscaler selection (or set
`Fsr4EnableWatermark=true` in `OptiScaler.ini`), save, and restart the game. The
watermark must report `FSR4-i8`, not `FSR3`; OptiScaler v0.9.4 is the first
release whose bundled FFX 2.3 SDK supplies FSR 4.1.1. This also requires a
current Proton/VKD3D with FFX 2.3 support. White-screen flashes, flickering, or
artifacting are the warning signs of a broken FSR4 image path, although
unsupported Ultra Quality presets can cause the same symptoms.

**Reconsider only if:** a game is pinned to FSR 4.1.0 or older (restore the
variable for that game, not globally), OR the watermark does not report
`FSR4-i8` on RDNA3, OR those visual failures appear and persist on a supported
preset. Check the [upstream FSR4 compatibility list](https://github.com/optiscaler/OptiScaler/wiki/FSR4-Compatibility-List)
before changing the wrapper because the requirement is model- and
version-dependent.

---

### The gaming layer deliberately breaks the minimal-overlay rules (accepted 2026-06-05, amended 2026-08-26)

This box stopped dual-booting: the Windows partition is gone and Windows-era PC
gaming moved onto Linux wholesale. That migration is the reason a `fedora/`
setup that had been proudly COPR-free and minimal now layers RPM Fusion and a
stack of non-CLI desktop packages. The break is intentional and scoped, not
drift — this entry exists so a future cleanup pass doesn't "restore purity" and
amputate the gaming stack.

What the migration forced, and why each was previously avoided:

- **RPM Fusion (free + nonfree).** `steam`, `gamescope`, `mangohud`, `gamemode`
  are not in stock Fedora repos. The old rule was "prefer what stock Fedora
  ships"; full-fat gaming simply isn't there.
- **Host-layered desktop apps, not CLIs.** `base-packages.sh` is "a viable
  minimal bootstrap baseline, not a full daily-driver set," and comfort tooling
  lives in `mise`. None of that applies to Steam/gamescope/mangohud/gamemode:
  they are graphical, need host udev/driver integration (`steam-devices`,
  GameMode's group + governor control), and cannot live in `mise` or a toolbox.

How the break is contained so it doesn't rot the rest of the setup:

- **Quarantined in its own wrapper.** All of it lives in `steam-packages.sh` /
  `setup-steam.sh`, a separate `steam_packages` array. A non-gaming host never
  runs it; the base/sway/mise layers stay minimal exactly as before. The split
  *is* the firewall.
- **No COPRs.** RPM Fusion is the only added repo. Two COPRs were tried and both
  removed; see the rejected entries below.
- **Opt-in and manual.** RPM Fusion must be enabled by hand before
  `setup-steam.sh` (documented in the script header). Nothing in the default
  bootstrap pulls it in.
- **Pillar fit:** #8 (opinionated — this is *the* gaming box now, lean into it),
  #7 (still a thin wrapper around a data list), #4 (each gaming package answers
  a "why not builtin/stock?" — the answer is "it doesn't exist there"). The
  tension is with the self-imposed minimal-overlay goal, which was never a
  pillar, just a default for a CLI-first host. The host's job changed. The
  COPR half of the tension is settled rather than tolerated: both COPRs were
  eventually removed, so the no-COPR rule holds repo-wide again.

**Reconsider only if:** the machine stops being a gaming box (then delete
`steam-packages.sh`/`setup-steam.sh`, the gaming packages, drop RPM Fusion, and
this whole layer reverts cleanly), OR Fedora ships these in
stock repos (unlikely for the nonfree pieces), OR a future Atomic image bundles a
gaming profile that supersedes the manual RPM-Fusion dance.

---

### Handheld streaming is plain Steam Remote Play (accepted 2026-06-26, amended 2026-08-26)

Stream Steam games to a SteamOS handheld (1920x1200 native) with **Steam Remote
Play, from whatever session is running**. No dedicated session, no host-side
stack, no repo, no firewall rules, no pairing UI.

Host resolution does not affect the stream, so there is nothing to configure.
Steam takes the capture ceiling from the client (`Maximum capture: 1920x1080`,
the Legion Go S limit) and renegotiates the PipeWire stream down before the
encoder sees a frame. A 4K session logs `stream format changed (size:
3840x2160)`, then `(size: 1920x1080)`, then `Configuring encoder:
[width=1920][height=1080]` — the same encoder settings a 1080p host produces. A
1080p SDR SDDM entry existed for two months on the theory that a smaller scanout
encodes cheaper; the logs disprove it, and it is gone. Stream quality is set by
the client's resolution limit, a knob on the handheld.

The launcher keeps `GS_OUT_W/H`, `GS_REFRESH`, `GS_HDR`, and `GS_ARGS` for
ad-hoc runs; nothing ships that overrides them.

Rejected for the true-native 1920x1200 (16:10) target: virtual display, EDID
injection, headless gamescope, and headless Sway all need an EDID or kernel-arg
hack and break "pure gamescope, no nesting, launch options just work".
Per-client resolution swapping in one session segfaults when gamescope re-modes
mid-stream, and the Bazzite trick for it assumes a
`gamescope-session-plus@steam.service` user unit this hand-rolled SDDM session
does not have. The LG panel is 16:9 only, so the handheld letterboxes 1080p
regardless.

**Pillar fit:** #4 (the zero-infra option survived; the one piece of local
infrastructure it grew was deleted once measured), #3 (no streaming-specific
config left to explain).

**Reconsider only if:** a client's resolution limit exceeds what the host's
default mode can feed, OR Remote Play is measured, not guessed, to be limited by
host capture rather than encode or network.

---

## Rejected

### Sunshine/Moonlight game streaming (removed 2026-08-26)

Layered the `lizardbyte/beta` COPR rpm and ran Sunshine in the gamescope session
so a Moonlight handheld could KMS-capture the DRM scanout. Verified working to a
Legion Go S (hevc_vaapi, BT.709, ~15 Mbps, 1080p). Removed anyway.

- **Failed pillar #4.** Barely used — streaming to the handheld is an occasional
  novelty, not a habit — and disproportionately complicated to stand up for
  that. Steam Remote Play ships in the `steam` rpm and covers the same use
  cases. Sunshine's edge in latency and AV1 only pays off at a frequency of use
  that never arrived.
- **Playtested after removal:** Remote Play is not worse than Sunshine/Moonlight
  on this LAN, with hardware HEVC encode in place (see
  `mesa-va-drivers-freeworld` in `steam-packages.sh` — both arches). The quality
  gap that justified the layering tax did not survive measurement.
- **Cost.** A COPR on an otherwise COPR-free host, a firewalld script with
  verify/revert modes and a 313-line test, a systemd drop-in forcing
  `capture=kms` and clearing the Sway-inherited `WAYLAND_DISPLAY`, `GS_SUNSHINE`
  wiring in `steam-session`, a streaming doc, and a client-side
  `COLOR_SPACE_OVERRIDE=1` flatpak override to undo Moonlight's hardcoded
  BT.601. Every piece worked around something.
- **It was also aimed at the wrong layer.** Remote Play was dropped here as
  unreliable, but that reads as a LAN problem, and no host-side stack fixes the
  network.
- **Worth knowing if this comes back:** the rpm is Atomic-correct. `%post`
  detects rpm-ostree and skips its setcap/uinput steps, `cap_sys_admin` and
  `cap_sys_nice` ship as file capabilities rpm-ostree preserves into `/usr`, and
  it ships its own udev rule and user unit, so the rpmrebuild and
  copy-and-cap-in-`/usr/local` workarounds in older guides are obsolete. The
  Flatpak is a dead end: KMS capture needs `CAP_SYS_ADMIN`, which the sandbox
  cannot hold (LizardByte #2948/#3953).

**Reconsider only if:** handheld streaming becomes routine, the LAN is fixed,
and Remote Play still falls short on latency or codec efficiency.

---

### LACT GPU undervolt/overclock (removed 2026-07-02)

Ran the `ilyaz/LACT` COPR + `lactd` to undervolt and lightly OC the RX 7800 XT
(Navi 32) on this Windows→Linux gaming box. Removed after months of chasing a
stable point.

- **Didn't earn keep.** The climb only ever went *backwards*: −65 → −55 → −50
  → −30 mV, every step forced by a fresh crash (GPU MODE1 resets, hard
  freezes), and even −30 wasn't proven stable. Linux stresses the card
  differently than Windows (Adrenalin ran −50/1100 mV stable for a year), so the
  Windows numbers didn't transfer and a fresh Linux climb never converged.
- **The final profile, and why the numbers made little sense:**

  | Knob | Final value | Stock | What it bought |
  | --- | --- | --- | --- |
  | `voltage_offset` | −30 mV | 0 | ~nothing — backed off so far it was noise |
  | `max_core_clock` | 3000 MHz | ~2625 | placebo; boost is power/thermal bound, not clock-ceiling bound |
  | `max_memory_clock` | 1275 (2550 eff) | 1219 (2438 eff) | small gain — and the prime silent-crash suspect (shared core+mem voltage) |
  | `power_cap` | 280 W | 280 W | zero; already the stock ceiling |

  Read as a set, the profile was self-defeating. The undervolt was supposed to
  be the whole point ("largest stable offset = free perf + cooler/quieter"),
  but crash-driven backoff walked it down to −30 mV where it does essentially
  nothing — yet it still carried all the crash risk. The core-clock bump raises
  a ceiling the card never hits (boost is power/thermal limited on RDNA3), so
  it's pure placebo. The power cap was set to exactly the stock value, i.e. not
  a tune at all. That left the *only* real lever as a small memory OC — which,
  because core and memory share a voltage rail, was also the most likely cause
  of the silent resets. So the surviving config was: three knobs doing nothing
  or worse, and one marginal knob that was probably the bug.
- **Cost was real.** A COPR that deliberately broke the otherwise COPR-free
  baseline, a `dotfiles-sync` drift check (`lact-drift`), a committed config
  snapshot, a tuning doc, and weeks of crash-chasing. Bad value/complexity
  ratio for a marginal-to-zero benefit.
- **What went:** `_dotfiles_sync/lact_drift.py` + its `lact-drift` task,
  `fedora/data/lact/`, `fedora/docs/LACT.md`, the `lact` package from
  `steam-packages.sh`, the `yaml` unresolved-import allowance in `ty.toml`
  (pyyaml was only used by the drift check), and all LACT references in docs.
  The card runs stock now.

**Reconsider only if:** a future kernel/mesa makes RDNA3 undervolting reliably
stable on Linux AND the perf/thermal win is worth re-adding the COPR + drift
plumbing. The old approach (walk the offset down on every crash) is a known
dead end — climb from stock with the memory OC and suspend/resume in the soak
test, not just a 30-min game session.
