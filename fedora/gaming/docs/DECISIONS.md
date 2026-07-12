# Gaming Layer Decisions

Decisions specific to the Fedora **gaming layer** ([`README.md`](./README.md)) —
why it breaks the baseline's COPR-free/minimal rules, and the streaming stack.
The *partitioning* decision (why this lives in its own quarantined scope) is in
the [root DECISIONS](../../../docs/DECISIONS.md#quarantine-the-fedora-gaming-layer-into-fedoragaming-as-an-opt-out-scope-accepted-2026-07-12).

Each entry: context, key points, what would justify revisiting.

---

## Accepted (non-obvious)


### The gaming layer deliberately breaks the COPR-free + minimal-overlay rules (accepted 2026-06-05)

This box stopped dual-booting: the Windows partition is gone and Windows-era PC
gaming moved onto Linux wholesale. That migration is the reason a `fedora/`
setup that had been proudly COPR-free and minimal now layers RPM Fusion, a COPR,
and a stack of non-CLI desktop packages. The break is intentional and scoped, not
drift — this entry exists so a future cleanup pass doesn't "restore purity" and
amputate the gaming stack.

What the migration forced, and why each was previously avoided:

- **RPM Fusion (free + nonfree).** `steam`, `gamescope`, `mangohud`, `gamemode`
  are not in stock Fedora repos. The old rule was "prefer what stock Fedora
  ships"; full-fat gaming simply isn't there.
- **A COPR (`lizardbyte/beta` for Sunshine).** Game-stream host, not in stock
  Fedora or RPM Fusion. The SwayFX entry
  (in the [root DECISIONS](../../../docs/DECISIONS.md#swayfx-tried-and-reverted-2026-04-28)) rejected SwayFX partly
  *because* it "adds a COPR on Sericea"; here the COPR earns its place (see
  [`STREAMING.md`](./STREAMING.md)). Note: an earlier
  `ilyaz/LACT` COPR (AMD GPU undervolt/fan/power control) was **removed
  2026-07-02** — see the reverted entry below.
- **Host-layered desktop apps, not CLIs.** `base-packages.sh` is "a viable
  minimal bootstrap baseline, not a full daily-driver set," and comfort tooling
  lives in `mise`. None of that applies to Steam/gamescope/mangohud/gamemode:
  they are graphical, need host udev/driver integration (`steam-devices`,
  GameMode's group + governor control), and cannot live in `mise` or a toolbox.

How the break is contained so it doesn't rot the rest of the setup:

- **Quarantined in its own wrapper.** All of it lives in `steam-packages.sh` /
  `setup-steam.sh`, a separate `steam_packages` array. A non-gaming host never
  runs it; the base/sway/mise layers stay COPR-free and minimal exactly as
  before. The split *is* the firewall.
- **Opt-in and manual.** RPM Fusion and the Sunshine COPR must be enabled by
  hand before `setup-steam.sh` (documented in the script header). Nothing in
  the default bootstrap pulls them in.
- **Pillar fit:** #8 (opinionated — this is *the* gaming box now, lean into it),
  #7 (still a thin wrapper around a data list), #4 (each gaming package answers
  a "why not builtin/stock?" — the answer is "it doesn't exist there"). The
  tension is with the self-imposed COPR-free/minimal goals, which were never
  pillars, just defaults for a CLI-first host. The host's job changed.

**Reconsider only if:** the machine stops being a gaming box (then delete
`steam-packages.sh`/`setup-steam.sh`, the gaming stow packages, drop RPM Fusion +
the Sunshine COPR, and this whole layer reverts cleanly), OR Fedora ships these in
stock repos (unlikely for the nonfree pieces), OR a future Atomic image bundles a
gaming profile that supersedes the manual COPR/RPM-Fusion dance.

---

### Handheld streaming: a 1080p gamescope session, Remote Play tried then Sunshine (accepted 2026-06-26)

Goal: stream Steam games to a SteamOS handheld (1920x1200 native) from this box.
Two parts settled separately: the **session** (stable) and the **transport**
(Remote Play first, Sunshine after it proved unreliable). Full procedure in
[`STREAMING.md`](./STREAMING.md).

**The session** is a second gamescope SDDM entry, `Steam (gamescope stream)`:
the `steam-session` launcher with env overrides
(`GS_OUT_W=1920 GS_OUT_H=1080 GS_HDR=0 GS_SUNSHINE=1`). 1080p because the LG 4K
panel is **16:9 only** (no native 1920x1200); the handheld letterboxes it.
Resolution fixed at launch, not per-client — no new script, no duplicated logic.

**The transport**, alternatives walked:

- **Steam Remote Play (tried first, reverted).** Built into the `steam` rpm,
  negotiates client resolution natively, does proper frame pacing — the
  zero-infra option. Tried on the wired LAN; proved **unreliable** in practice
  (drops/instability), so it didn't stick. Kept as the easy first thing anyone
  should try, but not the answer here.
- **Sunshine (Flatpak).** Dead end: KMS capture needs `CAP_SYS_ADMIN`, which the
  Flatpak sandbox cannot hold. Confirmed across LizardByte issues #2948/#3953.
- **Sunshine (COPR rpm, layered) — accepted.** Older guides describe a painful
  Atomic workaround (rpm `%post` `setcap` aborts the layer on read-only `/usr`,
  #2972; rebuild the rpm to strip `%post`; copy-and-cap in `/usr/local`,
  Sunshine#1075). **Verified obsolete** by inspecting the current
  `lizardbyte/beta` rpm in the dev toolbox: `%post` now detects rpm-ostree and
  skips its steps, `cap_sys_admin,cap_sys_nice` ship as file capabilities that
  rpm-ostree preserves into `/usr`, and the rpm ships its own udev rule + user
  unit. So Sunshine is just a `steam-packages.sh` entry (COPR repo file +
  `rpm-ostree install`); no setup script, no rpmrebuild, no
  cap-on-copy, no tracked udev/unit. Lesson: read the package, not just the
  issue tracker. `beta` not `stable` (stable lags the newest Fedora, #4395).
  Sunshine's quality edge (lower latency, AV1) comes with *manual* per-game
  frame-pacing tuning.
- **Virtual display / EDID-injection / headless gamescope / headless Sway.** All
  aimed at the handheld's true 1920x1200 (16:10). Rejected: the 16:9 panel needs
  an EDID/kernel-arg hack or a separate headless compositor — the fragile,
  black-screen-after-reboot territory the Bazzite/Reddit threads warn about, and
  it breaks the "pure gamescope, no nesting, launch options just work" property.
- **Dynamic per-client resolution swap in one session.** Rejected: restarting
  gamescope mid-stream to re-mode segfaults (the documented Bazzite
  `sunshine-swap.sh` failure), and the trick assumes Bazzite's
  `gamescope-session-plus@steam.service` user unit, which this hand-rolled
  SDDM-launched session does not have.

Non-obvious traps that took real digging, each documented with the fix in
[`STREAMING.md`](./STREAMING.md):

- **Session scoping** — the rpm unit is never `enable`d; `GS_SUNSHINE=1` in the
  stream `.desktop` is the sole trigger, so Sunshine runs only there.
- **KMS-capture vs stale `WAYLAND_DISPLAY`** — a tracked systemd drop-in forces
  `capture=kms` and clears the Sway-inherited Wayland env, else capture dies.
- **`mesa-va-drivers-freeworld`** — stock mesa strips the VAAPI encoders.
- **Firewall** — the rpm ships no firewalld rule (`setup-sunshine.sh`).
- **Washed-out colors** — Moonlight's VAAPI renderer hardcodes BT.601; fixed
  client-side with a `COLOR_SPACE_OVERRIDE=1` flatpak override, not on the host.

Verified butter-smooth to a Legion Go S (hevc_vaapi, BT.709, ~15 Mbps, 1080p).

**Pillar fit:** #4 (Remote Play was the no-infra first try; Sunshine earns its
layering tax only after Remote Play actually failed, not speculatively), #7 (the
stream session is a thin env-override of the existing launcher), #8 (this is the
gaming box — the gaming-layer entry
[above](#the-gaming-layer-deliberately-breaks-the-copr-free--minimal-overlay-rules-accepted-2026-06-05)
already accepted COPR + layering here).

**Reconsider only if:** Sunshine stops earning its keep (then `rpm-ostree
uninstall Sunshine` and revert the `GS_SUNSHINE` wiring — Remote Play remains the
zero-cost fallback), OR the monitor is replaced with a 16:10/native-1920x1200
panel (then the true-native-res virtual-display routes become worth
re-evaluating).

---

## Rejected

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
