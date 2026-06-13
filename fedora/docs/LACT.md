# LACT GPU Tuning

Tuning notes for the AMD GPU via [LACT](https://github.com/ilya-zlobintsev/LACT)
(Linux GPU Configuration And Monitoring tool). LACT is installed from the
`ilyaz/LACT` COPR layered in `steam-packages.sh`; the daemon is `lactd`
(`sudo systemctl enable --now lactd`). Why this box carries a COPR at all (it
deliberately breaks the otherwise COPR-free baseline) is recorded in
[`docs/DECISIONS.md`](../../docs/DECISIONS.md).

This machine: **Radeon RX 7700/7800 XT (Navi 32, RDNA3)**.

> ⚠️ Undervolting/overclocking can crash, corrupt, or (rarely) damage hardware.
> Change **one** variable at a time, test, and write the result down before the
> next step. The log at the bottom is the point of this doc.

## Repo snapshot & drift

The daemon owns `/etc/lact/config.yaml` and **rewrites it on every GUI tweak**
(`apply_settings_timer`), so it is *not* stowed or symlinked. Instead the repo
keeps a plain snapshot at [`fedora/lact/config.yaml`](../lact/config.yaml) as the
committed source of truth, and `dotfiles-sync --check` compares the live file
against it (semantic YAML compare, fedora-only, issue id `lact-drift:config`).

Workflow — "recreate, don't restore":

```bash
# After tuning in LACT, adopt the live values into the repo and commit:
sudo cp /etc/lact/config.yaml fedora/lact/config.yaml

# On a fresh machine, push the repo profile back to the daemon:
sudo cp fedora/lact/config.yaml /etc/lact/config.yaml && sudo systemctl restart lactd
```

`dotfiles-sync --check` prints both commands (plus a `diff`) whenever it detects
drift. Suppress with `--ignore lact-drift:config` if you're mid-experiment.
The snapshot is intentionally a manual `cp`, not a live sync: it should only
capture a value you've **decided** to keep, never a transient slider position.

## Background (RDNA3 specifics)

- RDNA3 has **no absolute voltage slider** — you set a negative *voltage offset*
  applied across the whole V/F curve. Undervolting is therefore the main lever.
- Stock GPU voltage is ~**1.15 V**; these cards typically destabilise around
  **~950 mV**, so there's a lot of headroom — but the *stable* point is
  per-silicon and usually well above that floor.
- **Undervolting often raises performance**: freeing power headroom lets AMD's
  boost algorithm push clocks higher within the same power cap. TechPowerUp's
  review literally recommends a small undervolt as free perf.
- Real-world 7800 XT offsets people run under Linux: **−90 mV** (Sapphire Pulse),
  **−170 mV** (XFX Merc with a 225 W cap), commonly **−100 to −200 mV**. Treat
  these as starting hypotheses, not targets — every chip differs.
- **Linux reports Total Graphics Power (TGP), Windows reports Total Board Power
  (TBP)** — the wattage in LACT/MangoHud reads ~20 W lower than Windows tools for
  the same state. Don't compare them 1:1.
- Core and memory voltage are **shared**, so pushing VRAM also stresses the core.

### Prerequisites / gotchas

- Full overclock controls may need the kernel arg
  `amdgpu.ppfeaturemask=0xffffffff`. On rpm-ostree:
  `rpm-ostree kargs --append=amdgpu.ppfeaturemask=0xffffffff` then reboot.
  Check LACT shows clock/voltage sliders; if greyed out, this is usually why.
- **LACT version matters**: 0.7.0 had a bug where the card wouldn't boost past
  the game clock (~2130 MHz), hurting performance; 0.6.0 was the known-good
  workaround at the time. If max clocks look pinned low, check the LACT version.
- LACT can auto-apply a profile on `gamemode` status — pairs with the
  `gamemode` package in this repo (see `fedora/README.md`).

## What you can tune

| Knob | LACT control | This card's sysfs range | Notes |
| --- | --- | --- | --- |
| Voltage offset | "GPU Voltage Offset" | −450 to 0 mV | **The big lever.** Cooler/quieter, often faster. |
| GPU clock ceiling | "Max Core Clock" | 500–5000 MHz (def top ~2625) | Raising alone rarely helps; boost is power/thermal bound. |
| VRAM clock | "Max Memory Clock" | 97–1500 MHz (def top ~1219) | Small gains; **crashes silently** when too high (no artifacts). |
| Power cap | "Power Cap" (TGP) | card-specific W | Lower for quiet/cool, raise for sustained boost. |
| Fan curve | "Fan Control" | — | Tune last, once the V/F point is settled. |

## Hill-climbing to the optimum

Goal: largest stable undervolt (plus any worthwhile MCLK bump), verified by
*better-or-equal* performance, not just "it booted".

### 0. Baseline

1. Everything stock in LACT, apply, reboot.
2. Pick a **repeatable, demanding** workload. For RDNA3, short synthetic
   benchmarks (TimeSpy/Superposition) are **poor stability indicators** — they
   pass at voltages that crash in real games. Use a known "instability sniper":
   **Star Citizen** and **Starfield** are community favourites; otherwise your
   most demanding actual game, same scene/settings every run.
3. Capture baseline numbers (see logging below) as the first log row.

### 1. Undervolt (the main climb)

Bisect — don't crawl in 5 mV steps from zero.

1. Set GPU Voltage Offset to **−100 mV**. Apply. Run the workload + a soak test.
2. Stable? Go more aggressive (**−150**, **−200**…). Unstable? Back off halfway
   toward the last stable value, then fine-tune in **±10 mV** steps.
3. "Stable" must pass **all** of:
   - 20–30+ min in the instability-sniper game, no crash/black-screen/reset,
   - **no visual artifacts** (sparkles, flicker, corrupted textures),
   - `journalctl -k -b | grep -i amdgpu` shows no ring resets / GPU hangs.
4. The last fully-stable offset is the edge. Then back off **~20–30 mV** for
   daily-driver headroom — RDNA3 instability often shows up as a crash *once an
   hour*, not in 5 minutes, so the margin matters.

### 2. VRAM clock (optional, smaller)

1. With the undervolt locked, raise Max Memory Clock in **~10–20 MHz** steps.
2. GDDR6 has error correction: too-high MCLK **crashes outright with no
   artifacts**, and just-below-crash can *lower* FPS via retransmits. The
   optimum is the highest clock where FPS still climbs and nothing crashes.
3. Remember core+mem voltage is shared — a big VRAM OC can destabilise an
   otherwise-fine undervolt; revisit the offset if so.

### 3. Power limit & fans

- Quiet/cool goal: lower the power cap, watch the FPS cost (RDNA3/MCM scales
  down only so well — expect ~5–15% perf loss for ~20–30% power cut).
- Max-perf goal: raise it, confirm temps stay sane.
- Tune the fan curve last. RDNA3 throttles at ~110 °C junction; sustained
  ~90–96 °C hotspot under full load is normal and safe, though tighter fans pull
  it down.

### 4. Lock it in

- Save the profile in LACT, enable "apply on boot" (`lactd` persists it).
- Re-verify after a clean reboot — boot-time apply can differ from a live apply.

### Rules of thumb

- One variable at a time. Always.
- Document everything, including failures — they define the ceiling.
- "Game starts = stable" is a trap. Soak-test the candidate you intend to keep.

## Logging your values

Log two things: the **per-step tuning journal** (the climb) and the **final
profile** (what's applied). Keep both here so the machine is reproducible.

### Quick capture commands

```bash
# Current applied clocks/voltage table:
cat /sys/class/drm/card*/device/pp_od_clk_voltage

# Live monitoring while testing (clocks, temp, power, fan, busy %):
watch -n1 'sensors amdgpu-* ; cat /sys/class/drm/card*/device/gpu_busy_percent'

# In-game: MangoHud overlay (already configured) shows FPS/clocks/temp/power.
# Bench-to-file: set output_folder in MangoHud.conf, press Shift_L+F2 in-game.

# Kernel sanity check after a test run (resets/hangs = unstable):
journalctl -k -b | grep -i amdgpu | tail -30
```

### Tuning journal

Append a row per experiment. "Result" = stable/unstable + the metric that moved.
Note power is **TGP** on Linux (lower than Windows TBP). **MCLK is in LACT's
effective scale** (what the GUI slider shows); the real sysfs clock is half
(e.g. 2550 effective = 1275 MHz in `pp_od_clk_voltage`).

Unit reference for this card (Gigabyte RX 7800 XT Gaming OC, Navi 32):
stock SCLK ceiling 2625, stock MCLK 2438 (1219 real), voltage offset range
−450..0 mV, power cap 212–280 W (TGP). Controls unlocked via
`amdgpu.ppfeaturemask=0xffffffff` (already set).

| Date | V offset (mV) | Max SCLK | Max MCLK (eff) | Power cap (W) | Workload | Result / notes |
| --- | --- | --- | --- | --- | --- | --- |
| 2026-06-05 | 0 (stock) | 2625 | 2438 | 280 | Cyberpunk 2077 bench (GPU ~99%) | baseline |
| 2026-06-05 | −50 | 2625 | 2438 | 280 | Cyberpunk bench | ✅ rock solid |
| 2026-06-05 | −100 | 2625 | 2438 | 280 | Cyberpunk bench | ❌ GPU reset on bench *exit* (load transition); `amdgpu ... GPU reset(1)` / `device wedged`. Edge sits 50–100. |
| 2026-06-05 | −75 | 2625 | 2438 | 280 | Cyberpunk bench (incl. exit) | ✅ stable on bench (later reset under extended play — see below) |
| 2026-06-05 | −75 | 2625 | 2600 | 280 | Cyberpunk bench | ❌ unstable (matches Windows: 2550 was the Adrenalin-stable mem) |
| 2026-06-05 | −75 | 2625 | 2550 | 280 | Cyberpunk bench | ✅ stable, slight FPS bump |
| 2026-06-05 | −75 | 3000 | 2550 | 280 | Cyberpunk bench | ✅ stable, another FPS bump; survived a clean reboot (lactd re-applied) |
| 2026-06-05 | −75 | 3000 | 2550 | 280 | extended play (multiple titles, incl. suspend/resume) | ⚠️ occasional driver reset under longer real-world load — −75 not bulletproof |
| 2026-06-05 | −65 | 3000 | 2550 | 280 | extended play (multiple titles, suspend/resume) | ✅ stable — chosen edge with safety margin |
| 2026-06-13 | −65 | 3000 | 2550 | 280 | daily driver (sway desktop) | ❌ hard lock / freeze — no log trail (session died mid-journal, `last` shows `crash`); −65 not bulletproof over weeks |
| 2026-06-13 | −55 | 3000 | 2550 | 280 | daily driver | ⏳ backed off from −65 after the freeze; Windows ran −50 stable for a year, so −55 is a conservative re-entry. Soak-testing. |
| 2026-06-13 | −50 | 3000 | 2550 | 280 | daily driver | ❌ −55 still crashed; dropped to −50 to match the year-long Windows-stable floor (1100 mV absolute). Soak-testing. |

Found an existing Windows Adrenalin profile mid-session
(`~/Downloads/over_under_7800.xml`, DevID 747E): 1100 mV, mem 2550, max core
3000, power +15% (TBP). Used to cross-check the Linux tune — see the side-by-side
in **Current locked-in profile** below.

### Current locked-in profile

**The applied values live in [`fedora/lact/config.yaml`](../lact/config.yaml) —
that committed snapshot is the source of truth, not this doc.** `dotfiles-sync
--check` flags drift between it and the live `/etc/lact/config.yaml` (see
[Repo snapshot & drift](#repo-snapshot--drift)). Read the offset / clocks /
power cap there rather than duplicating them here.

Windows reference = the old Adrenalin profile (`~/Downloads/over_under_7800.xml`,
DevID 747E) this tune was cross-checked against. Note the unit differences:
voltage on Windows is an **absolute** value (mV), Linux is an **offset**; power
on Windows is **TBP** (+%), Linux is **TGP** (absolute W) and reads ~20 W lower
for the same state; memory clock matches in LACT's effective scale. The Windows
profile confirmed the 2550 memory sweet spot and 3000 core ceiling; its ≈−50 mV
offset (1100 mV absolute) is the proven-stable floor the Linux undervolt is
tuned around.

- Fan curve: **stock/automatic** (not tuned; idle fans-off, temps fine)
- Applied on boot via `lactd`: **yes** (`systemctl is-enabled lactd` = enabled;
  values verified present in `pp_od_clk_voltage` on a fresh boot)
- History: −65 was the 2026-06-05 chosen edge but **hard-froze under weeks of
  daily use (2026-06-13)**; −55 still crashed, so backed off to −50 — the
  year-long Windows-stable floor (1100 mV absolute). See the tuning journal
  above for the full climb.

## References

- LACT — <https://github.com/ilya-zlobintsev/LACT> (features, kernel arg,
  recovery from a bad overclock, config reference)
- TechPowerUp RX 7800 XT OC review (undervolt-for-perf method, memory climb) —
  <https://www.techpowerup.com/review/amd-radeon-rx-7800-xt/40.html>
- r/linux_gaming "Undervolting 7800 XT with LACT" (real Linux offsets, temps,
  the 0.7.0 clock bug) —
  <https://www.reddit.com/r/linux_gaming/comments/1i6v5mx/>
- Overclock.net 7800 XT Owners' Club (RDNA3 stability ≠ TimeSpy; document
  everything) —
  <https://www.overclock.net/threads/official-amd-radeon-rx-7800-xt-owners-club.1808002/page-9>
