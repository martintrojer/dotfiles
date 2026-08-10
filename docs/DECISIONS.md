# Decisions

Past decisions about this setup — rejected alternatives and non-obvious "yes" choices. Read before relitigating.

Each entry: context, key points, what would justify revisiting. Pillars from [`README.md`](../README.md#zen-of-this-setup); daily editing rules in [`AGENTS.md`](../AGENTS.md).

---

## Rejected

### Splitting the four big scripts into their own repos (rejected 2026-08-01)

A complexity heatmap flagged four files as "programs wearing dotfile clothing": `tms` (993), `agent-attention` (976), `wallpaper` (825), `optiscaler-sync` (938) — ~3.7k lines, 14% of the repo in four files. Considered extracting them.

- **Three are coupled.** `tms` and `agent-attention` import `_tmux_common`; `tms` and `wallpaper` carry generated `THEME BEGIN` blocks from `render_theme.py`. `agent-attention` is the worst candidate — it's a protocol endpoint, not a program: six packages participate in its state machine (pi extension, opencode plugin, mako, `.tmux.conf`, `status-ai`, `integration_checks.py`).
- **Only `optiscaler-sync` is separable** — pure stdlib, no theme block, own test file, two consumers. Still not worth its own repo alone.
- **Cost per extracted repo:** duplicate the four-stage `check-python` gate, lose stow as the install story, turn atomic changes into two-PR dances, and let `render_theme.py` write into a directory it doesn't own.
- The heatmap flagged these files for being **big and churny**, not **misplaced**. Extraction relocates complexity without reducing it.

**Reconsider only if:** `optiscaler-sync` attracts outside users, OR `fedora/gaming/` needs a different release cadence than the shell config (in which case the honest unit is that whole directory, not four scattered scripts).

---

### A shared `pylib/` helper module (rejected 2026-08-01)

With the monorepo kept, the obvious follow-up was one importable helper module for the duplicated `run()` wrappers, XDG resolution, notification sending, and JSON state IO across `tmux/`, `fuzzel/`, `waybar/`, `sway/`, `local-bin/`, `fedora/bin/`.

- **Killer argument:** every script relies on `sys.path[0]` colocation (documented at `fuzzel/_common.py:5-7`). A shared module needs `PYTHONPATH` set in *both* `environment.d` and zsh exports — the same two-place-PATH problem already used to reject a `gaming/` subdir. `environment.d` is also Linux-only, while tmux scripts run on macOS.
- **The duplication is smaller than it looks:** the five `run()` wrappers are five *different* 3-line functions, not five copies. Abstracting them buys nothing.
- **What to do instead:** fix the two real duplications in place — XDG spelled two ways across 6 packages (the `,`-default form is buggy on empty env), and clipboard copy implemented three ways where the best impl is the least used.

**Reconsider only if:** a helper needed by >4 packages appears AND exceeds ~30 lines, OR the macOS surface disappears (removing the `environment.d` portability blocker).

---

### A unified Rust CLI for the script layer (rejected 2026-08-01, re-argued same day)

Revisited as "fold *every* script — pickers, renderers, `_dotfiles_sync`, the lot — into one binary". Still no — and after inspection, not even for the one component that looked like a fair candidate. The original two reasons were both wrong on measurement; keeping them would have got this relitigated.

- **Retracted: the perf pro.** Claimed 25-40ms interpreter start. Measured: `python3 -c pass` is 10ms, ~20ms with typical imports. The hot renderers cost 50-80ms and the bulk is `git`/`swaymsg` subprocesses, not Python. A rewrite removes the 20ms and leaves the 60ms.
- **Retracted: "a build step kills the loop".** Measured warm incremental: `cargo run` 0.12s, `go run` 0.13s — the same, and both invisible. `make check-ts` already costs 4.4s unremarked.
- **The reason it actually loses — fit.** 44 of 72 scripts are text-munging and process orchestration: 493 f-strings, 48 `subprocess.run`, 45 `.splitlines()`, 35 `os.environ.get`, 23 `shutil.which`. `toolboxes:43` parses `toolbox list` by splitting on `\s{2,}` in 16 lines; the honest Rust is ~28 with a `OnceLock<Regex>`, a `Kind` enum, and three `to_string()`s to dodge lifetimes. Screen-scraping another CLI's columns is *inherently* untyped, so types buy least exactly where the code is most script-like. Paying a systems language for a scripting workload.
- **Also:** one binary means one dependency tree and one release cadence for image processing, HTTP+zip, VDF editing, symlink management and 14 pickers. Coupling bought to solve a tooling problem worth ~300 lines of config.
- **Wrong axis.** The question was never Python-vs-Rust, it was untyped-vs-typed, and we are already on the good side. `Literal` under `ty` is a real closed sum type: `h: Action = "bogus"` is an `invalid-assignment`, and a missing branch surfaces as `invalid-return-type`. That is the guarantee the whole idea was reaching for. Go would actually be a *downgrade* here — it has no sum types at all, only string constants.

**The pro that survives, narrowed:** a third of one audit's bugs were contract drift in disguise, but only the *cross-package* ones are still open. `ty` sees inside a package, so the `config.py`/`cli.py` join was fixable in place; it cannot follow `agent-attention.ts` -> `_tmux_common.py`, which is why agent-state stayed bare strings and `STATE_GLYPH` needed a codegen pipeline to have one source of truth. That gap is a stow-layout consequence, not a language one.

**`_dotfiles_sync/` was the one candidate — and reading it closed the case.** 2.5k lines, annotated throughout, `ty`-clean, six frozen dataclasses, `Literal` aliases for `Action`/`PackageScope`, 39 tests, eight stringly-typed lookups left. It also reads well, which is what a rewrite would most likely lose: the value in `stow.py` and `repo_checks.py` is not the types but the comments recording *why* — `--no-folding` avoids the shared-target fold bug, the prune step's parent walk depends on `iter_managed_links` order. That is knowledge about stow, not about Python, so a port carries it verbatim and improves nothing.

**Reconsider only if:** a contract spanning packages starts drifting *despite* `ty` — the agent-state strings are the live candidate, and the cheaper fix there is a shared enum plus the generator that already exists, not a language change.

---


### Arch and Ubuntu toolbox bootstrap scripts (removed 2026-05-01)

`fedora/setup-toolbox-arch.sh` and `-ubuntu.sh` were added in the initial commit and never invoked again. Removed.

- Violates pillar #8 (opinionated — this repo is Fedora-only) and #4 (zero recorded use).
- The arch script was also a "shell vs Python" smell: 43 lines of `getent passwd | awk`, sudo-as-other-user, manual yay bootstrap.
- The Fedora toolbox script stays — it matches the stated stack and is used.

**Reconsider only if:** a daily-driver workflow develops that needs an Arch or Ubuntu toolbox, exercised more than once a quarter.

---

### Alacritty as the cross-OS terminal (tried 2026-04-30, reverted 2026-05-15)

After a Ghostty OOM (~160 GB resident; also nuked `~/.zsh_history` via the `share_history` truncate-and-rewrite race — daily snapshot now in `zsh/.zshrc`), swapped to Alacritty as the unified macOS+Linux terminal. Held ~2 weeks; reverted in favor of one terminal per OS.

- What the swap got right: stability, `alacritty msg create-window` IPC, per-OS config via TOML `import` + nested stow packages.
- Why it didn't last: "one terminal everywhere" is a portability goal, not a quality goal. Alacritty was never the *most* Wayland-native option (foot is) nor the *most* macOS-native option (Ghostty is). Optimizing for sameness paid an opportunity cost on both sides.
- Kept from the swap: the daily history snapshot, `option_as_alt = "OnlyLeft"` thinking (now on Ghostty), the nested-stow per-OS overlay pattern (still used by `fedora/`).

Superseded by ["Each OS gets its native terminal"](#each-os-gets-its-native-terminal-foot-on-linux-ghostty-on-macos-accepted-2026-05-15) below.

---

### `Hyper+G` / `Hyper+1..5` as desktop back-and-forth in Hammerspoon (rejected 2026-04-29)

Recurring temptation: mirror sway's `$mod+g` (`workspace back_and_forth`) and direct workspace-number binds in Hammerspoon. Tried twice.

- **Round 1**, synthetic `Cmd+Tab`: unreliable — macOS treats `Cmd+Tab` as privileged and synthetic events don't behave like real ones. Also semantically wrong: sway's `$mod+g` is a toggle, not an app switcher.
- **Round 2**, `hs.spaces.gotoSpace` / synthetic `Ctrl+1..5`: semantically closer but visibly janky (Mission Control zoom-out), and the Hyper-wrapped Ctrl variant was unreliable even when plain `Ctrl+1..5` worked.
- Violates pillar #1 (fighting Mission Control isn't boring), #3 (flaky synthetic events), #4.

**Reconsider only if:** Hammerspoon gains a reliable native wrapper around previous-desktop behavior that feels native rather than like a Mission Control hack.

---

### Vendoring tmux plugins instead of using TPM (rejected, ongoing)

5 third-party tmux plugins via TPM (`tmux-yank`, `tmux-cpu`, `tmux-fingers`, `tmux-fzf`, `vim-tmux-navigator`). Each audited individually:

- `vim-tmux-navigator`: ~30 lines but handles fzf/ssh edge cases I'd rediscover the hard way.
- `tmux-fingers`: too large to rewrite. *(Now `tmux-fingers-rs`, see below.)*
- `tmux-fzf`: clipboard popup is ~5 lines of bind, just works.
- `tmux-yank`: cross-platform clipboard (`wl-copy`/`pbcopy`/`xclip`) — pillar #8 says to avoid that kind of busywork in shared config.
- `tmux-cpu`: could become a local `status-cpu` script, but TPM is already running for the other 4 so marginal cost is zero.
- TPM itself: ~200 lines of bash that does three things (clone, update, source). Boring and understood.

**Tension:** #5 (local over upstream) vs #2 (TPM *is* the builtin idiom for tmux plugins) vs #4 (each plugin earns its place individually). Pillar #5 fires when adding *new* plugins, not when rewriting well-scoped existing ones.

**Reconsider individual plugins if:** upstream goes unmaintained 12+ months, OR a Wayland-native clipboard solution makes `tmux-yank` unnecessary.

---

### Centralized `docs/` folder for package docs (rejected 2026-04-27)

Considered collecting per-package READMEs into topic-shaped `docs/*.md` files.

- Co-location is the killer feature: editing `tmux/.tmux.conf` puts `tmux/README.md` in your face. Move it elsewhere and the docs go stale.
- Stow packages are the natural unit: deleting `sway/` cleanly removes its docs without remembering to also delete `docs/sway.md`.
- No external audience; future-me is already in the package directory.
- Violates pillar #10 (config next to thing) and #2 (new structure to maintain).

**What changed on reconsideration:** cross-cutting repo docs (this file, `SETUP.md`, `THEME.md`, `VSCODE.md`) moved to `docs/`. Per-package READMEs stayed put.

**Reconsider only if:** I start wanting to move package-specific READMEs to `docs/`. Today the answer is still no.

---

### oh-my-zsh (removed 2026-04-25)

Used OMZ for years. Audited what it actually did.

- Providing: 3 plugins, `..`/`...` aliases, case-insensitive completion, history options, prompt theme. All small primitives.
- Costing: ~60ms startup (160ms → 100ms), an `FPATH` leak workaround, hidden keymaps, framework dependency for ~30 lines of real config.
- Violates #2 (framework around builtins), #3 (thousands of unread lines), #5, #4 (earned its place by inertia).

**Replacement:** ~110 lines of native zsh — `compinit`, `setopt`s, `bindkey -e`, direct sourcing of `zsh-autosuggestions` + `zsh-syntax-highlighting`, `fzf --zsh`, `zoxide init zsh`. Aliases cherry-picked from OMZ's git plugin after grepping history for usage ≥5.

**Reconsider only if:** personal zsh config grows past ~400 lines AND a single framework can replace 3+ ad-hoc subsystems at once.

---

### chezmoi (rejected 2026-04-25)

Audited [chezmoi](https://www.chezmoi.io/) against `dotfiles-sync`'s actual responsibilities.

- chezmoi cleanly replaces the boring half (stow driver, conflict UX, OS scoping). `chezmoi diff` is nicer than custom `--check`.
- chezmoi does **not** solve the hard half: per-skill symlink fan-out into `~/.agents/skills/` doesn't fit the static-target model (drops into `run_onchange_*.sh`, i.e. the same Python under a different roof). Files mutated by other tools (`~/.claude/settings.json`, `~/.codex/config.toml`) don't fit "chezmoi owns the target".
- Violates #2 (heavier abstraction), #3 (`dot_`, `executable_`, `symlink_`, `run_onchange_` + Go templates = more concepts), #10 (source tree no longer mirrors `$HOME` shape).
- Templating, encryption, externals fetcher all unused.

**Reconsider only if:** chezmoi gains a real per-skill fan-out primitive, OR you start needing per-machine templated configs and in-repo secrets at scale.

---

### niri (replaced by sway 2026-04-23)

Ran [niri](https://github.com/YaLTeR/niri) as the daily-driver Wayland compositor for ~6 months. Switched to sway. Quoting the commit: "I'm a grown up now. Stability over visual flare."

Sway wins on merits, not just maturity:

- **Manual layout tree is the actual power move.** Sway exposes the tree directly: `splitv`/`splith`, `focus parent/child`, marks, `move container to mark`, scratchpad. You're *building* the layout. Niri's scrollable model hides the structure — beautiful on ultrawide, stops being a feature the moment you want "this window pinned next to that one." (See [Sway School](https://martintrojer.github.io/sway-school/).)
- **i3 IPC is the standard.** Every Wayland status bar, idle daemon, lock-screen wrapper, screenshot tool assumes the i3 IPC contract. Niri's own IPC is tax paid in glue scripts.
- **Frozen surface area is a positive property.** Sway is feature-complete; config and IPC are learnable in a weekend. Niri is still adding fundamental concepts.
- What niri does better: out-of-box animations and visual polish. Nice; not a reason to switch.
- Violates #1, #2, #5.

**Reconsider only if:** niri ships a stable 1.0 with frozen IPC/config, the i3-IPC ecosystem gains first-class niri support, AND scrollable-tiling gets an addressable-layout escape hatch.

---

### SwayFX (tried and reverted 2026-04-28)

Swapped upstream sway for [SwayFX](https://github.com/WillPower3309/swayfx) for an afternoon. Full effects stack: `corner_radius 8`, 2-pass blur on waybar/mako/fuzzel/swaylock, soft Catppuccin shadows, 10% inactive dim. Reverted same day.

- Nice: rounded corners changed how the desktop *feels*. Glass blur looked good in screenshots.
- Didn't earn keep: most screen-time is terminal + nvim + tmux, none benefit from layer-shell effects. Fuzzel and mako are visible ~2 seconds at a time. Tabbed-layout corners broken upstream ([swayfx#405](https://github.com/WillPower3309/swayfx/issues/405)). Headline animations (fade, window-movement) still on roadmap.
- Violates #1 (fork lags upstream), #2, #4, plus adds a COPR on Sericea.
- Upstream sway has explicitly declined effects ([swaywm/sway#3380](https://github.com/swaywm/sway/issues/3380) and friends, all closed "feel free to fork"). SwayFX is the only path, but cost/benefit still fails today.

**Reconsider only if:** SwayFX ships fade + window-movement animations AND tabbed-corner bugs are fixed.

#### If you do retry: install + config recipe

Kept here (rather than as live config) so this file is the single source of truth for SwayFX.

**Install on Sericea.** SwayFX `Provides:`/`Conflicts:` `sway`, so install fails unless override + install happen in **one** transaction:

```bash
sudo wget -O /etc/yum.repos.d/_copr_swayfx-swayfx.repo \
  "https://copr.fedorainfracloud.org/coprs/swayfx/swayfx/repo/fedora-$(rpm -E %fedora)/swayfx-swayfx-fedora-$(rpm -E %fedora).repo"
sudo rpm-ostree override remove sway --install swayfx
systemctl reboot
```

**Rollback.** `sudo rpm-ostree rollback && systemctl reboot`, or undo on current deployment: `sudo rpm-ostree uninstall swayfx && sudo rpm-ostree override reset sway && systemctl reboot`.

**Config layering that worked.** Add `include ~/.config/sway/config.d/*` at the bottom of `sway/config`, then drop into `sway/.config/sway/config.d/swayfx.conf`:

```
corner_radius 8
titlebar_separator enable

shadows enable
shadows_on_csd disable      # GTK apps double-shadow otherwise
shadow_blur_radius 20
shadow_offset 0 6
shadow_color #1e1e2eaa      # Catppuccin base, ~67% alpha (soft, not pure black)
shadow_inactive_color #18182566

blur enable
blur_passes 2               # 2/4 is readable + cheap
blur_radius 4
blur_noise 0.02             # hides banding on solid colors

default_dim_inactive 0.1
dim_inactive_colors.unfocused #000000FF
for_window [title="^Picture in picture$"] dim_inactive 0.0
for_window [app_id="^Cider$"]              dim_inactive 0.0

# Layer-shell effects. Probe namespaces on YOUR machine first:
#   notify-send probe; sleep 0.3
#   swaymsg -r -t get_outputs | jq -r '.[].layer_shell_surfaces[]?.namespace' | sort -u
# On this box: "first" (waybar), "notifications" (mako), "launcher" (fuzzel),
# "lockscreen" (swaylock).
layer_effects "first"         { blur enable; blur_ignore_transparent enable; shadows enable }
layer_effects "notifications" { blur enable; blur_ignore_transparent enable; shadows enable; corner_radius 8 }
layer_effects "launcher"      { blur enable; blur_ignore_transparent enable; shadows enable; corner_radius 8 }
layer_effects "lockscreen"    { blur enable; blur_ignore_transparent enable; shadows enable }
```

**Companion alpha tweaks** — layer blur only shows through translucent surfaces:

| File | Field | Value |
|---|---|---|
| `fuzzel/.config/fuzzel/fuzzel.ini` | `[colors] background` | `1e1e2ecc` (~80%) |
| `fuzzel/.config/fuzzel/fuzzel.ini` | `[border] radius` | `8` |
| `mako/.config/mako/config` | `background-color` (normal) | `#1e1e2e8c` (~55%) |
| `mako/.config/mako/config` | `background-color` (critical) | `#1e1e2ed9` (~85%) |
| `mako/.config/mako/config` | `outer-margin` | `6` (was `20`; doubles as blur halo) |
| `foot/.config/foot/foot.ini` | `[colors-dark] alpha` | `0.9` |

#### SwayFX gotchas worth remembering

Even if you never re-enable SwayFX, these bit on first setup:

1. **Layer-shell namespaces are app-defined and undocumented — probe.** Trigger surfaces first (`notify-send probe; sleep 0.3`) or empty namespace list. Waybar advertises as `"first"`, not `"waybar"`. Wrong namespace = no error, no warning, no effect.
2. **Mako rejects CSS `rgba()`** — only `#RRGGBBAA` hex. Bad parse silently kills the daemon; check `journalctl --user -u mako`.
3. **80% alpha looks opaque through 2 blur passes.** Calibrate at ~0.50–0.55 for normal surfaces, ~0.85+ for must-read-instantly (mako-critical). Sanity check: drop to `40` temporarily; if it goes glassy, alpha was too high.
4. **Mako's `outer-margin` doubles as the layer-shell surface size** → big blur halo at large values. Drop to ~6 when blur is on.
5. **`blur_ignore_transparent enable` on every layer block.** Required, no exceptions.
6. **`shadows_on_csd disable`** or GTK apps double-shadow.
7. **Pure-black shadows clash with Catppuccin.** Use `#1e1e2eaa` instead of `#000000aa`.
8. **Test single layer effects without a config edit:** `swaymsg layer_effects '"notifications"' '"blur enable"'`. Faster than reload-and-eyeball.
9. **Live-reload paths:** sway `swaymsg reload`; mako `systemctl --user restart mako`; waybar `pkill -SIGUSR2 waybar` (style only); fuzzel re-reads on each invocation; foot re-reads on file change.

---

## Accepted (non-obvious)

---

### Skills are single-agent discipline; orchestration is `mu`'s (accepted 2026-08-04)

The 2026-08 skills audit had to decide, repeatedly, whether to vendor skills that ship their own multi-agent machinery — superpowers' `subagent-driven-development` (503 lines), `dispatching-parallel-agents`, `requesting-code-review`; LifeOS `council`'s parallel debate rounds. The rule that fell out: a skill may describe *how one agent behaves*, never *how several are coordinated*.

- **The duplication is prose vs. a program.** `subagent-driven-development` is `mu`'s `implement → review → address → ship` DAG written as instructions the agent hand-executes. Vendoring it trades a real task graph, per-agent workspaces, and the cherry-pick flow for a description of them.
- **Orchestration skills assume a dispatch primitive.** They call `Task` / `subagent_type`, which pi doesn't have. `council` had been installed and broken for months for exactly this reason — every trigger word loaded 79 lines of SKILL.md that dead-ended at a nonexistent tool. Discovered only because the audit tried to run it.
- **The four superpowers skills kept** (`test-driven-development`, `systematic-debugging`, `receiving-code-review`, `verification-before-completion`) share the property that makes them safe: they change one agent's behaviour in one context, need no dispatch, and therefore compose with `mu` — including inside a `mu` worker pane.
- **`mu` stays a symlink, not a vendored copy.** It's a real program with its own repo and tests; `~/.agents/skills/mu` points at `~/hacking/mu/skills/mu`.
- **Escape hatch for one-shot work:** `pi-subagents` for fire-and-return with no follow-up. Not currently installed; its agents set `inheritSkills: false`, so skills wouldn't reach its children anyway.

The operational form of this, plus nine other rules extracted from the same audit, is *Zen Of These Skills* in [`skills/README.md`](../skills/README.md).

**Reconsider if:** `mu` stops being the daily driver, or pi gains a first-class subagent tool that makes the prose-orchestration skills executable as written (its `examples/extensions/subagent/` is the thing to watch).

---

### Gate the two unchecked languages: shellcheck and tsc (accepted 2026-08-01)

A repo-wide audit found `pi/` (2,955 lines of TypeScript) was prettier-formatted but never type-checked, and 37 shell files across 8 packages had no linter at all — the two largest ungated surfaces in the repo.

- **Measured, not assumed:** `shellcheck --severity=style` over all 37 shell files exits 0 with zero output today. The gate costs ~5 Makefile lines and zero remediation.
- `tsc` closes the TS hole. `ty` was verified to already resolve per-directory sibling `_common.py` imports correctly, so Python coverage was never the gap.
- This is a ratchet, not a testing policy change — see the entry below, which still stands.

**Reconsider only if:** either tool starts producing noise that outweighs the regressions it catches.

---

### Drop the Claude Code plugin; make `skills/` and `pi/` plain stow packages (accepted 2026-07-12)

Removed the Claude Code integration surface (`.claude-plugin/`, `hooks/`, the marketplace install flow) entirely, and converted the two agent source trees from a custom symlink fan-out to normal stow packages.

- **Claude removal:** Claude Code is no longer a supported harness. The universal `~/.agents/skills/` path already reaches every other agent (Codex, OpenCode, Pi, Cursor, Amp, Cline, Warp, OpenClaw); only Claude wanted its own plugin cache, and that special case earned its keep only while Claude was in use. `CLAUDE.md` became the canonical `AGENTS.md` (all agents read it).
- **Fan-out → stow:** `_dotfiles_sync/external.py` grew an ~88-line `_agent_link_apply` helper that hand-rolled per-item symlinks with pruning for skills and pi extensions. Both are now stow packages: `skills/.agents/skills/<name>/` and `pi/.pi/agent/extensions/*.ts` mirror `$HOME`, so `--restow` produces the same links with automatic pruning. Deleted the helper, both wrapper functions, the two apply-tasks, and the now-unused `relative_symlink_target`.
- **Why the skills refactor was blocked until now:** the Claude plugin bundled `skills/` with the plugin-relative shape `skills/<name>/SKILL.md` — a published marketplace contract. Restructuring to the `$HOME`-mirroring `skills/.agents/skills/<name>/` would have broken Claude's in-bundle skill discovery. Claude pinned the top-level `skills/<name>/` shape; removing Claude unpinned it, which is what made the stow conversion possible.
- **The one subtlety — folding:** the repo stows everything `--no-folding` (per-leaf) to avoid the shared-target fold bug. But skills *must* fold so each skill links as one opaque directory symlink — otherwise stow descends per-file and `.stowrc`'s global `README.md`/`LICENSE.*` ignore drops `avoid-ai-writing/LICENSE` and `detector/README.md`. So `skills` opts back into folding via `PackageSpec.fold`, and a `fold_anchors` mkdir (`~/.agents/skills/`) pins the fold level (without a pre-existing real dir, stow folds `~/.agents` into a single repo symlink). Pi extensions are all `.ts`, so the ignore never fires and they stow per-file under plain `--no-folding`.
- Net: fewer moving parts (one mechanism, stow, instead of stow + bespoke fan-out), and `pi`/`skills` now show up in `--check` package coverage like every other package.

**Reconsider only if:** Claude Code comes back as a daily driver (re-add the plugin surface), OR stow's folding behavior changes such that the `fold_anchors` mkdir is no longer needed.

---

### Quarantine the Fedora gaming layer into `fedora/gaming/` as an opt-out scope (accepted 2026-07-12)

The main rig's gaming/streaming/RGB stack (Steam, gamescope, Sunshine,
OptiScaler, MangoHud, GameMode, OpenRGB, controller-firmware) is a deliberate
break from the baseline's COPR-free/minimal rules — that break was already
accepted (why it earns its place is in
[`fedora/gaming/docs/DECISIONS.md`](../fedora/gaming/docs/DECISIONS.md)). The
problem was that it had *sprawled* across `fedora/os/`, `config/`, `bin/`,
`systemd/`, `data/`, `docs/`, and two repo-root packages (`gamemode/`,
`mangohud/`), so the baseline/break boundary was invisible and there was no way
to stand up a gaming-free work box from the same repo.

The fix:

- **Physical quarantine.** All gaming material moved under one namespace,
  `fedora/gaming/` (`os/`, `config/`, `data/`, `docs/`, plus a single `home/`
  stow package for what lands in `$HOME`). `fedora/` is now a clean COPR-free
  Sway baseline; `fedora/gaming/` is deletable as a unit. `gamemode`/`mangohud`
  configs folded in from repo-root (they were pure-gaming, a leak).
- **Opt-out scope.** `dotfiles-sync` gained a `gaming` scope, default-on for
  Fedora and dropped by `--skip-gaming`. Default-on because the daily driver
  *is* the gaming rig; the flag exists for work/laptop hosts.

Alternatives weighed:

- **A separate OS for gaming (SteamOS / Bazzite dual-boot).** Researched and
  rejected. SteamOS desktop mode is a stale, locked-down appliance; single-SSD
  dual-boot is bootloader-fragile and fights the "boring" pillar; a second
  drive + Bazzite would work but throws away the Sway gaming mode that already
  works. The complaint was repo hygiene, not a missing capability — wrong tool.
- **`~/.local/bin/gaming/` subdir for the helpers.** Rejected: PATH is set in
  two places (`zsh/.zsh/exports.zsh`, `environment.d/10-local-bin.conf`) and
  neither recurses; the gaming helpers are exactly the ones invoked
  non-interactively (SDDM session, `.desktop`, systemd unit), so a subdir would
  need *both* PATH entries edited — more surface to solve a cosmetic problem.
- **Opt-in `--include-gaming`.** Rejected: would force the main rig to pass a
  flag on every apply. Default-on + `--skip-gaming` matches actual usage.
- **Leaving `~/.config` reorg as a goal.** Dropped: stow decouples repo layout
  from `$HOME`, and almost nothing gaming lands in `~/.config` anyway
  (`/etc`, `~/.local/bin`, SDDM dirs). Repo cleanliness was the real, achievable
  win; `~/.config` cleanliness was neither the problem nor changed by this.

**Pillar fit:** #8 (the rig is a gaming box, but the *repo* serves many hosts),
#2 (one namespace + one scope flag, no framework).

**Reconsider only if:** the rig stops gaming (then `rm -rf fedora/gaming/` and
drop the `gaming` scope — the baseline is already clean), OR a second host also
needs gaming (the scope already supports it; no per-host branching required).

---

### Cmd+C / Cmd+V everywhere via Rainy75 firmware layer, not xremap (accepted 2026-05-28)

MacOS muscle memory wants one chord for copy/paste everywhere. Linux gives two: `Ctrl+C` in GUI apps, `Ctrl+Shift+C` in terminals (because `Ctrl+C` is SIGINT). Living with both was a constant low-grade annoyance — different motor program per focused-app type, no way for the brain to fork it cleanly.

The walk through the alternatives:

- **xremap with per-window-class rules** is the only thing that actually delivers "Super+C means copy in Chrome but stays uninterpreted in foot." Rejected: not packaged in Fedora, needs `cargo install`, needs a udev rule on `/dev/uinput` plus `input` group membership, needs a user systemd unit. Too much infrastructure for a chord preference (pillar #1, #4). Vendoring the static-linked binary was considered — rejected for the same reasons plus opacity in `jj diff`.
- **keyd / input-remapper** are in Fedora but global-only on Wayland. No per-app context means any Super→Ctrl remap turns terminal Super+C into SIGINT. Dead end.
- **Sway-level synth with wtype** can't scope bindsym per focused window without a focus-watcher hack. Racy, not worth it.
- **Going back to defaults** (`Ctrl+Shift+C/V` in terminals, `Ctrl+C/V` in GUI) is the obvious right answer for most people, and was briefly accepted. The two-chord cognitive split never embedded after years of trying.

The accepted solution: **the programmable keyboard does the translation in firmware.** Rainy75 supports VIA. Left Cmd is `MO(1)`; on layer 1, `C`→`LCTL(KC_INS)`, `V`→`LSFT(KC_INS)`, `X`→`LSFT(KC_DEL)`. `Ctrl+Insert` / `Shift+Insert` / `Shift+Delete` are the universal Linux legacy clipboard chords that every GTK/Qt/Chromium app and every terminal already honors. Foot needed a small additive config to accept them alongside its `Ctrl+Shift+c/v` defaults (and `primary-paste=none` to free `Shift+Insert` from its default primary-selection binding).

Right Cmd stays raw Super so the sway `$mod+...` universe is untouched. Asymmetric split, one hand each.

Why this is the right layer:

- Firmware doesn't care about Wayland security, app focus, or distro. The remap lives below the OS entirely.
- Zero new infrastructure: no daemon, no udev rule, no group, no systemd unit, no COPR, no vendored binary. Repo policy intact.
- Additive at the foot layer: every existing chord (`Ctrl+Shift+C/V`, `Ctrl+C` = SIGINT, mouse-selection-to-clipboard) still works. Anyone with standard Linux muscle memory hits zero surprises.
- Portable: the keyboard does the same thing on any host. macOS users get Cmd+C natively from Mac mode (Win key remapped to Cmd position by firmware), so no layer needed there.

**Reconsider only if:** the Rainy75 dies and the replacement keyboard lacks programmable layers, OR an app appears in the daily stack that ignores `Ctrl+Insert` / `Shift+Insert` (rare — some Electron and Java Swing apps).

---

### Waybar is a restrained block statusline, not a widget strip (accepted 2026-05-23)

Waybar should visually read as the topmost statusline of the Sway/tmux/nvim stack. The old slash/backslash spacer layout made it feel like floating text with decorative separators; the current design uses square, tmux-like blocks with small 1px gaps. The right side is grouped by function (system stats, connectivity, audio, clock/tray/power) using only Catppuccin surface colors, while the window title is plain text on the translucent bar instead of a long filled slab.

Several more colorful variants were tried first: full pastel fills, then high-alpha pastel cluster backgrounds, then only selected pastel clusters. Dark text on pastel blocks was very legible, but it reduced refinement and fought the terminal-first desktop. The accepted compromise is deliberately bland: surfaces for structure, lavender only for the active workspace, red only for warnings/offline states. This keeps the bar aligned with tmux/nvim, where accents are meaningful rather than decorative.

The custom spacer modules were removed once 1px CSS margins between blocks provided the same visual separation without extra config objects. Unused modules (`sway/mode`, `battery`, `backlight`) were also removed because they were configured but not displayed. `sway/mode` can return if Sway binding modes become part of the workflow; today resize is done through direct keybindings.

A one-pixel seam under the bar on bright wallpapers appears to be a Sway/Wayland fractional-scale artifact (`scale 1.5`) rather than a Waybar border. CSS borders, padding, and negative Waybar margins did not fix it reliably. Keep the bar slightly translucent (`alpha(@base, 0.75)`) and avoid spending more time on the seam unless it becomes worse or an upstream fractional-scale fix lands.

### nvim format-on-save uses a hand-rolled LSP → CLI fallback, not conform.nvim / none-ls (accepted 2026-05-15)

Markdown table formatting was the trigger. The old autocmd called `vim.lsp.buf.format()` and stopped, so filetypes whose attached LSP doesn't format (markdown, lua, sh, raw json/yaml) silently weren't formatted. Obvious answer is `conform.nvim`; chosen answer is ~30 lines of glue in `nvim/.config/nvim/lua/format_on_save.lua`.

- **Flow:** on `BufWritePre`, try LSP first; if nothing formats, look filetype up in a small table (prettier for markdown/json/yaml/etc., stylua for lua, shfmt for sh/bash but not zsh); shell out via `vim.system({...}, { stdin = ... }):wait()`, replace lines only on real diff. Missing binaries are silent no-ops. Trailspace trim runs last.
- **Why not conform/none-ls:** failed #4 (plugin wrapping shell-outs to formatters on `$PATH`), #2 (`vim.system` + a table is the builtin path), #5 (30 lines is the textbook local-script case). Conform's ordering/async/per-formatter-options features are real but unused here.
- **Why not prettier-as-LSP via efm-langserver:** swaps a plugin for a daemon, doesn't reduce moving parts.
- Latency: `prettier` over stdin is ~150–300ms; swap for `prettierd` if it ever bites.

**Reconsider only if:** the formatter table grows past ~10 filetypes or needs per-formatter args / range formatting / async coordination.

---

### Each OS gets its native terminal: foot on Linux, Ghostty on macOS (accepted 2026-05-15)

Replaces the brief alacritty-everywhere experiment ([above](#alacritty-as-the-cross-os-terminal-tried-2026-04-30-reverted-2026-05-15)). Principle: **let each OS bloom in its own direction.** Most-native-per-platform beats same-everywhere.

- **Linux → foot.** Ships in Sericea. Wayland-native. Run as a server (`sway-foot-server.service`); `footclient` spawns near-instant windows sharing one process — right shape for sway's many-windows model. fuzzel and waybar launch `footclient` directly.
- **macOS → Ghostty.** The most macOS-native option: native AppKit, real app-bundle launch, single-process tab/split model aligned with Mission Control. 2026-04-30 OOM acknowledged as a known risk; daily `~/.zsh_history` snapshot is the mitigation that keeps it survivable.
- **Shared behavior lives in tmux/zsh.** Always-inside-tmux means the terminal's job is narrow: rendering, input, clipboard, current-Space window creation.
- **No fallback chains.** Hammerspoon binds Ghostty-only; sway binds footclient-only. If a daemon is down, fix the unit.
- **Pillar fit:** #8 (each OS gets its native primitive), #4 (best fit per OS, not a both-sides compromise), #2 (foot ships in Sericea; Ghostty is the macOS native option).

**Reconsider only if:** Ghostty has a second OOM-class incident (then revisit, likely WezTerm or back to Alacritty), OR foot regresses on Wayland, OR always-in-tmux stops holding.

---

### Tmux owns in-buffer text ops; the terminal owns only what it structurally can (accepted 2026-05-02, revised 2026-05-15)

Recurring temptation: every modern terminal (Alacritty, Ghostty, foot) has hint/url-picker/vi-mode. Each overlaps with tmux copy-mode + `tmux-fingers-rs`. Resist.

**Killer fact, given always-inside-tmux:** the terminal operates on the *viewport* — the rendered frame tmux paints. It cannot see tmux's per-pane scrollback. So terminal vi-mode "scrollback" is just the current frame, terminal hints can't match buffered content, and the terminal can't paste back into the active pane.

| Use case | Owner | Why |
|---|---|---|
| Scrollback search/nav/copy | tmux copy-mode | sees per-pane history |
| Pattern-pick + paste back | `tmux-fingers-rs` (`prefix + Tab`) | one-keystroke paste-back |
| Open URL in browser | terminal | foot URL mode (`Ctrl+Shift+o`), Ghostty Cmd+click |
| RGB / undercurl | terminal | wired via `terminal-features` |

**Carry-over notes worth keeping:**

- `Ctrl-v` rectangle-toggle and `Alt-Up`/`Alt-Down` shell-prompt jumping (OSC 133) live in copy-mode-vi.
- tmux 3.6a's OSC 133 parser accepts only BEL, not ST (despite the man page).
- OSC 133;A from precmd gets stomped by zsh's prompt-redraw — embed in `PROMPT` via `%{..%}`.
- foot's URL mode is *not* expanded to non-URL patterns; those belong to tmux-fingers.

**Reconsider only if:** workflow stops being always-inside-tmux, OR terminal gains structural access to tmux scrollback (would need IPC tmux doesn't expose), OR tmux copy-mode regresses.

---

### Per-tool learning guides, authored as Markdown under `guides/` (accepted 2026-05-01)

Browser-friendly walkthroughs with quizzes for keyboard-heavy tools where forgetting a binding costs time. Each per-package README links to its matching guide.

- Originally hand-written HTML with ~85% duplicated boilerplate (~188 KB). Replaced with `guides/*.md` rendered by stdlib-only `guides/build.py` to gitignored `guides/build/*.html`. One CSS, one JS, one wrapper template. `make {build,serve,check}-guides`.
- **Why Markdown + tiny Python over Hugo/mdBook:** Hugo would add a binary dep for a 5-page site whose features (tabs, quizzes, scoreboard) Hugo doesn't model anyway. Python is already required by `_dotfiles_sync`. Pillars #2, #3, #5.
- **Why a Markdown subset:** the format needed is tiny (headings, bullets, inline code/bold/links, ```` ```quiz ```` blocks with TOML). Constrained format → ~80-line renderer, zero third-party dep.
- **Why `guides/` not `docs/`:** source is Markdown, gets committed; HTML is build artifact in gitignored `guides/build/`. Pillar #10 (source next to its build target).

**Reconsider only if:** an individual guide stops being opened over 12 months (delete just that one), OR the Markdown subset has to grow far enough that a real library is cheaper than maintaining the renderer.

---

### Keep `_dotfiles_sync/` split across small modules (accepted 2026-05-01)

Audited the 12-file split (1,395 lines, largest file 291). Considered collapsing.

- Smaller files win on readability per file. Directory listing reads as a TOC.
- Collapsing would save a few cross-imports at the cost of 250-line modules with multiple concerns.
- `model.py` is types-only Java-style; cross-imports between `external.py` and `integration_checks.py` smell mildly. Acceptable.

**Reconsider only if:** module count crosses ~20, OR cross-imports form a real cycle, OR a single function needs to span three modules.

---

### Keep the `tuicr/` theme override (re-affirmed 2026-05-01 after a brief mis-deletion)

The `tuicr/` package contains one line: `theme = "catppuccin-mocha"`. Briefly classified cosmetic and deleted; immediately restored when `<leader>gt` rendered white-on-dark and was unreadable inside nvim.

- tuicr's default is white. Inside Catppuccin foot/ghostty + nvim it's *functionally unusable*. Pillar #9 (one palette everywhere).
- **Lesson:** the right test for cosmetic-vs-functional on one-file packages isn't "does it change colors?" — it's "would removing this degrade the workflow that uses the tool?" tuicr: yes. vale: yes (alert-level). Both keep.

**Reconsider only if:** tuicr ships a dark-respecting default, OR the desktop palette changes.

---

### Keep the `summarize` naming triplet (accepted 2026-05-01)

Three things named "summarize": the stow package (CLI config), the agent skill, the CLI binary. Intentionally co-named — they're facets of the same workflow (skill calls CLI, CLI reads config). Renaming any would obscure the relationship. One-second disambiguation cost when reading `git log`; acceptable.

**Reconsider only if:** a fourth unrelated "summarize" tool enters the repo.

---

### Replace GNU Stow with an in-repo symlink planner (accepted 2026-08-01)

Stow was doing very little. We passed `--no-folding` on 33 of 34 packages, which turns off its signature feature, then regex-scraped its English output to recover the package/source/target structure we had just handed it.

- **Deleted the seam.** Seven of thirteen functions in `stow.py` existed only to talk to stow: probe the binary, filter its chatter, parse conflicts, parse foreign targets, map an output line back to a package. `package_for_conflict` was recovering information we passed in.
- **One ignore rule set, not two.** `.stowrc` fed stow; `repo_checks` hand-reimplemented stow's matching so the backlink audit could agree with it — including a `SystemExit` guard for stow's footgun that slash patterns silently never match. Now `ignore.py`, shared by planner and audit.
- **Folding became a declaration.** `fold` + `fold_anchors` + pre-`mkdir` existed to coax stow into linking `~/.agents/skills/<name>` rather than folding `~/.agents` whole. Now `bundle_dirs = (".agents/skills",)` says it outright.
- **Conflicts are objects.** `LinkState` (OK / MISSING / STALE / CONFLICT) replaces string matching, so `--check` reports per-link instead of forwarding stow's stdout.
- **Not a line-count win.** ~360 removed against ~265 added; roughly break-even. The case is bug density: three bugs fixed in one audit (`meta_stow_skip_pkg_wrong_index`, `meta_stowrc_slash_branch_dead`, the ignore-scan issue) were all in the seam, none in the linking.
- **Verified against a golden capture.** All 158 stow-created links reproduced byte-for-byte, in the plan and in a real `--apply` to a scratch target. The other 4 repo-pointing links in `$HOME` turned out to be `mise` and `systemctl enable` artifacts, never stow's.

**What we gave up:** `stow -t ~ zsh` as a bail-out on a machine where `dotfiles-sync` is broken. Pillar #6 already says recreate rather than restore, and a fresh machine runs `--apply` anyway.

**Reconsider if:** the planner develops edge cases stow had already solved — cross-device links, adopt semantics, or partial-`$HOME` states — faster than the tests pin them.

---

### Tests are bug-driven, not coverage-driven (accepted 2026-05-01, rewritten 2026-08-01)

Was "no unit tests for the control-plane"; three amendments in one day inverted it (44 → 288 checks, seven directories). Restated as a rule rather than a pile of exceptions.

- **Earns a test:** it can fail *without you noticing* — headless renderers, silent wrong answers, destructive/unattended paths.
- **Does not:** being large, complex or important. Loud failure is covered by pillar #3. `tms` is 993 lines; only its render path is tested. Still bare on purpose: the list parsers, `compact_condition`, `infer_remote`, ~700 of `wallpaper`'s 825 lines, `local-bin/`, most of `fedora/bin/`.
- **Write it when the bug lands**, not before. Every suite here pins something that already broke; `wallpaper`'s found a live data-loss bug while being written.
- **A test that cannot fail is worse than none.** Two shipped in one session, both asserting things true regardless of the code. Break the code, watch it fail, then commit.
- **Location:** repo-internal under the package; for stowed scripts, beside the script (`sys.path[0]` colocation) with `.stowrc --ignore=tests`. If no `make` target discovers it, it is not a test.

**Reconsider if:** test code outgrows script code without a bug history, OR `check-all` (~32s) gets slow enough you skip it, OR you are writing a test to satisfy the rule rather than pin a failure.

---

### Split the repo control plane into `dotfiles-sync` + `_dotfiles_sync/` (accepted 2026-04-27)

Replaced a ~1300-line bootstrap script with a root entrypoint (`dotfiles-sync`) and a repo-local Python package (`_dotfiles_sync/{cli,config,system,stow,checks,external,model}.py`). Cross-cutting repo docs moved to `docs/`; the tiny `vscode/` pseudo-package was demoted to `docs/VSCODE.md`.

- Driver: the old script mixed stow orchestration, package inventory, pinned clones, agent fan-out, drift checks. No longer read like intentional software.
- Top-level stow packages, `fedora/`, `skills/`, and `pi/` all stayed put.
- Improves #1, #3, #10. Tax is one new directory.

**Reconsider only if:** control plane shrinks below ~300 lines (collapse the split), OR grows into a reusable tool (promote to standalone).

---

### Switch to `martintrojer/tmux-fingers-rs` (accepted 2026-04-27)

Replaced `Morantron/tmux-fingers` (Crystal) with a personal Rust port.

- **Driver:** Crystal toolchain is painful on macOS and Linux. Homebrew's formula lags, distro packages are inconsistent, LLVM dep makes container builds slow. Upstream's prebuilt binaries are Linux x86_64 / Intel macOS only. On Apple Silicon and the Linux laptop, the real path was "install Crystal first" — which kept failing. Rust port: `cargo install` works everywhere.
- **Why this looks like it contradicts "vendor tmux plugins" above:** that entry rejected *forking* on the grounds that fingers was too large to rewrite and a fork would freeze upstream. The Rust port already exists with feature parity. Upstream tracking is preserved via an `upstream-crystal` branch that fast-forwards from Morantron's master; new commits get manually ported with `Port:` messages.
- Installed via TPM (`set -g @plugin 'martintrojer/tmux-fingers-rs'`); binary is `tmux-fingers-rs` so it can coexist with upstream on `$PATH`. All `@fingers-*` option names unchanged.

**Reconsider if:** Crystal's install story improves dramatically, OR porting upstream becomes >1 hr/quarter, OR a critical upstream fix lands and we're slow to port twice in a row.

---

### Agent state awareness: tmux scripts, not herdr (accepted 2026-05-29)

herdr (`github.com/ogulcancelik/herdr`) is a Rust terminal multiplexer purpose-built for coding agents. It replaces tmux and adds per-pane agent-state detection (`idle / working / blocked / done`), a cross-workspace urgency sidebar, a socket API for agents to drive panes, and persistence with session restore.

Evaluated it against the existing tmux setup. Decision: stay on tmux, port the one idea that earns rent (agent state awareness), skip the rest.

- **What herdr does better:** real-time agent-state sidebar across workspaces. Our old `agent-attention` script had a binary flag (`!` or nothing). herdr's model is richer.
- **What we'd lose:** `vim-tmux-navigator` (prefix-free nvim↔tmux pane movement), `tmux-fingers-rs` (hint picking), `tms` session recipes, the TPM plugin ecosystem, 17 years of tmux stability, and full control over the status bar. herdr's always-visible left sidebar wastes screen real estate for information you only need occasionally — `prefix+a` surfaces the same data on demand without a permanent column tax. herdr is v0.x, one maintainer, AGPL, with active churn.
- **What we built instead:** upgraded `agent-attention` from a binary attention flag to a three-state push model (`working / blocked / crashed`) using an `IntEnum` with urgency as the value. Pi's extension pushes state on `agent_start`, `agent_end`, and `session_shutdown`. A pid-liveness reaper detects crashes (zero-fork `os.kill(pid, 0)`). Events are stored in SQLite WAL for crash detection and picker history; `@agent_state` tmux window option is the authoritative display state, written by the extension (direct tmux calls for `agent_end`/`session_shutdown`, Python script for `agent_start` which records the pid). Status pill and window glyphs color by state. `prefix+a` popup groups by urgency with idle agents at the bottom.
- **Design rule applied:** port the idea, keep the substrate. tmux is the substrate. herdr is a collection of ideas, one of which was worth stealing.
- **Scope guard:** push-only, no terminal scraping. Pi extension resolves the tmux pane at load time via `tmux display-message` (works across toolbox/container boundaries where `TMUX_PANE` is stripped). Other agents (codex, opencode) still emit the old single-bit `blocked` signal via the `notify` subcommand — they don't get `working` or `done` because they don't push `agent_start`/`agent_end`, and their own signals can't stand in: opencode's `session.idle` fires both for "finished" and "waiting on you", and codex's `notify` carries no state at all, so inferring `done` would mean guessing or scraping. It cuts the other way too: pi has no permission *event* (approval is an in-process `ctx.ui.confirm()`), so its unfocused `agent_end` writes `done` and `blocked` is now notify-only. Capability table in [`tmux/README.md`](../tmux/README.md#harness-capability).

**Reconsider if:** something extraordinary happens. tmux has been the terminal substrate for 17 years and the switching cost is total.

**Extended 2026-08-09 — two more herdr ideas ported.** The decision matured rather than changed: same substrate, same push-only rule, two more borrowed ideas.

- **Per-agent glyphs instead of a rollup count.** `status-ai` now renders one glyph per running agent, grouped into contiguous colored runs, urgency-descending, behind a robot icon (the `AI` label is gone — the icon is the label). "Three working, one blocked" is a shape you read, not a number you decode.
  - **Amended 2026-08-10 — per-state rollup replaces the global `+N` tail.** The shipped version fitted runs into a 10-glyph budget and summarised the overflow as one muted `+N`, which erased *state identity*: `x !!! >>>>>> +3` and `x vvv >>>>>> +3` are different fleets that render alike, and a five-state fleet collapsed nine agents into a `+10` from which nothing could be recovered. Now each run repeats its glyph up to `ROLLUP_THRESHOLD` (3, chosen on a live render) and reads `<N><glyph>` past it, so no state is ever swallowed. At or below the threshold the output is byte-identical to before — this is a better failure mode for large fleets, not a redesign of the common case. The glyph cap is retired as redundant (a run is now at most a number plus one glyph), but the reason it existed is not: `.tmux.conf` re-expands `@ai_status` through `#{E:...}`, so the length of the value must not be upstream's to choose. Digits are not self-bounding, so `MAX_COUNT = 999` inherits that job and a count of `10**400` renders `999+▶`.
- **The seen/done bit.** A finished-but-unseen agent gets its own state, `done`, with a teal `✓`. It outranks `working` in the urgency order because state urgency ranks *what wants you*, not *what is busy*: a working agent needs nothing from you, a finished one is waiting. `done` is cleared by focusing the pane — it is an unread marker, not a result.
- **State filters as an axis, not a search.** `prefix+a` binds `ctrl-a/b/w/d/x` to all/blocked/working/done/crashed, filtering rows rather than typing into the query, plus a `◆` current-window marker and an event-history preview pane.
- **The boundary that held.** We took the state vocabulary and the display grammar. We still refused the permanent sidebar, live refresh inside the modal (it renders a snapshot; reopen it if you want fresher), and the row-token config DSL. Port the idea, keep the substrate — and keep the config a tmux config.
- **The modal-vs-pane trade decides everything else.** herdr pays a permanent column of screen for information you need a few times an hour. `prefix+a` pays a keystroke on demand instead. Everything above follows from that: the status segment must compress to a glyph run because it is always visible, and the picker can afford preview panes and filter keys because it is not.

**Confirmed 2026-08-10 by running it, not reasoning about it.** The 2026-05 decision was made from the outside, which is a weak way to reject a tool. So herdr got real dotfiles ([`herdr/`](../herdr)), the tmux keymap ported onto it, and a day of actual use. Outcome: back on tmux. The entry stands, now on evidence.

- **What the eval cost, and why it was still cheap.** One package, one backend seam in `tms`, and no change to tmux. `tms` now runs on either substrate from one file and one `tms.toml`, so the switch back was a shell away rather than a revert. Keeping herdr installed and configured means re-testing later costs a `prefix+b`, not another day.
- **The stated objection was right but for the wrong reason.** "Permanent column tax" was the 2026-05 argument. Measured, the compact rail is 4 columns — 1.5% of width on this display, 2.8% of cells including the tab row. That is not what grates. What grates is that it is *fixed and left-edge*: herdr exposes `sidebar_width`/`min`/`max`, `sidebar_start_collapsed` and `sidebar_collapsed_mode`, and no position setting at all (`sidebar_position`, `_placement`, `_side`, `_orientation`, `_location` are all unknown keys; `tab_bar_position` moves the tab row, a different thing). A horizontal strip is not on offer. tmux's pills share a row that was already being spent — the status bar is overhead you have already paid, so the marginal cost of another segment is zero. A column is a new bill.
- **The features that justify a sidebar are fleet features.** Priority-sorted agent panel, cross-workspace blocked detection, `herdr agent prompt --wait`. At one or two agents they show you what you already know, which is why herdr felt like it was charging rent for nothing. The honest test is 4–6 agents across 3+ workspaces — and that is the same experiment that decides `mu`-on-tmux versus the herdr agent CLI, since `mu` spawns tmux panes and `allow_nested = false`. Not run yet. Until it is, "unclear what it buys" is an accurate read rather than a premature one.
- **What the eval bought anyway.** Two things that outlive it. `tms` grew a backend seam and got its first coverage of the agent-glyph path — which immediately caught a real bug (row content depended on which multiplexer the process was launched from). And the sidebar objection is now a measurement instead of an intuition, which is what makes it re-testable.
- **The provenance objection, added as pillar #11.** The screen-real-estate complaint is the one that got noticed first, but it is the smaller one. herdr is agent-era infrastructure *for* agents: v0.x, one maintainer, AGPL, pre-1.0 protocol bumps that stop old clients attaching to a new server, and a release cadence fast enough that this eval spanned a version. That is the opposite end of the axis from tmux (17 years, many maintainers, distro-packaged, exhaustively tested) and from everything else this repo stands on. Pillar #11 now names it directly: prefer infra a human designed, owns, and tests. Note the asymmetry that makes this consistent rather than hypocritical — this repo hands agents plenty of *work* (see `mu`, the skills tree, most of the scripts in these packages), all of it reviewed, tested, and reverted in one command. The substrate under it is where the standard is different, because a bad line in a script is a bug and a bad line in the multiplexer is a rebuild. **Scope, honestly:** this is a judgement about maturity and accountability — age, maintainer count, test surface, protocol stability — not a forensic claim about how any particular line of herdr was written, which is not something to assert without evidence.

**Reconsider if:** a real fleet (4–6 agents, 3+ workspaces) makes the agent panel pay for itself, OR herdr gains a movable/horizontal panel, OR `mu` stops being the daily driver for orchestration. Any of those has to clear pillar #11 as well: a 1.0 with a real test suite and more than one maintainer is a different proposition from what was evaluated here. The package stays in the repo so re-testing is a config edit, not a rebuild.

---

### Pane-controlled values never reach a tmux format shell line (accepted 2026-08-09)

tmux expands `#{...}` *into* the `#(...)` command line and hands the finished string to `sh -c`. Any pane-writable value interpolated there is therefore executable, and there is no argv boundary to protect — so quoting is not a fix. `#{q:}` is a half-fix at best: it escapes shell metacharacters but not the newline separator, and a payload was executed through it via a directory name containing a newline.

This was live: `#{pane_title}` was interpolated into the window-label command. Any program in any pane sets its title with a bare OSC 2 escape — no tmux access needed — and the payload ran on every status redraw. `#{window_name}` and `#{pane_current_path}` had the same shape.

**The rule:** a `#()` command line may only carry values this config generates — ids (`#{pane_id}`, `#{window_id}`), integers, flags. Anything a pane can write is fetched by the script itself, over a pipe, as data. `#{T:}`/`#{E:}` re-expansion of a script's *output* is safe by contrast: neither re-expands `#()`.

**Reconsider if:** never for the rule itself. Re-audit whenever a new `#()` call site is added — the audit that found this also found a second hole (`prefix+T` passing `#{pane_current_path}` to `run-shell`) that the original report had missed.

---

*(Promote entries from `README.md § Zen Of This Setup` or `AGENTS.md` to here when they need longer-form than a pillar bullet.)*
