# Decisions

Past decisions about this setup — both rejected alternatives and non-obvious "yes" choices. Read before relitigating.

Each entry is dated and structured the same way: what was audited, what was found, which Zen pillars it helped or violated, the conclusion, and what would justify revisiting.

Sources of truth:

- Pillars: [`README.md` § Zen Of This Setup](../README.md#zen-of-this-setup).
- Daily editing rules: [`CLAUDE.md`](../CLAUDE.md) (also symlinked as `AGENTS.md`).

---

## Rejected

### Arch and Ubuntu toolbox bootstrap scripts (removed 2026-05-01)

`fedora/setup-toolbox-arch.sh` and `fedora/setup-toolbox-ubuntu.sh` were added in the initial commit and never invoked again. `git log` showed zero touches after `bcc3533`. Audited during a pillar pass and removed.

- **Pillar costs of keeping them:** failed pillar #8 (opinionated, not agnostic — "Linux is Fedora + Wayland + Sway"; supporting Arch and Ubuntu toolbox bootstrap leaks a wider stance than the repo actually wants), and pillar #4 (each piece earns its place — zero recorded use across the lifetime of the repo).
- `setup-toolbox-arch.sh` also tripped the AGENTS.md "shell vs Python" smell: 43 lines that did `getent passwd | awk` parsing for a non-root user, sudo-as-other-user, manual `git clone` + `chown` of yay. The right response to a script with that much non-trivial logic is either Python or deletion; deletion was cheaper.
- The Fedora toolbox script (`setup-toolbox.sh`) stays. It is the only one that matches the stated stack and is actually used.

**Conclusion:** delete both. If a future need arises for a one-off Arch or Ubuntu toolbox, installing `git`, `ripgrep`, `stow`, `tmux`, `zsh` by hand is shorter than maintaining a script for it.

**Reconsider only if:** a daily-driver workflow develops that genuinely needs an Arch or Ubuntu toolbox, *and* that workflow is exercised more than once a quarter.

---

### Ghostty (replaced by Alacritty 2026-04-30)

Ran [Ghostty](https://ghostty.org/) as the daily-driver macOS terminal for ~a year. Switched to alacritty after a Ghostty memory leak grew the process to ~160 GB resident, OOM-killing the machine on 2026-04-30 and forcing a hard power reset — which also nuked years of `~/.zsh_history` via the `share_history` truncate-and-rewrite race (separate fix landed in `zsh/.zshrc` to snapshot the history file daily). The OOM was the trigger but the merit case stands on its own:

- **Stability first.** Across ~a year of daily use, Ghostty leaked memory intermittently. The crash that triggered the swap grew the process to ~160 GB resident before the OS gave up — not a recoverable fault, a hard power reset, which is exactly the worst time to be running a terminal that's holding the only live copy of your shell history. Alacritty has been boring-stable on the same hardware.
- **Real IPC for tooling.** `alacritty msg create-window` (over a per-process Unix socket) is the right primitive for "spawn a new terminal on the current Space without activating the existing one." Ghostty's CLI grew that ability later and via the macOS app bundle; it's less mature and less scriptable. The Hammerspoon launcher (`Hyper+T`, `Hyper+PadEnter`) is materially cleaner with alacritty's IPC than the synthetic `Cmd+N` trick we'd been using for Ghostty (which always swooshed to whichever Space already held a Ghostty window — pillar #8). Hammerspoon now keeps a Ghostty fallback for mixed Macs, but that compatibility path does not change the preference order.
- **Per-OS config without a custom layer.** Alacritty's `[general] import = ["~/.config/alacritty/os.toml"]` plus the dotfiles-sync stow-scope mechanism (one package per OS, only one ever stowed) lets the base config stay shared while OS overlays land at a stable path. No env-var hacks, no shell-driven generation. Mirrors the `fedora/` nested-package pattern.
- **What Ghostty does better:** simpler config (flat `key = value`, no TOML import precedence trap — we tripped on this with `opacity` during migration), built-in tabs/splits, single-process model that reuses memory across windows. None are load-bearing: tmux owns tabs/splits already, and the single-process model is exactly the property that made the OOM eat *all* shell sessions at once.
- **Both are equally hostile to macOS Spaces.** Both need IPC (or an equivalent escape hatch) to spawn a window on the current Space. The fix shape is identical; alacritty's IPC was just easier to reach for first.

**Pillar costs of staying on Ghostty:** failed pillar #1 (boring infra — a terminal that occasionally consumes 160 GB of RAM and forces a hard reset isn't boring), pillar #4 (each piece earns its place — Ghostty stopped earning it the moment it ate the history file), and pillar #8 (lean into native primitives — the synthetic-`Cmd+N` launcher hack against macOS app activation was already a smell).

**Conclusion:** Alacritty is the more boring, more stable, more scriptable terminal on macOS. The OOM was the trigger, but the merit case (IPC, per-OS config layering, no single-process blast radius) would justify the swap on its own now that the migration is done. Ghostty may stay installed and stowed as a compatibility fallback on Macs that need it; it is not the preferred target for new terminal workflow.

**Reconsider only if:** Ghostty ships a stable release with no recurring memory leaks on macOS for ≥3 months, *and* it gains an IPC story comparable to `alacritty msg create-window`, *and* it offers something Alacritty can't (e.g. crash-surviving session restoration, or shared-state cross-window features that materially change the workflow). The first two would just clear the maturity bar; the third is what would move the needle.

---

### `Hyper+G` / `Hyper+1..5` as desktop back-and-forth in Hammerspoon (rejected 2026-04-29)

Recurring temptation: make Hammerspoon mirror sway's `$mod+g` (`workspace back_and_forth`) and direct workspace-number binds. Audited again after the Hammerspoon hotkey trim.

- **What was tried, round 1:** synthetic `Cmd+Tab` behind `Hyper+G` as an approximation of "go back to the previous thing."
- **What happened, round 1:** unreliable to non-functional. macOS treats `Cmd+Tab` as a privileged system-level app switcher, and synthetic key events from Hammerspoon do not behave like a real held `Cmd+Tab` gesture. In practice it either failed outright or acted unlike the desired muscle memory.
- **Why that analogy was wrong anyway:** sway's `$mod+g` is not an app switcher UI — it is `workspace back_and_forth`, i.e. a direct toggle to the last workspace. `Cmd+Tab` is a different interaction model entirely.
- **What was tried, round 2:** a real previous-desktop toggle. First via `hs.spaces.gotoSpace(previousSpaceId)`, then via tracked desktop numbers with synthetic `Ctrl+1..5` so `Hyper+G` and `Hyper+1..5` used the same Mission Control shortcuts that already worked directly.
- **What happened, round 2:** semantically closer, but still too unpredictable behind the Hyper layer. `hs.spaces.gotoSpace` uses Mission Control accessibility plumbing and visibly zooms the desktop out. The synthetic `Ctrl+1..5` path was also unreliable when wrapped in Hyper, even though plain `Ctrl+1..5` worked directly. The result was an interaction that was slower, jankier, and less trustworthy than just using macOS's own bindings.
- **Pillar costs:** violates pillar #1 (boring infra — fighting Mission Control and reserved shortcuts is not boring), pillar #3 (every line is understood — flaky synthetic event wrappers are the opposite), and pillar #4 (each piece earns its place — the binds did not earn their keep).

**Conclusion:** don't bind `Hyper+G` as a desktop back-and-forth toggle, and don't wrap desktop `1..5` switching in Hyper. Hammerspoon is not a good place to fake reliable macOS desktop switching semantics. Keep desktop switching out of the Hyper layer; use the native macOS shortcuts directly when needed.

**Reconsider only if:** Hammerspoon grows a robust native wrapper around previous-desktop behavior, or a future implementation proves reliable enough in real use to feel native rather than like a flaky Mission Control hack.

---

### Vendoring tmux plugins instead of using TPM (rejected, ongoing)

Recurring temptation: tmux config currently loads 5 third-party plugins via TPM (`tmux-yank`, `tmux-cpu`, `tmux-fingers`, `tmux-fzf`, `vim-tmux-navigator`). "Pillar #5 says local scripts over upstream plugins — should I vendor or rewrite these?" Audited each.

- **`vim-tmux-navigator`:** ~30 lines of tmux config that detects vim and forwards `C-h/j/k/l` accordingly. Rewriting locally is feasible but the upstream version handles edge cases (fzf panes, ssh sessions) I'd re-discover the hard way.
- **`tmux-fingers`:** hint-overlay copy-mode (think vimium for tmux panes). Implementation is hundreds of lines of Crystal + shell. Not feasible to rewrite, not worth vendoring (would just freeze upstream and miss bug fixes). *(See "Switch to tmux-fingers-rs" below — this changed.)*
- **`tmux-fzf`:** session/window/pane pickers via fzf. Used primarily for the clipboard-history popup (`prefix + v`). Rest of fzf integration is bespoke (`tms`, `cheatsheet`, `agent-attention`). Could drop it if I rewrote the clipboard picker; haven't yet because it's ~5 lines of bind + works.
- **`tmux-yank`:** clipboard integration that handles Wayland/macOS/Linux differences. Replicating the platform-detection + `wl-copy`/`pbcopy`/`xclip` fallback chain locally is exactly the kind of cross-platform busywork pillar #8 says to avoid in shared config.
- **`tmux-cpu`:** status-line CPU% formatter. *Could* be replaced by a `status-cpu` script alongside the existing `status-ram` / `status-uptime` Python helpers. **Kept (re-confirmed 2026-05-01):** TPM is already in active use for the other plugins on this list, so the marginal cost of one more plugin is zero, and rewriting locally would add code without removing the upstream-plugin dependency anyway. Pillar #5 fires when a *new* plugin is being added, not when a well-scoped existing plugin could in theory be replaced. The output format and color thresholds being already tuned is a tiebreaker, not the load-bearing reason.
- **TPM itself:** ~200 lines of bash that clones, updates, and sources plugins. Boring infra (pillar #1), well-understood (pillar #3 — it's three commands: install, update, source), not blocking anything.

**Pillar tension:** pillar #5 (local scripts over upstream plugins) vs pillar #2 (builtins first — TPM *is* the builtin idiom for tmux plugins) vs pillar #4 (each piece earns its place — each plugin above does, individually).

**Conclusion:** Pillar #5 is about not adding *new* third-party plugins when a 30-line local script would do (the way `tms` replaced tmuxp, `agent-attention` replaced any "notify on activity" upstream tool, `status-*` replaced tmux-cpu-style frameworks). It is *not* a mandate to rewrite well-scoped existing plugins that solve cross-platform problems (clipboard) or UI-heavy problems (fingers) just to satisfy a slogan. TPM stays.

**Reconsider individual plugins if:** the upstream goes unmaintained for 12+ months, OR a Wayland-native clipboard solution makes `tmux-yank` unnecessary, OR I find myself working around a plugin's behavior more than I use it.

---

### Centralized `docs/` folder (rejected for package docs, reconsidered 2026-04-27)

Original proposal: collect all 12 per-package READMEs into a `docs/` folder with topic-shaped names (`docs/lock-screen.md`, `docs/wallpaper.md`, etc.) and internal wiki-style links. Debated, rejected.

- **Argued for:** discoverability (one folder to browse), nicer cross-references, topic-shaped (some topics like lock-screen span 3 stow packages), better surface for an index.
- **Argued against:**
- **Co-location is the killer feature.** Editing `tmux/.tmux.conf` puts `tmux/README.md` in your face in `ls`, `eza`, fzf file pickers. Move it to `docs/tmux.md` and the docs go stale because the editor isn't visually nudging you. Pillar #10 (config lives next to the thing it configures) is exactly this principle.
- **Stow packages are the natural unit.** If `sway/` ever gets deleted (e.g. switching back to a different compositor), its docs go with it cleanly. With centralized docs you have to remember to also delete `docs/sway.md`, `docs/lock-screen.md`, `docs/wallpaper.md`...
- **The root `README.md` is already the hub.** If discovery hurts, fix the index in root README, don't rebuild the layout.
- **No external audience.** This isn't a public project where strangers browse `docs/`. Audience is future-me, who is already in the package directory because that's where the edit is happening.
- **Pillar costs:** a `docs/` folder is a new structure to maintain (pillar #2 — boring infra; the existing layout is boring and works), with its own internal navigation discipline that drifts from code over time.
- **The narrow real pain** — root-level repo docs and control-plane files were starting to blur together. That is a different problem from moving package docs.

**Conclusion:** `find . -name README.md` already works. Per-package READMEs stay where they are. What changed on reconsideration is only the handling of *cross-cutting repo docs*: those now live under `docs/` (`SETUP.md`, `DECISIONS.md`, `THEME.md`, `VSCODE.md`) so the root can more clearly read as packages + contracts + control plane.

**Reconsider only if:** I start wanting to move package-specific READMEs into `docs/` too. Today the answer is still no — co-location wins for package docs.

---

### oh-my-zsh (removed 2026-04-25)

Used OMZ as a zsh framework for years. Audited what it was actually doing for me before keeping it.

- **What OMZ was providing:** ~3 plugins loaded (`git`, `colored-man-pages`, fzf integration), `lib/directories.zsh` aliases (`..`, `...`, `cd -`), case-insensitive completion menu, history options, prompt theme. All small, native zsh primitives underneath.
- **What OMZ was costing:** ~60ms of startup time (160ms → 100ms after removal), an `FPATH` leak workaround in `.zshrc` to undo OMZ's `fpath` mutation, hidden keymaps ("why does `^P` do that? oh, OMZ rebound it"), and a framework dependency for ~30 lines worth of actual config.
- **Pillar costs:** violates pillar #2 (framework around builtins), pillar #3 (every line is understood — OMZ has thousands of lines I never read), pillar #5 (local scripts over upstream plugins), and was the canonical example of pillar #4 failure (it earned its place by inertia, not by audit).
- **Replacement:** `zsh/.zshrc` rewritten to ~110 lines of native zsh: `compinit` with case-insensitive matcher, `setopt`s for history/dirs/glob, `bindkey -e` plus the prefix-history-search bind, direct sourcing of `zsh-autosuggestions` and `zsh-syntax-highlighting` from `~/.local/share/zsh-plugins/` (cloned by `dotfiles-sync --apply`), `eval "$(fzf --zsh)"` for Ctrl-T/Ctrl-R/Esc-c, `eval "$(zoxide init zsh)"` for `z`. Aliases live in `zsh/.zsh/{git,jj,hg}-aliases.zsh` cherry-picked from OMZ's `git.plugin.zsh` after grepping `~/.zsh_history` for actual usage counts (>=5 invocations earned a keep).

**Conclusion:** A framework is the right answer when you'd write >500 lines of equivalent config without it. ~110 lines of native zsh is well under that threshold; the framework was overhead, not leverage.

**Reconsider only if:** the personal config grows past ~400 lines of zsh, *and* a single framework can replace 3+ ad-hoc subsystems at once (not just "OMZ has plugins for git" — OMZ's plugins were thin sugar over native commands).

---

### chezmoi (rejected 2026-04-25)

Audited [chezmoi](https://www.chezmoi.io/) v2.70.2 against `dotfiles-sync`'s actual responsibilities. Findings:

- chezmoi cleanly replaces the boring half of `dotfiles-sync` (stow driver + conflict UX + OS scoping). `chezmoi diff` / `chezmoi apply --dry-run` are nicer than the custom `--check` flow.
- chezmoi does **not** solve the harder half:
- Per-skill symlinks into `~/.agents/skills/` don't fit the static-target-state model. Either you duplicate symlink declarations or you drop into `run_onchange_*.sh` scripts — same Python code, just relocated under a different roof.
- Files mutated by other tools (`~/.claude/settings.json`, `~/.codex/config.toml`) don't fit chezmoi's "chezmoi owns the target" model.
- **Pillar costs:** violates pillar #2 (chezmoi is a heavier abstraction than stow + Python), pillar #3 (filename munging like `dot_`, `private_`, `executable_`, `symlink_`, `run_onchange_` plus Go template syntax = more concepts to hold in head, not fewer), pillar #10 (source tree no longer mirrors `$HOME` shape; today `nvim/.config/nvim/init.lua` is its own destination, under chezmoi it becomes `dot_config/nvim/init.lua` and the repo loses its self-documenting layout).
- **Features not used today:** templating (no per-machine variation in `git/.gitconfig` etc.), encryption (secrets are intentionally out-of-repo per `CLAUDE.md`), the externals fetcher. chezmoi's most valuable features would sit unused on day one.

**Conclusion:** chezmoi solves the *easy* half of the problem and ignores the *hard* half. The right response to "`dotfiles-sync` is sprawling" was to delete the agent-orchestration code in favor of upstream tools and `~/.agents/skills/` symlinks (already done), not to swap the underlying tool.

**Reconsider only if:** chezmoi gains a real per-skill fan-out primitive (one source file → N symlink destinations), *or* you start needing per-machine templated configs and in-repo secrets at scale.

---

### niri (replaced by sway 2026-04-23)

Ran [niri](https://github.com/YaLTeR/niri) as the daily-driver Wayland compositor for ~6 months. Switched to sway. Commit message at the time:

> migrate desktop from niri to sway. I'm a grown up now.
> Stability over visual flare.

The quote is real but does niri a disservice. Sway didn't only win on maturity — it wins on the merits too:

- **Manual control of the layout tree is the actual power move.** Sway exposes the layout as a directly manipulable tree: `splitv`/`splith` to shape containers, `focus parent/child` to move up and down it, marks to address windows by name, `move container to mark` to restructure on the fly, scratchpad to stash subtrees. You're not picking from a menu of preset layouts — you're *building* the layout by hand and the tree remembers it. Niri's scrollable model deliberately hides the structure (it's a sequence of columns, auto-arranged), which demos beautifully on a single ultrawide but stops being a feature the moment you want "this window pinned next to that one across workspace switches." Manual tree wins because the tree is the thing you're actually trying to express. (See [Sway School](https://martintrojer.github.io/sway-school/) for why the tree model is worth learning.)
- **i3 IPC is the standard, not just the older option.** Every Wayland status bar, idle daemon, lock-screen wrapper, screenshot tool, and tiling-WM script assumes the i3 IPC contract. waybar, swayidle, swaylock, kanshi, mako, fuzzel — the entire ecosystem composes around it. niri implementing its own IPC isn't a feature, it's tax that the user pays in glue scripts.
- **Smaller, frozen surface area is a positive property.** Sway hit feature-complete years ago and now just gets bug fixes. That means the config schema, IPC vocabulary, and binding model are stable and learnable in a weekend. niri is still adding fundamental concepts; every upgrade was a small archaeology project on bindings and config syntax.
- **What niri does better:** out-of-the-box animations and visual polish. That's it. Both genuinely nice; neither load-bearing for actual work.

**Pillar costs of staying on niri:** failed pillar #1 (boring infra — niri is the opposite of boring), pillar #5 (the niri ecosystem required more local scripts to fill gaps that sway covers natively via i3 IPC), and pillar #2 (builtins first — sway's tree primitives *are* the builtins for this kind of work; reaching for scrollable-tiling was reaching for a more exotic abstraction).

**Conclusion:** Sway is the better tool *and* the more boring tool. The merit case alone would have justified the switch; the stability case is the bonus.

**Reconsider only if:** niri ships a stable 1.0 with a frozen IPC/config schema, the i3-IPC ecosystem gains first-class niri support, *and* the scrollable-tiling primitive gains an addressable-layout escape hatch comparable to sway's tree. Today only the third would change my mind on the merits; the first two are necessary just to clear the maturity bar.

---

### SwayFX (tried and reverted 2026-04-28)

Swapped upstream sway for [SwayFX](https://github.com/WillPower3309/swayfx) for an afternoon to evaluate the visual-effects tax/benefit ratio. SwayFX is a sway fork that adds GLES2 effects (rounded corners, blur, shadows, inactive-window dim) on top of an otherwise config-compatible sway. Set up the full stack: `corner_radius 8`, 2-pass blur with `blur_ignore_transparent` on the waybar/mako/fuzzel/swaylock layer-shell surfaces, soft Catppuccin-base shadows, 10% inactive dim, plus the matching alpha tweaks in `mako/config`, `fuzzel.ini`, and `alacritty.toml` so the layer blur actually had something to show through. Reverted same day.

- **What was nice:** rounded corners genuinely changed how the desktop *feels* — the only effect that registered as a real upgrade. Glass blur on fuzzel and mako looked good in screenshots.
- **What didn't earn its keep:**
  - Most of my screen-time is `alacritty` + `nvim` + `tmux`. None of them benefit from layer-shell effects (alacritty is a regular toplevel, the others render inside it). The aesthetic surface where blur/shadows actually show is small.
  - Fuzzel and mako are visible for ~2 seconds at a time. Paying a permanent fork tax for sub-second eye candy is a bad trade.
  - Tabbed-layout corners are broken upstream ([WillPower3309/swayfx#405](https://github.com/WillPower3309/swayfx/issues/405), [#400](https://github.com/WillPower3309/swayfx/issues/400)) — the outer container rounds, individual tab children don't. Not fixable from config.
  - Configuration friction in one short session: layer-shell namespaces had to be probed by hand with `swaymsg -t get_outputs | jq`, mako alpha needed retuning twice (0.80 too solid through 2 blur passes, settled near 0.55), `outer-margin` had to shrink because it doubles as the layer-surface size and the blur halo grew with it.
  - The two effects that would actually justify a fork — fade animations and window-movement animations — are still on the SwayFX roadmap, not shipped. Paying the fork tax without the headline features.
- **Pillar costs:** violates pillar #1 (boring infra — a fork that lags upstream sway by weeks-to-months for bugfixes/features), pillar #2 (builtins first — upstream sway is the builtin), pillar #4 (each piece earns its place — corners alone don't justify the surface area), and adds a third-party copr/Terra repo on top of an atomic Fedora Sericea base where layering is intentionally minimised.
- **Upstream stance is stable, not racing SwayFX:** upstream sway has explicitly and repeatedly declined to add effects ([swaywm/sway#3380](https://github.com/swaywm/sway/issues/3380), [#3583](https://github.com/swaywm/sway/issues/3583), [#3652](https://github.com/swaywm/sway/issues/3652), [#5998](https://github.com/swaywm/sway/issues/5998), [#7107](https://github.com/swaywm/sway/issues/7107) — all closed with "feel free to fork"). This is exactly why SwayFX exists. So SwayFX is *not* at risk of being obsoleted by upstream; if anything, it's the only effects-on-sway-IPC path and will remain so. That removes one objection (no "upstream will catch up" hope) but doesn't rescue the cost/benefit case below.

**Conclusion:** the corner rounding is the only effect that meaningfully changed the feel of the desktop, and one effect doesn't clear the bar for adopting a fork. Stability over visual flare — same conclusion as the niri migration above, arrived at faster because the cost/benefit was more lopsided this time.

**Reconsider only if:** SwayFX ships fade and window-movement animations *and* the tabbed-corner family of bugs is fixed. Rounded corners alone are not enough. (Don't wait for upstream sway to grow effects — the maintainers have made it clear that won't happen.)

#### If you do retry: install + config recipe

Kept here (rather than in `fedora/README.md` or as live config) so this file stays the single source of truth for the SwayFX decision. If you ever flip the call, copy the snippets out; don't re-derive them.

**Install on Sericea (atomic Fedora).** SwayFX `Provides:`/`Conflicts:` `sway`, so plain `rpm-ostree install swayfx` fails with:

```
error: Could not depsolve transaction; 1 problem detected:
 Problem: package swayfx-… conflicts with sway provided by sway-… from @System
```

The fix is to swap the base package and layer the replacement in **one** transaction — doing override-remove and install separately doesn't work, depsolve still sees `sway` provided by the other side.

```bash
# 1. Enable the SwayFX COPR (rpm-ostree doesn't grok `dnf copr enable`,
#    so drop the .repo file in directly).
sudo wget -O /etc/yum.repos.d/_copr_swayfx-swayfx.repo \
  "https://copr.fedorainfracloud.org/coprs/swayfx/swayfx/repo/fedora-$(rpm -E %fedora)/swayfx-swayfx-fedora-$(rpm -E %fedora).repo"

# 2. Remove base sway and install swayfx in ONE transaction.
sudo rpm-ostree override remove sway --install swayfx

# 3. Reboot into the new deployment.
systemctl reboot
```

**Rollback.** Fastest path — boot back to the previous deployment (still has stock sway):

```bash
sudo rpm-ostree rollback
systemctl reboot
```

Or undo the override on the current deployment without losing other layered changes:

```bash
sudo rpm-ostree uninstall swayfx
sudo rpm-ostree override reset sway
sudo rm /etc/yum.repos.d/_copr_swayfx-swayfx.repo  # optional
systemctl reboot
```

Check state with `rpm-ostree status` — overrides show under `RemovedBasePackages:`, layered packages under `LayeredPackages:`.

**Config layering that worked.** Five files touched. Sway already auto-includes nothing under `~/.config/sway/config.d/`, so the main config needs an explicit `include` line at the bottom:

```
# sway/.config/sway/config (last line)
include ~/.config/sway/config.d/*
```

Drop all SwayFX-specific directives in `sway/.config/sway/config.d/swayfx.conf`. Vanilla sway will warn-and-continue on unknown directives, so this is safe to keep around even after a revert (though we deleted it). Recipe that worked:

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
blur_xray disable
blur_passes 2               # 2/4 is readable + cheap; bump for more drama
blur_radius 4
blur_noise 0.02             # hides banding on solid colors
blur_brightness 1.0
blur_contrast 1.0
blur_saturation 1.1

default_dim_inactive 0.1
dim_inactive_colors.unfocused #000000FF
dim_inactive_colors.urgent    #f38ba8FF
for_window [title="^Picture in picture$"] dim_inactive 0.0
for_window [title="^Picture-in-Picture$"] dim_inactive 0.0
for_window [app_id="^Cider$"]              dim_inactive 0.0

# Layer-shell effects. Verify namespaces on YOUR machine first:
#   notify-send probe; sleep 0.3
#   swaymsg -r -t get_outputs | jq -r '.[].layer_shell_surfaces[]?.namespace' | sort -u
# On this box: "first" (waybar), "notifications" (mako), "launcher" (fuzzel),
# "lockscreen" (swaylock), "wallpaper". GTK layer-shell apps usually
# advertise "gtk-layer-shell".
layer_effects "first" {            # waybar: bar background is rgba(.., 0.5)
    blur enable
    blur_ignore_transparent enable
    shadows enable
    # No corner_radius: full-width bar would only round at screen edges.
}
layer_effects "notifications" {     # mako
    blur enable
    blur_ignore_transparent enable
    shadows enable
    corner_radius 8
}
layer_effects "launcher" {          # fuzzel
    blur enable
    blur_ignore_transparent enable
    shadows enable
    corner_radius 8
}
layer_effects "lockscreen" {        # swaylock
    blur enable
    blur_ignore_transparent enable
    shadows enable
}
```

**Companion alpha tweaks.** Layer blur only shows through translucent surfaces. The alpha values that actually read as "glass" through 2 blur passes (the dark Catppuccin base dominates fast):

| File | Field | Value | Notes |
|---|---|---|---|
| `fuzzel/.config/fuzzel/fuzzel.ini` | `[colors] background` | `1e1e2ecc` (~80%) | `ff` = fully opaque, no blur visible. |
| `fuzzel/.config/fuzzel/fuzzel.ini` | `[border] radius` | `8` | Match `corner_radius` for visual consistency (set to `0` when reverting). |
| `mako/.config/mako/config` | `background-color` (default + `[urgency=normal]`) | `#1e1e2e8c` (~55%) | Mako does **not** accept CSS `rgba()` — only `#RRGGBBAA` hex. |
| `mako/.config/mako/config` | `background-color` (`[urgency=low]`) | `#1e1e2e80` (~50%) | |
| `mako/.config/mako/config` | `background-color` (`[urgency=critical]`) | `#1e1e2ed9` (~85%) | Stay mostly opaque so urgent reads instantly. |
| `mako/.config/mako/config` | `outer-margin` | `6` (was `20`) | Doubles as the layer-shell surface size; large value → large blur halo around each notification. Keep small (4-8) when SwayFX blur is on. |
| Terminal opacity (Linux) | `alacritty/alacritty-linux/.config/alacritty/os.toml` | `[window] opacity` | `0.9` | Already this value pre-trial; just confirm. Lower than ~0.85 hurts readability. |
| `waybar/.config/waybar/style.css` | (none) | — | Bar bg is already `rgba(30,30,46,0.5)`; no change needed. |

**Known broken:** tabbed-layout corners ([WillPower3309/swayfx#405](https://github.com/WillPower3309/swayfx/issues/405), [#400](https://github.com/WillPower3309/swayfx/issues/400)) — outer container rounds, individual tab children don't. No config workaround.

#### Lessons learned (the gotchas that cost time)

In rough order of how badly each one bit during setup. Future-you will hit at least three of these again. Read first, debug second.

1. **Layer-shell namespaces are app-defined and undocumented — you must probe.** The `layer_effects "<name>"` blocks only fire when `<name>` matches what the app advertises. Discover with:

   ```bash
   # Trigger surfaces to exist FIRST. Mako has no live surface until something
   # is showing; an empty notification list means an empty namespace list.
   notify-send probe; sleep 0.3
   swaymsg -r -t get_outputs | jq -r '.[].layer_shell_surfaces[]?.namespace' | sort -u
   ```

   On this box that printed `first wallpaper notifications` (waybar advertises as `"first"` because of the `name` field in `waybar/config.jsonc`, not `"waybar"` — the obvious guess fails silently). GTK-based layer-shell apps usually advertise `"gtk-layer-shell"`. Wrong namespace = no error, no warning, just no effect. **First debugging step when an effect doesn't show: re-probe the namespace.**

2. **Mako rejects CSS `rgba()`. It only takes `#RRGGBBAA` hex.** Fuzzel and waybar accept CSS-style colors; mako does not. Pasting `rgba(30, 30, 46, 0.80)` into `mako/config` makes the daemon fail to parse and exit — silently from the user's POV (no notifications appear, but nothing pops up to tell you mako is dead). **Always check `journalctl --user -u mako -n 20` after a mako config change**; it'll show `[config:N] Failed to parse option ...` immediately. Hex alpha cheat sheet: `40` ≈ 25%, `80` ≈ 50%, `8c` ≈ 55%, `bf` ≈ 75%, `cc` ≈ 80%, `d9` ≈ 85%, `f2` ≈ 95%, `ff` = 100%.

3. **80% alpha looks fully opaque through 2 blur passes.** First instinct was "0.80 is plenty translucent." It is not. The dark Catppuccin base (#1e1e2e) plus the blur's color-averaging makes anything above ~0.65 read as solid. Calibration that worked: ~0.50–0.55 for normal-priority surfaces (fuzzel, mako-normal), ~0.85+ for surfaces that need to read instantly (mako-critical). To check whether you have an alpha problem vs. a layer-namespace problem vs. a renderer problem, **temporarily drop alpha to ~0.25 (`#1e1e2e40`)**: if it goes obviously glassy, you just had alpha too high; if it stays solid, the blur isn't reaching the surface (wrong namespace, missing `layer_effects`, or the daemon is painting over it).

4. **Mako's `outer-margin` doubles as the layer-shell surface size.** The default `outer-margin=20` produces a 20px-wide "glass halo" around each notification because SwayFX paints blur across the full layer surface, not just the visible notification rectangle. Looks ridiculous. Drop to `outer-margin=6` (or smaller) when blur is enabled. This is invisible in vanilla sway because there's no halo to draw, but with effects on it's the most obvious visual artifact in the whole setup.

5. **Fuzzel's default `background=1e1e2eff` is fully opaque — same trap as mako.** No blur shows through `ff`. Drop to `cc` or lower. Same family of problem; remember that *every* layer-shell app's background needs alpha audited, not just mako.

6. **Alacritty isn't a layer-shell surface.** `layer_effects` does nothing for it. The *global* `blur enable` directive does cover regular toplevels, though, and any window with `[window] opacity < 1.0` automatically gets the blurred-desktop treatment. No alacritty-specific config needed on the sway side; just keep opacity in the 0.85–0.95 range (lower hurts readability for long terminal sessions).

7. **`blur_ignore_transparent enable` is required on every layer block.** Without it, layer blur composites against the full surface as if it were opaque, and the translucent regions don't show the blurred content underneath. Set it on every `layer_effects` block, no exceptions.

8. **`shadows_on_csd disable` or GTK apps double-shadow.** Client-side-decorated apps (most GTK/libadwaita) draw their own shadows; SwayFX adds another on top. Looks awful. Disable `shadows_on_csd`; xdg-toplevel apps without CSD still get the SwayFX shadow.

9. **Pure-black shadows clash with Catppuccin.** `#000000aa` reads as a hard cutout against the lavender/mauve palette. Use the base color at moderate alpha instead: `shadow_color #1e1e2eaa` reads as soft darkening, not a black halo. Same logic for `shadow_inactive_color`.

10. **Live-reload paths per app** — use these instead of restarting the session:
    - sway: `swaymsg reload`
    - mako: `systemctl --user restart mako` (then `systemctl --user is-active mako` + `journalctl --user -u mako -n 5` to confirm)
    - waybar: `pkill -SIGUSR2 waybar` (style-only) or restart for config.jsonc changes
    - fuzzel: re-reads on each invocation; just open it again
    - alacritty: `live_config_reload = true` is already set; saves apply to running windows
    - swaylock: re-reads on next lock

11. **Test single layer effects without a config edit.** SwayIPC accepts one effect at a time:

    ```bash
    swaymsg layer_effects '"notifications"' '"blur enable"'
    swaymsg layer_effects '"notifications"' '"corner_radius 12"'
    swaymsg layer_effects '"notifications"' '"reset"'   # clear all effects on layer
    ```

    Iterate values live, then write the winner into `swayfx.conf`. Much faster than reload-and-eyeball.

12. **`workspace_auto_back_and_forth yes` plus an explicit `$mod+g` toggle is one rule too many.** Unrelated to SwayFX, but caught during this session: with both on, mashing `$mod+1` ping-pongs between workspaces 1 and the previous one, which makes "go to workspace 1" non-idempotent. Pick one mechanism. We kept the explicit `$mod+g` (mirrors tmux `prefix+g`) and disabled the auto behavior.

---

## Accepted (non-obvious)

### macOS terminal policy: prefer Alacritty, tolerate Ghostty fallback (accepted 2026-05-14)

The Mac fleet is temporarily mixed: some machines have Alacritty installed, some still have Ghostty. Do **not** turn that into a second equal terminal strategy. Policy:

- **Alacritty remains the preferred macOS terminal.** The 2026-04-30 decision below still stands: boring stability, per-window process isolation, and `alacritty msg create-window` make it the better default for Hammerspoon and tmux-heavy work.
- **Ghostty is a compatibility fallback, not the target.** Keep the small Darwin `ghostty/` config stowed so older / alternate Macs are usable, but do not add new workflow features that only work in Ghostty.
- **Terminal launch binds choose in order:** Hammerspoon's `Hyper+T`, `Hyper+Return`, and `Hyper+PadEnter` look for Alacritty first and fall back to Ghostty when Alacritty is absent. The fallback exists to keep machines usable during transition, not to relitigate the default.
- **Use native launch primitives per app.** Alacritty gets its IPC socket (`alacritty msg --socket ... create-window`) to create windows on the current Space. Ghostty uses `open -na "Ghostty"` / app-local `Cmd+N` as the current practical macOS fallback. Avoid generic synthetic focus tricks that jump Spaces.
- **Shared terminal behavior belongs in tmux/zsh.** Anything inside the shell/editor/multiplexer path should stay terminal-emulator-neutral. App-specific config is limited to rendering, input translation, clipboard, and current-Space window creation.

**Reconsider only if:** every active Mac has converged on one terminal again, or Ghostty clears the stability / IPC bar described below and offers a workflow gain large enough to replace Alacritty rather than merely coexist with it.

---

### Tmux owns in-buffer text ops; alacritty owns only what the terminal layer structurally can (accepted 2026-05-02)

Recurring temptation: alacritty has hints (regex-based, vimium-style picker) and a vi mode (full vi navigation over the viewport). tmux has copy-mode (vi nav over per-pane scrollback) and `tmux-fingers-rs` (hint picker over visible pane content). The two stacks visibly overlap. Audit triggered while adding `option_as_alt = "OnlyLeft"` and noticing the duplication.

**The killer fact, given "always inside tmux":** alacritty's hints and vi mode operate on the alacritty *viewport* — the rendered frame tmux paints into the terminal. They cannot see tmux's per-pane scrollback. So in this workflow:

- alacritty vi mode "scrollback" is mostly the current rendered frame; useless for "what scrolled past 5 minutes ago"
- alacritty hints match only content visible in the current frame, not anything tmux is buffering above the viewport
- alacritty has no way to paste a hint match back into the active tmux pane — best it can do is copy to the system clipboard for a manual paste

Adopting alacritty's stack would be a strictly weaker duplicate of tmux's, with the only upside being "works the same way outside tmux" — irrelevant when the workflow is always inside tmux.

**Where the split naturally lands:**

| Use case | Owner | Why |
|---|---|---|
| Scrollback search / nav / copy / select | tmux copy-mode | sees per-pane history; alacritty cannot |
| Pattern-pick + paste back into pane | `tmux-fingers-rs` (`prefix + Tab`) | one-keystroke `Shift`+hint paste-back; alacritty has no equivalent |
| Click a URL → open in browser | alacritty hints (`Cmd`/`Ctrl`-click) | mouse-aware, knows pixel position; tmux structurally cannot |
| RGB / undercurl rendering | alacritty (terminal duty) | already wired via `terminal-features` |

Not really "two stacks fighting" — each tool owns the operations only it can do. Zero functional overlap once the split is enforced.

**Pillar costs of the rejected "let alacritty do more" path:** failed pillar #4 (each piece earns its place — alacritty hints expanded beyond URL would be a dormant duplicate of tmux-fingers), pillar #2 (tmux copy-mode + tmux-fingers-rs are already the builtins for in-pane text ops), and pillar #8 (lean into native primitives — the discipline is to use each layer for what only it can do, not to make either one "do everything").

**What this committed to in the same session:**

- Added `Ctrl-v` rectangle-toggle and `Alt-Up` / `Alt-Down` shell-prompt jumping (OSC 133) to copy-mode-vi so the in-tmux text workspace is the strictly more capable one. Required `option_as_alt = "OnlyLeft"` on alacritty/macOS so left-Option sends Meta.
- Dropped the unused `@fingers-jump-key BTab` after auditing actual usage. Kept the `Shift`+hint paste-back action — the load-bearing feature alacritty cannot replicate.
- Kept alacritty hints scoped to URLs only. Not growing them to match fingers' patterns is a deliberate ceiling, not an oversight.
- Did not learn alacritty vi mode. Strictly weaker than `prefix + [` copy-mode in this workflow.
- Two non-obvious gotchas surfaced and are now documented in code comments: tmux 3.6a's OSC 133 parser only accepts the BEL terminator (not ST, despite the man page); and a precmd-emitted OSC 133;A marker gets stomped by zsh's prompt-redraw sequences, so the marker is embedded directly in `PROMPT` via `%{..%}` instead.

**Reconsider only if:** the workflow stops being "always inside tmux" (e.g. running raw alacritty for some long-lived purpose), *or* alacritty gains structural access to tmux's per-pane scrollback (would require IPC tmux does not expose), *or* tmux's copy-mode regresses materially relative to alacritty's vi mode.

---

### Keep the per-tool learning guides, authored as Markdown under `guides/` (accepted 2026-05-01, format revised 2026-05-01)

Originally five hand-written `docs/{HAMMERSPOON,NVIM,TMUX,YAZI,ZSH}_LEARNING_GUIDE.html` files. Tool-specific, which on its face violates pillar #10 (config next to the thing it configures). Considered moving them into per-package folders or deleting them outright.

- **Pillar tension:** #10 (config next to thing) versus the failure mode of any "learning notes" content that does not live next to the code it teaches.
- **What they do:** browser-friendly walkthroughs with quizzes for the keyboard-heavy tools where forgetting a binding genuinely costs time. Each per-package README links to its matching guide explicitly, so the co-location pillar is partially honored — the guide is one click away from the package edit.
- **Why kept:** the alternative is either inlining hundreds of lines of guide content into the package READMEs (mixing reference docs with tutorials), or moving the guides into a separate "learning notes" repo where they would predictably go stale. Keeping them in this repo guarantees they get updated whenever the corresponding tool config is touched, and they are exactly the kind of content where staleness is the primary failure mode.

**Format revision (2026-05-01).** The original five files were hand-written HTML with the same Catppuccin CSS block and quiz JS engine inlined into each one (~85% duplicated boilerplate, ~188 KB committed, content authored as one minified line per section). Replaced with `guides/*.md` Markdown sources rendered by a stdlib-only `guides/build.py` to gitignored `guides/build/*.html`. One CSS file, one JS file, one wrapper template — changing the theme is now a one-file edit instead of five. `make build-guides` / `make serve-guides` / `make check-guides` (wired into `check-all`).

- **Why Markdown + tiny Python builder over Hugo / mdBook:** Hugo would add a system dependency (binary on every bootstrap path) for a 5-page site whose features (tabs, quizzes, sticky scoreboard) Hugo doesn't model natively anyway. Python is already required by `_dotfiles_sync` and `make check-python`; the builder is ~250 lines of stdlib code with full type hints. Pillars #2 (builtins first), #3 (every line understood), #5 (local scripts over upstream plugins) all point the same way.
- **Why a Markdown subset rather than full CommonMark:** the format we need is tiny (`#`/`##`/`###` headings, `-` bullets, inline `` `code ``/`**bold**`/`*italic*`/links, ``` ```quiz ``` fenced blocks with TOML body). A constrained format means a constrained renderer (~80 lines), and zero third-party Markdown dependency. If we ever need footnotes, tables, etc., graduating to `markdown-it-py` is a 10-minute change with no source-file edits.
- **Why `guides/` rather than `docs/`:** the source is Markdown and gets committed; the rendered HTML is a build artifact and lives in gitignored `guides/build/`. Keeping the source next to its build target (rather than dumping it in `docs/` which is for hand-curated long-form content like this file) honors pillar #10 once more. `guides/` does not appear in `_dotfiles_sync/inventory.py`'s `PACKAGE_GROUPS`, so it is invisible to stow.

**Pillar fit:** #10 honored twice over (each guide one click from its package README; build artifact next to source). Plus #2, #3, #5 above. Net positive.

**Reconsider only if:** any individual guide stops getting opened over a 12-month window (check browser history), in which case delete just that one. Or if the Markdown subset has to grow far enough that swapping in a real Markdown library is cheaper than maintaining the renderer.

---

### Keep `_dotfiles_sync/` split across small modules (accepted 2026-05-01)

Audited the 12-file split inside `_dotfiles_sync/` (1,395 lines total; largest file 291 lines). Considered collapsing to ~8 files by merging `integration_checks.py` into `external.py`, inlining `model.py`, and inlining `pins.py`.

- **Pillar tension:** #3 (every line understood — many small files might be ceremonial) versus everyday readability when navigating any single file.
- **What was true:** the cross-import of private helpers (`_pinned_clone_head`/`_pinned_clone_resolve`) between `external.py` and `integration_checks.py` does smell like one module pretending to be two. `model.py` is a types-only module in the Java style.
- **Why kept anyway:** smaller files win on readability when reading any one of them in isolation, and the directory listing already reads as a mental table of contents (`cli`, `config`, `inventory`, `pins`, `system`, `stow`, `repo_checks`, `integration_checks`, `external`, `model`). Collapsing would save a few cross-imports at the cost of producing 250-line modules with multiple unrelated concerns. The current shape pays off whenever any single concern is being changed in isolation.

**Reconsider only if:** the module count crosses ~20, *or* the cross-imports start forming a real cycle, *or* a single function genuinely needs to span what are today three modules.

---

### Keep the `tuicr/` theme override (re-affirmed 2026-05-01 after a brief mis-deletion)

The `tuicr/` stow package contains a single line: `theme = "catppuccin-mocha"`. During a pillar audit it was briefly classified as cosmetic and deleted, then immediately restored when `<leader>gt` (which opens tuicr as the code review UI inside nvim) rendered as white-on-dark and was unreadable.

- **Why it earns its place:** tuicr's default theme is white, which is readable in a stand-alone light terminal but clashes inside the Catppuccin-Mocha alacritty + nvim stack the rest of this repo commits to (pillar #9, one palette everywhere). Without this override the tool is *functionally unusable* in the workflow that actually invokes it.
- **Lesson for the cosmetic-vs-functional rule on one-file packages:** "theme = ..." looks cosmetic on its face but is functional whenever the default would render the tool unusable in the host context. The right test is not "does this file change colors?" but "would removing this file degrade the workflow that uses the tool?" For `vale/.vale.ini`, the answer is yes (alert-level config). For `tuicr/.config/tuicr/config.toml`, the answer is also yes (legibility inside nvim). Keep both.
- **What would actually fail the rule:** a one-file package containing only an override that produces an *aesthetic* preference between two equally-readable defaults. None of the current one-file packages match that shape.

**Reconsider only if:** tuicr ever ships a dark-respecting default, *or* the rest of the desktop palette changes such that the upstream default becomes legible in context.

---

### Keep the `summarize` naming triplet (accepted 2026-05-01)

The repo contains three things named "summarize": the stow package `summarize/` (CLI config), the agent skill `skills/summarize/`, and the external `summarize` CLI binary itself. Audited as a potential pillar #3 (every line understood) smell because `git log` and `rg summarize` both return mixed results.

- **Why kept as is:** the three are intentionally co-named because they are facets of the same workflow — the skill calls the CLI, which reads the config from the stow package. The naming clash is a feature, not noise; renaming any of them would obscure the relationship.
- The cost is one second of disambiguation when reading `git log` for "summarize." That cost is acceptable and lower than the cost of inventing distinct names for three things that genuinely belong to one workflow.

**Reconsider only if:** a fourth unrelated tool also called "summarize" enters the repo, at which point the existing three would need qualifying prefixes to stay unambiguous.

---

### No unit tests for the control-plane and helper scripts (accepted 2026-05-01)

The only test runner in the repo is `tmux/.config/tmux/scripts/test-status-tools` (driven from `make check-tmux-tests`). The following code has no tests beyond `ruff` and `ty`:

- `_dotfiles_sync/` — 1,395 lines, including symlink fan-out, pinned clone management, stow conflict parsing.
- `fuzzel/.config/fuzzel/scripts/_common.py` — 256 lines of shared script-helper logic backing 10 fuzzel scripts.
- `local-bin/.local/bin/solo` — 120 lines of file-locking with a documented `100` exit-code contract.
- All other `local-bin/`, `sway/.config/sway/scripts/`, `fedora/bin/.local/bin/` Python scripts.

Made explicit during a pillar audit so future-me doesn't relitigate it every time a script grows past 100 lines.

**Why this is the deliberate choice:**

- **Pillar #6 (recreate, don't restore).** The blast radius of any single bug is one shell session or one `--apply` run. Sessions are disposable; an `--apply` regression shows up immediately the next time it runs. Tests do not buy meaningfully more coverage than the next manual invocation does.
- **Pillar #3 (every line understood).** All of these scripts are short enough to audit end-to-end on read. Tests would duplicate that audit in code, not extend it.
- **Pillar #1 (boring).** Adding a unit-test runner (pytest, etc.) and matching CI is non-trivial infrastructure for a personal repo where the current loop is "edit, run, observe."
- **The tmux exception.** `test-status-tools` exists because tmux status helpers run on every status refresh (every few seconds, headless), so silent breakage is invisible until the bar visibly changes. Smoke coverage there has obvious value; for code that runs interactively and prints to a terminal the operator is already looking at, the same value is not present.

**Reconsider individual scripts if:** any of them grows past ~300 lines *and* ships a bug that costs more than 30 minutes to debug. In that case add a focused smoke test for the regression, not a generic suite.

---

### Split the repo control plane into `dotfiles-sync` + `_dotfiles_sync/` (accepted 2026-04-27)

Replaced the sprawling single-file bootstrap script with a tiny root entrypoint (`dotfiles-sync`) and a small repo-local Python package (`_dotfiles_sync/`). At the same time, moved cross-cutting repo docs into `docs/` and demoted the tiny `vscode/` pseudo-package to `docs/VSCODE.md`.

**Driver:** the old bootstrap had turned into a merge-conflict magnet and a layout wart. It mixed generic Stow orchestration, repo package inventory, pinned clone management, agent symlink fan-out, and drift checks in one ~1300-line file. The problem was not that Stow was wrong; the problem was that the control plane no longer read like intentional software.

**What changed:**

- `dotfiles-sync` is now the only root command.
- `_dotfiles_sync/` holds the implementation in a small set of focused modules (`cli`, `config`, `system`, `stow`, `checks`, `external`, `model`).
- Cross-cutting repo docs live in `docs/` (`SETUP.md`, `DECISIONS.md`, `THEME.md`, `VSCODE.md`).
- `vscode/` was deleted; the settings were too small to justify a fake top-level package.

**What did *not* change:**

- Top-level stow packages stay top-level and keep their own READMEs.
- `fedora/` stays a special-case namespace (nested packages + setup wrappers).
- `skills/` and `pi/` stay top-level source trees.
- Claude marketplace baggage (`agents/`, `hooks/`, `.claude-plugin/`) stays ugly at repo root because that published layout is an external integration contract, not an accident.

**Why this instead of chasing a more generic tool:** this keeps the boring parts boring (still Stow + Python, still source tree mirrors `$HOME`) while making the implementation legible. It solves the code-layout problem without inventing a framework, manifest DSL, or fake reusable abstraction.

**Pillar fit:** improves pillar #1 (boring infra), #3 (every line is understood), and #10 (the repo root now more clearly reads as packages + contracts + control plane). The only real tax is one more directory (`_dotfiles_sync/`), which was worth it once the single file stopped being mentally local.

**Reconsider only if:** the control plane shrinks back below ~300 lines and the module split starts to feel ceremonial, *or* it grows into a genuinely reusable tool used by multiple repos, in which case it should be promoted into a real standalone project rather than half-pretending inside this repo.

---

### Switch to `martintrojer/tmux-fingers-rs` (accepted 2026-04-27)

Replaced `Morantron/tmux-fingers` (Crystal) with `martintrojer/tmux-fingers-rs`, a personal Rust port of the same plugin.

**Driver:** the Crystal toolchain. Building `tmux-fingers` from source requires a working Crystal install, which is painful on both macOS and Linux — Homebrew's `crystal` formula is frequently broken or lags upstream, distro packages are inconsistent, and the LLVM dependency makes container builds slow. The upstream install wizard tries to paper over this with prebuilt binaries and a Homebrew tap, but those are Linux x86_64 / Intel macOS only and depend on Morantron's release cadence. On Apple Silicon and on the Linux laptop, the realistic path was "install Crystal first," which kept failing.

A Rust port removes that pain entirely: `cargo install` works everywhere, prebuilt binaries on a CI matrix we control, and Rust is already installed for everything else here.

**Why this looks like it contradicts "Vendoring tmux plugins instead of using TPM" above:** that entry rejected *forking* `tmux-fingers` on the grounds that the implementation was too large to rewrite and a fork would freeze us off upstream. Both objections still apply in principle but were outweighed by the toolchain pain:

- The Rust port already exists, with feature parity and integration tests against real tmux servers. The rewrite was a finite up-front cost, not an ongoing maintenance commitment.
- Tracking upstream is preserved via an `upstream-crystal` branch on the fork that fast-forwards from `Morantron/tmux-fingers master`. New upstream commits get ported manually with `Port:` commit messages, so we are not actually frozen — just operating on a delay.

Still installed via TPM (`set -g @plugin 'martintrojer/tmux-fingers-rs'`); the binary is named `tmux-fingers-rs` so it can coexist on `$PATH` with the upstream Crystal `tmux-fingers` if needed. All `@fingers-*` configuration option names are unchanged.

**Reconsider if:** Crystal's installation story improves dramatically (rustup-equivalent, reliable static binaries on all our platforms), OR porting upstream changes becomes a meaningful chore (more than ~1 hr/quarter), OR a critical upstream fix lands and we're slow to port it twice in a row. In any of those cases the fallback is one line in `.tmux.conf` to switch back to `Morantron/tmux-fingers#<version>`.

---

*(Add another entry the next time a "yes" decision needs the same prose treatment as a rejected one — e.g. why `~/.agents/skills/` over per-agent fan-out, why the github Claude marketplace over local plugin install, why kill OMZ. Today these live in `README.md § Zen Of This Setup` and `CLAUDE.md`; promote to here when they need a longer-form explanation than a pillar.)*
