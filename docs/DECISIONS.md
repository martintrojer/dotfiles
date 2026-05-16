# Decisions

Past decisions about this setup — rejected alternatives and non-obvious "yes" choices. Read before relitigating.

Each entry: context, key points, what would justify revisiting. Pillars from [`README.md`](../README.md#zen-of-this-setup); daily editing rules in [`CLAUDE.md`](../CLAUDE.md).

---

## Rejected

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

**Reconsider only if:** Hammerspoon gains a robust native wrapper around previous-desktop behavior that feels native rather than like a Mission Control hack.

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
- What niri does better: out-of-box animations and visual polish. Nice; not load-bearing.
- Violates #1, #2, #5.

**Reconsider only if:** niri ships a stable 1.0 with frozen IPC/config, the i3-IPC ecosystem gains first-class niri support, AND scrollable-tiling gets an addressable-layout escape hatch.

---

### SwayFX (tried and reverted 2026-04-28)

Swapped upstream sway for [SwayFX](https://github.com/WillPower3309/swayfx) for an afternoon. Full effects stack: `corner_radius 8`, 2-pass blur on waybar/mako/fuzzel/swaylock, soft Catppuccin shadows, 10% inactive dim. Reverted same day.

- Nice: rounded corners genuinely changed how the desktop *feels*. Glass blur looked good in screenshots.
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
- **macOS → Ghostty.** The most macOS-native option: native AppKit, real app-bundle launch, single-process tab/split model aligned with Mission Control. 2026-04-30 OOM acknowledged as a known risk; daily `~/.zsh_history` snapshot is the load-bearing mitigation.
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

**Reconsider only if:** module count crosses ~20, OR cross-imports form a real cycle, OR a single function genuinely needs to span three modules.

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

### No unit tests for the control-plane and helper scripts (accepted 2026-05-01)

Only test runner is `tmux/.config/tmux/scripts/test-status-tools`. `_dotfiles_sync/`, `fuzzel/.config/fuzzel/scripts/_common.py`, `local-bin/`, `sway/.config/sway/scripts/`, `fedora/bin/.local/bin/` all have zero tests beyond `ruff` and `ty`.

- **Pillar #6** (recreate, don't restore): blast radius is one shell session or one `--apply`. Sessions are disposable; regressions show up next run.
- **Pillar #3**: scripts are short enough to audit end-to-end on read.
- **Pillar #1**: a test runner + CI is non-trivial infra for a personal repo whose loop is "edit, run, observe".
- **The tmux exception:** `test-status-tools` exists because status helpers run headless every few seconds, so silent breakage is invisible.

**Reconsider individual scripts if:** any grows past ~300 lines AND ships a bug that costs >30 min to debug. Add a focused smoke test for that regression, not a generic suite.

---

### Split the repo control plane into `dotfiles-sync` + `_dotfiles_sync/` (accepted 2026-04-27)

Replaced a ~1300-line bootstrap script with a root entrypoint (`dotfiles-sync`) and a repo-local Python package (`_dotfiles_sync/{cli,config,system,stow,checks,external,model}.py`). Cross-cutting repo docs moved to `docs/`; the tiny `vscode/` pseudo-package was demoted to `docs/VSCODE.md`.

- Driver: the old script mixed stow orchestration, package inventory, pinned clones, agent fan-out, drift checks. No longer read like intentional software.
- Top-level stow packages, `fedora/`, `skills/`, `pi/`, and the Claude marketplace baggage (`agents/`, `hooks/`, `.claude-plugin/`) all stayed put — the marketplace layout is an external integration contract.
- Improves #1, #3, #10. Tax is one new directory.

**Reconsider only if:** control plane shrinks below ~300 lines (collapse the split), OR grows into a genuinely reusable tool (promote to standalone).

---

### Switch to `martintrojer/tmux-fingers-rs` (accepted 2026-04-27)

Replaced `Morantron/tmux-fingers` (Crystal) with a personal Rust port.

- **Driver:** Crystal toolchain is painful on macOS and Linux. Homebrew's formula lags, distro packages are inconsistent, LLVM dep makes container builds slow. Upstream's prebuilt binaries are Linux x86_64 / Intel macOS only. On Apple Silicon and the Linux laptop, the real path was "install Crystal first" — which kept failing. Rust port: `cargo install` works everywhere.
- **Why this looks like it contradicts "vendor tmux plugins" above:** that entry rejected *forking* on the grounds that fingers was too large to rewrite and a fork would freeze upstream. The Rust port already exists with feature parity. Upstream tracking is preserved via an `upstream-crystal` branch that fast-forwards from Morantron's master; new commits get manually ported with `Port:` messages.
- Installed via TPM (`set -g @plugin 'martintrojer/tmux-fingers-rs'`); binary is `tmux-fingers-rs` so it can coexist with upstream on `$PATH`. All `@fingers-*` option names unchanged.

**Reconsider if:** Crystal's install story improves dramatically, OR porting upstream becomes >1 hr/quarter, OR a critical upstream fix lands and we're slow to port twice in a row.

---

*(Promote entries from `README.md § Zen Of This Setup` or `CLAUDE.md` to here when they need longer-form than a pillar bullet.)*
