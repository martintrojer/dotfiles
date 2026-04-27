# Decisions

Past decisions about this setup — both rejected alternatives and non-obvious "yes" choices. Read before relitigating.

Each entry is dated and structured the same way: what was audited, what was found, which Zen pillars it helped or violated, the conclusion, and what would justify revisiting.

Sources of truth:

- Pillars: [`README.md` § Zen Of This Setup](../README.md#zen-of-this-setup).
- Daily editing rules: [`CLAUDE.md`](../CLAUDE.md) (also symlinked as `AGENTS.md`).

---

## Rejected

### Vendoring tmux plugins instead of using TPM (rejected, ongoing)

Recurring temptation: tmux config currently loads 5 third-party plugins via TPM (`tmux-yank`, `tmux-cpu`, `tmux-fingers`, `tmux-fzf`, `vim-tmux-navigator`). "Pillar #5 says local scripts over upstream plugins — should I vendor or rewrite these?" Audited each.

- **`vim-tmux-navigator`:** ~30 lines of tmux config that detects vim and forwards `C-h/j/k/l` accordingly. Rewriting locally is feasible but the upstream version handles edge cases (fzf panes, ssh sessions) I'd re-discover the hard way.
- **`tmux-fingers`:** hint-overlay copy-mode (think vimium for tmux panes). Implementation is hundreds of lines of Crystal + shell. Not feasible to rewrite, not worth vendoring (would just freeze upstream and miss bug fixes). *(See "Switch to tmux-fingers-rs" below — this changed.)*
- **`tmux-fzf`:** session/window/pane pickers via fzf. Used primarily for the clipboard-history popup (`prefix + v`). Rest of fzf integration is bespoke (`tms`, `cheatsheet`, `agent-attention`). Could drop it if I rewrote the clipboard picker; haven't yet because it's ~5 lines of bind + works.
- **`tmux-yank`:** clipboard integration that handles Wayland/macOS/Linux differences. Replicating the platform-detection + `wl-copy`/`pbcopy`/`xclip` fallback chain locally is exactly the kind of cross-platform busywork pillar #8 says to avoid in shared config.
- **`tmux-cpu`:** status-line CPU% formatter. *Could* be replaced by a `status-cpu` script alongside the existing `status-ram` / `status-uptime` Python helpers. On the edge. Kept because the output format and color thresholds are already tuned and there's no specific complaint.
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

## Accepted (non-obvious)

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
