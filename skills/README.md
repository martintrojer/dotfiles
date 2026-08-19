# Agent Skills

Shared agent skills following the [Agent Skills standard](https://agentskills.io).

## Distribution

This is a package: skills live at `.agents/skills/<name>/` so the in-package path mirrors `$HOME`. `./dotfiles-sync --apply` links each `<name>/` into `~/.agents/skills/<name>` as a single directory symlink (a `bundle_dirs` package, so each skill links as an opaque bundle and vendored `README`/`LICENSE` files ride along). Codex, OpenCode, Pi, Cursor, Amp, Cline, Warp, OpenClaw, and other generic agents read that path natively. Edits propagate live.

Skills are auto-discovered and can be invoked explicitly with `/skill:name` or loaded automatically when the agent detects a matching task.

## Zen Of These Skills

The repo pillars ([root `README.md`](../README.md#zen-of-this-setup)) apply here
too — *each piece earns its place*, *every line is understood*. These are the
skill-specific ones, extracted from the calls actually made during the
2026-08 audit. Read before vendoring anything.

1. **A skill is discipline, not capability.** The model can already code. A
   skill earns its slot by changing *how* it works — a habit, a gate, a refusal.
   Skills that add domain knowledge age badly and are usually a doc, an
   extension, or a CLI instead.
2. **Every skill must be runnable here.** Prose describing a tool the harness
   doesn't have is worse than nothing: it burns context, then dead-ends.
   `council` sat broken for months calling a `Task` tool pi has never had.
   Check the primitives before vendoring, not after.
3. **Orchestration belongs to `mu`.** See below. Skills that bundle their own
   fan-out reimplement it in prose and lose the task graph.
4. **Trigger overlap is the real cost, not disk.** Two skills answering
   "review this" means neither reliably wins. Prefer one good skill plus a
   cross-link over two competing ones — which is why `ponytail-review` and
   `ponytail-audit` are skipped while `ponytail` is kept.
5. **Backport the idea, don't vendor the skill.** When something popular
   overlaps 80%, take the 20% that's new and put it where it belongs.
   Karpathy's *Surgical Changes* became six lines in `ponytail`; `grill-me`'s
   two good habits became two bullets in `brainstorm`.
6. **Usage is the audit.** Session history is ground truth. `execute-plan` had
   zero loads ever and went; `commit` had 150 and got invested in. Check before
   keeping something because it sounds useful.
7. **Vendored means re-syncable.** Minimal diffs, marked `<!-- local addition -->`,
   pinned by commit in the table below. A vendored skill you've rewritten is a
   fork you now maintain.
8. **Write down the rejections.** "Why don't we have the 175k-star skill?"
   should cost one table lookup, not another full survey. *Deliberately not
   vendored* is documentation that earns its keep.
9. **Cross-link at decision points.** A `## Related` row earns its place by
   redirecting work — *when* to jump, not *that* something adjacent exists.
   "See also" lists get skipped.
10. **Skills are prompts, so they get edited like prose.** Concrete examples
    beat abstract rules; a wrong example is worse than none (`commit` taught a
    subject-line style the repo doesn't use). Match the target's real
    conventions — read `git log` before writing a commit skill.

## Delegation is `mu`'s job

Skills here provide **single-agent discipline** — how one agent behaves in one
context. Multi-agent work is a separate layer, and this machine has a
preference order:

| Need | Use |
|------|-----|
| Long-lived crew, multi-phase or review-gated work, parallel tracks, anything that must survive compaction | [`mu`](https://github.com/martintrojer/mu) — the default |
| A helper you'll keep talking to, no DAG | `mu`'s reserved `scratch` workstream |
| One-shot "fire and get a result back", no follow-up | `pi-subagents` (`pi install npm:pi-subagents`; not currently installed) |

`mu` is symlinked into `~/.agents/skills/mu` from its own repo, not vendored
here. **Skills that bundle their own orchestration are not vendored** — they
duplicate `mu` in prose and lose the task graph, workspaces, and cherry-pick
flow. See *Deliberately not vendored* below.

## Skills

### Locally authored vs vendored

Locally authored skills carry a `version:` field in their frontmatter; vendored
skills deliberately do not, so their frontmatter stays byte-comparable against
upstream. That's the quickest way to tell the two apart:
`grep -L '^version:' */SKILL.md` lists exactly the vendored set.

Keep edits to vendored skills minimal so they stay easy to re-sync.

### Sync procedure

For re-syncing a vendored skill, or evaluating a new one. Upstreams are cloned
under `~/hacking/<name>/` — diff against those, not the network.

```bash
cd ~/hacking/<upstream> && git pull && git log --oneline -1   # pin the commit
diff -r ~/hacking/<upstream>/<path> skills/.agents/skills/<name>/
```

1. **Diff first.** Local patches are marked `<!-- local addition -->` or listed
   in the vendoring table. Re-apply them on top of new upstream text; don't
   merge upstream into a locally-edited file.
2. **De-Claude.** Grep every candidate for `~/.claude`, `subagent_type`,
   `Task(`, `curl localhost:*/notify`, `superpowers:`, `your human partner`,
   and slash commands the harness lacks. Zen #2: if it can't run, it doesn't go
   in.
3. **Check the frontmatter.** `name` must match the directory; description
   under 1024 chars. Drop upstream `version:` unless it's theirs.
4. **Check trigger overlap** against installed skills. Overlap → backport the
   delta (zen #5), don't add a competitor (zen #4).
5. **Link and verify:** `./dotfiles-sync --apply && make check-all`. A full
   `--apply` also prunes links for deleted skills; a package-scoped one won't.
6. **Record it.** Update the vendoring table with the new commit, and add a row
   to *Deliberately not vendored* for anything rejected (zen #8).

To re-run the usage audit that drives keep/delete calls (zen #6):

```bash
cd ~/.pi/agent/sessions
for s in ~/.agents/skills/*/; do
  n=$(grep -rl "agents/skills/$(basename "$s")/SKILL.md" . 2>/dev/null | wc -l)
  echo "$n $(basename "$s")"
done | sort -rn
```

Counts include the session doing the counting — discount the last few days when
a skill only shows loads from the audit itself.

Also discount anything recently linked. As of the 2026-08 re-audit the four
superpowers skills sit at 2 loads each purely because they were linked days
earlier; that is audit noise, not a delete signal. Their keep is **provisional**
until a later count sees real use.

| Skill | Upstream | Notes |
|-------|----------|-------|
| `unslop` | [conorbronsdon/avoid-ai-writing](https://github.com/conorbronsdon/avoid-ai-writing) (MIT) v3.23.0 @ `f9fef0e` + [cursor/plugins pstack](https://github.com/cursor/plugins/tree/main/pstack) (MIT) `unslop` @ `60c641e` | Local synthesis, not byte-comparable to either source. The compact `SKILL.md` is always on; the exhaustive catalog and zero-dependency detector/validator load only for explicit cleanup. The merged license retains both upstream notices. `patterns.js` remains a library called through `node -e` |
| `technical-writing` | [cursor/plugins pstack](https://github.com/cursor/plugins/tree/main/pstack) (MIT) @ `60c641e` | Upstream `skills/technical-writing`. Local: dropped Cursor's `disable-model-invocation` field so generic agents can discover it, plus a marked `## Related` row routing agent-facing documents to `writing-for-agents` |
| `caveman` | [JuliusBrussee/caveman](https://github.com/JuliusBrussee/caveman) (MIT) | Synced @ `7066cc8`, byte-identical to upstream `skills/caveman/SKILL.md` |
| `council` | [danielmiessler/LifeOS](https://github.com/danielmiessler/LifeOS) `install/skills/Council` | Upstream v1.1.20 @ `47df8ee`. **De-Clauded**: dropped the voice-notification curl, the `~/.claude/LIFEOS/` customization path, the execution-log JSONL, and the RedTeam cross-references; `name` lowercased to match the directory. Upstream's `subagent_type: general-purpose` calls became the harness-neutral *Running the members* section. Also fixed upstream's bare `CouncilMembers.md` / `SKILL.md` references inside `Workflows/` to `../` |
| `ponytail` | [DietrichGebert/ponytail](https://github.com/DietrichGebert/ponytail) (MIT) | Synced @ `16f2980`. One local addition: a *Touch only what you must* section adapted from [forrestchang/andrej-karpathy-skills](https://github.com/forrestchang/andrej-karpathy-skills) (MIT) — upstream ponytail covers what you don't *write*, not what you don't *touch*. Upstream also ships five sibling skills (`-review`, `-audit`, `-debt`, `-gain`, `-help`), deliberately not vendored — see below |
| `summarize` | [steipete/summarize](https://github.com/steipete/summarize) | Locally rewritten backend section: this host pins OpenCode (`big-pickle`) as the only provider. See `summarize/README.md` |
| `writing-for-agents` | [mattpocock/skills](https://github.com/mattpocock/skills) (MIT) `skills/productivity/writing-for-agents` @ `84fdeff` | Plus `SKILL-MECHANICS.md`. Local: dropped the `CLAUDE.md` mentions, added the pstack enforcement ladder, and routes human-facing prose to `technical-writing` and cleanup to `unslop`. Upstream's `agents/openai.yaml` is Codex-plugin metadata, not vendored |
| `receiving-code-review` | [obra/superpowers](https://github.com/obra/superpowers) (MIT) @ `44c9b2d` | Local: "your human partner" → "the user"; two quoted-maxim attributions rewritten as standalone rules |
| `systematic-debugging` | [obra/superpowers](https://github.com/obra/superpowers) (MIT) @ `44c9b2d` | Plus `root-cause-tracing.md`, `defense-in-depth.md`, `condition-based-waiting.md` + example, `find-polluter.sh`. Local: retargeted two `superpowers:`-prefixed sub-skill references, fixed a mangled `## your human partner's Signals` heading |
| `test-driven-development` | [obra/superpowers](https://github.com/obra/superpowers) (MIT) @ `44c9b2d` | Plus `writing-good-tests.md` and earlier retargeting. Local selection gate adapted from pstack `tdd` @ `60c641e`: require red-first for requested TDD or cheap bug tests, otherwise use the closest meaningful executable check |
| `verification-before-completion` | [obra/superpowers](https://github.com/obra/superpowers) (MIT) @ `44c9b2d` | Local pstack @ `60c641e` backport: name and prove the safety invariant for risky indirect changes, or label it unproven |

### Planning & Execution

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:brainstorm` | "brainstorm", "design a feature", "think through" | Work a decision tree in dependency-ordered rounds, then write an approved spec at `docs/specs/`. Hard gate: no code before the user approves a design |
| `/skill:write-plan` | "write a plan", "plan this feature" | Turn an approved spec into ordered, independently verifiable tasks at `docs/plans/`. TDD steps, exact paths, no placeholders |

### Version Control

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:commit` | "commit", "make a commit", "commit my changes" | Detect the active VCS (git/jj/hg), draft a message matching the repo's prevailing style, commit non-interactively. Heavy on jj traps: `$EDITOR` hangs, the post-commit working-copy gotcha, the operation log |

### Writing

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:unslop` | Every response and prose artifact; explicit cleanup on "unslop", "remove AI-isms", "audit", "rewrite", or "edit" | Always-on direct, specific prose discipline. Explicit requests unlock the detailed catalog, detect/rewrite/edit modes, and deterministic detector/validator; ambient use never runs them |
| `/skill:technical-writing` | Writing or reviewing human-facing docs, RFCs, READMEs, PR descriptions, or commit messages | Diátaxis, Google developer style, Simplified Technical English, and Global English for tutorials, how-tos, reference, explanations, RFCs, READMEs, PR descriptions, and commit bodies |
| `/skill:wait-what` | You stopped following a reply. **User-invoked only** — type it | Re-pitch the last message: add the skipped premise, flatten the structure, keep every path/command/number verbatim. Simpler, not shorter |
| `/skill:writing-for-agents` | Creating or editing a skill, or modifying `AGENTS.md` | Prose agents read: context pointers, progressive disclosure, completion criteria, the no-op hunt, and an enforcement ladder that moves recurring constraints out of prose when code can own them |

### Code Quality

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:code-reviewer` | "review this code", after refactors | Find dead code, duplication, unnecessary complexity, leakage, temporal decomposition, reader-held state, and prose constraints code should enforce |
| `/skill:test-reviewer` | "review tests", after writing tests | Catch false confidence, excessive mocking, meaningless assertions |
| `/skill:test-driven-development` | Explicit TDD requests, or bugs with a cheap meaningful local test | Red-green-refactor when selected; otherwise state why a new test would be weak or expensive and use the closest executable verification |
| `/skill:systematic-debugging` | Any bug, test failure, or unexpected behaviour | Four-phase root-cause discipline. No fixes before investigation; symptom patches are failures |
| `/skill:receiving-code-review` | Getting review feedback, before acting on it | Verify before implementing. Kills performative agreement ("You're absolutely right!") and blind implementation |
| `/skill:verification-before-completion` | About to claim something works | Evidence before claims. For risky indirect changes, name and prove the safety invariant against real code or a running artifact, or call it unproven |

### Multi-Agent

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:council` | "council", "debate", "weigh options" | Multi-agent collaborative-adversarial debate with visible transcripts |

### Caveman Mode (Token Efficiency)

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:caveman` | "caveman mode", "be brief" | Ultra-compressed communication (~65% measured token savings). Levels: lite, full, ultra |
| `/skill:ponytail` | "ponytail", "be lazy", "yagni", "simplest solution" | Forces the laziest solution that works: YAGNI, stdlib/native first, shortest diff. Levels: lite, full, ultra |

### Tools & Integrations

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:summarize` | "use summarize.sh", "summarize this URL/article", "transcribe this YouTube/video" | `summarize` CLI helper for URLs, podcasts, local files, and best-effort transcript extraction. Routed through OpenCode |

## Deliberately not vendored

### superpowers — the orchestration half

`obra/superpowers` ships 14 skills. Four are here; the rest are skipped, and
most of them for one reason: **they are an orchestration stack, and `mu` is the
orchestrator on this machine.**

| Skipped | Why |
|---------|-----|
| `subagent-driven-development` (503 lines) | Fresh-subagent-per-task with two-stage review gates. That is exactly `mu`'s `implement → review → address → ship` DAG, but expressed as prose the agent has to hand-execute rather than a tool with a real task graph, workspaces, and cherry-pick flow |
| `dispatching-parallel-agents` | Parallel fanout over independent tasks — `mu`'s parallel tracks, with automatic diamond-merge |
| `requesting-code-review` | A reviewer-subagent prompt template. `mu` spawns `reviewer-N` roles directly; for one-shot review with no follow-up, `pi-subagents` has a purpose-built `reviewer` agent |
| `executing-plans` | Superseded by `write-plan`'s inline handoff. Its own text says to prefer `subagent-driven-development` when subagents exist |
| `using-git-worktrees` | `mu` manages per-agent workspaces itself; this repo is jj-first, and the skill is git-only |
| `finishing-a-development-branch` | Git-branch-and-PR integration flow. Doesn't fit a jj working-copy model |
| `using-superpowers` | Bootstrap telling the agent how to find superpowers skills. Pi discovers `~/.agents/skills/` natively |
| `writing-skills` (679 lines) | Meta-skill for authoring skills. Real, but 679 lines of context for something done a few times a year — read it from `~/hacking/superpowers/` when actually writing one |

The kept four (`test-driven-development`, `systematic-debugging`,
`receiving-code-review`, `verification-before-completion`) share a property: all
are **single-agent discipline**. They change how one agent behaves in one
context, need no dispatch primitive, and so compose with `mu` instead of
competing with it — including inside a `mu` worker pane.

### Surveyed and skipped (2026-08)

A scan of the high-star skill lists for anything overlapping this set. Most of
what's popular is domain capability (UI design, browser automation, docs
fetching) rather than engineering discipline; these are the ones close enough
to be worth a decision.

| Skill | Stars | Verdict |
|-------|-------|---------|
| [cursor/plugins pstack](https://github.com/cursor/plugins/tree/main/pstack) skills @ `60c641e` | — | **Partly taken.** Added `technical-writing`; merged compact `unslop` with the stronger existing cleanup machinery; backported caller-first design, the cheap-test TDD gate, design-shape review checks, blast-radius invariants, and the enforcement ladder. Skipped `poteto-mode`, `arena`, `swarm`, `interrogate`, `reflect`, setup/model routing, worktree/Graphite flows, and multi-model review because they rely on Cursor primitives or duplicate `mu`. Skipped `bro` because `wait-what` owns that decision. Skipped `how`, `why`, `teach`, `recall`, verification-skill generators, TypeScript guidance, and standalone principle skills because they add capability, environment-specific integration, or overlapping instructions rather than a distinct discipline |
| [`andrej-karpathy-skills`](https://github.com/forrestchang/andrej-karpathy-skills) | ~175k | **Partly taken.** Four rules: *Think Before Coding* ≈ `brainstorm`, *Simplicity First* ≈ `ponytail`, *Goal-Driven Execution* ≈ `test-driven-development` + `verification-before-completion`. Only *Surgical Changes* had no home — adapted into `ponytail`. Also it's a `CLAUDE.md`, always-on by design, which is a worse fit than a triggered skill |
| [`mattpocock/skills`](https://github.com/mattpocock/skills) `grill-me` / `grilling` | ~200k repo | **Partly taken**, twice. First pass took the recommended-answer-per-question and answer-from-the-repo refinements. Re-checked @ `84fdeff`: `grilling` had since become a **design tree** worked in **rounds** — ask the whole *frontier* (decisions whose prerequisites are settled) at once, recompute, repeat, done when the frontier is empty. That replaced `brainstorm`'s strict one-question-per-message rule, which was paying round-trips to re-derive an ordering the tree already encodes. `grill-me` and `grill-with-docs` are thin aliases into it |
| `mattpocock/skills` `handoff` | — | Skipped. Pi ships a `handoff` **extension** (`examples/extensions/handoff.ts`) that forks a real session — strictly better than a skill that writes a markdown file |
| `mattpocock/skills` main flow (`to-spec`, `to-tickets`, `implement`, `triage`, `wayfinder`) | — | Skipped. An issue-tracker-shaped pipeline needing `setup-matt-pocock-skills` per repo. Overlaps `brainstorm` → `write-plan` → `mu` and assumes GitHub/Linear |
| `superpowers` `writing-skills` | — | Skipped. 679 lines for something done a few times a year — read it from `~/hacking/superpowers/` when actually writing one. (mattpocock's `writing-great-skills` was rejected alongside it on the same size grounds; it has since been rewritten to 81 lines as `writing-for-agents` and is now **vendored** — see the table above) |
| `mattpocock/skills` `diagnosing-bugs`, `tdd`, `code-review` | — | **Partly taken.** Three backports rather than three competing skills (zen #4): the tight-feedback-loop Phase 1 and ranked-falsifiable-hypotheses fix into `systematic-debugging`, pre-agreed seams and the horizontal-slicing anti-pattern into `test-driven-development`, and the Fowler smell baseline into `code-reviewer`. Skipped from `code-review`: the Standards/Spec two-axis split and its parallel sub-agents — no issue tracker here, and the fan-out is `mu`'s |
| `mattpocock/skills` `codebase-design`, `improve-codebase-architecture`, `prototype`, `wizard`, `teach`, `research`, `resolving-merge-conflicts` | — | Skipped. Capability, not discipline (zen #1). `research` dispatches a background agent (`mu`'s job); `improve-codebase-architecture` renders a Tailwind/Mermaid HTML report and depends on a `CONTEXT.md` this repo doesn't keep; `resolving-merge-conflicts` is 14 lines of git-only flow in a jj-first repo |
| `code-simplifier`, `pr-review-expert`, `tech-debt-tracker` | various | Skipped. `code-reviewer` + `ponytail` cover this ground and are actually used (57 loads) |
| Frontend Design, UI/UX Pro Max, Vercel React/design rules, theme-factory | 90k+ | Not applicable. No frontend work in this repo |
| Claude Mem, Context7, Supermemory, Skill Seekers | various | Skipped. Memory/doc-fetching infrastructure, not discipline. `mu` task notes already survive compaction |
| `agent-browser`, `playwright-skill`, `webapp-testing` | 14k+ | Skipped. Nothing to browser-test here |

### The "say it simpler" genre

Surveyed when `wait-what` was written. Two upstreams were worth reading, neither
worth vendoring — the result is the distilled local `wait-what` above (zen #5).

| Skill | Verdict |
|-------|---------|
| [`luchasarie/bro-skill`](https://github.com/luchasarie/bro-skill) (MIT) @ `01e51f8` | **Partly taken.** Its two real contributions — *facts survive verbatim* and *simpler, not shorter* — are rules in local `wait-what`. Skipped: the "light bro flavor" rule (noise), the same-language rule (default behaviour on an English-only host), the PT-BR examples, and a four-tool `install.sh` that `dotfiles-sync` replaces |
| `mattpocock/skills` `wait-what` | **Partly taken.** The *mechanism* is right and is the one kept: name the **listener's** state, not the output. "Be concise" makes the model clip words and lose you; "wait, you lost me" makes it back up. Also its stay-tiny discipline — a 400-line concision skill still leaves the model verbose, because the model reads the volume, not the plea. Skipped: `CONTEXT.md` (not a convention here — retargeted to `AGENTS.md`) and ASD-STE100 Simplified Technical English, a controlled-language spec the model only half-knows. Its doc's "how far back to go" note was promoted into the skill body, where it's load-bearing |
| [`DreambigOu/ELI5`](https://github.com/dreambigou/eli5) | Skipped. Retargets an explanation at a chosen audience (kid, manager, engineer) — capability, not discipline (zen #1), and re-aiming is *re-answering*, which is the one thing this genre must not do |
| `/eli5`, `/tldr`, `/no-fluff`, `/talk-normal` as prompt macros | Skipped, and they are the anti-pattern. All name the **output**, so the model over-corrects into a caveman register: shorter and no clearer. `caveman` already owns deliberate compression |

### ponytail — the sibling skills

Upstream `ponytail` ships six skills. Only the mode skill itself is here:

| Skipped | Why |
|---------|-----|
| `ponytail-review`, `ponytail-audit` | `code-reviewer` §3 already covers the same ground (stdlib/native-first, single-implementation abstractions, speculative flexibility) and cross-references `ponytail` as its build-time mirror. Two skills competing for "review this" splits the trigger for no gain |
| `ponytail-debt` | Harvests `ponytail:` comments into a ledger. This repo has **zero** such markers, and the skill's whole mechanism is one grep: `grep -rnE '(#\|//) ?ponytail:' .`. Revisit if the markers ever accumulate |
| `ponytail-gain` | Prints upstream's benchmark medians as an ASCII scoreboard. No local signal |
| `ponytail-help` | Reference card for `/ponytail-*` slash commands and a Claude Code plugin auto-update flow, neither of which exists here |

The `ponytail:` comment convention itself lives in the main skill and stays.

## Removed

- **`execute-plan`** (2026-08) — never invoked in any recorded session, and its
  batching section assumed a `Task` subagent tool pi doesn't have. `write-plan`
  now closes with inline handoff guidance instead of pointing at a third skill.
- **`tmux`** (2026-08) — driving agents in tmux panes is `mu`'s job now, and the
  remaining niche (interactive REPL/debugger scraping) had zero recorded uses.
  Recoverable from git history if the REPL case comes back.
