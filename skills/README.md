# Agent Skills

Shared agent skills following the [Agent Skills standard](https://agentskills.io).

## Distribution

This is a package: skills live at `.agents/skills/<name>/` so the in-package path mirrors `$HOME`. `./dotfiles-sync --apply` links each `<name>/` into `~/.agents/skills/<name>` as a single directory symlink (a `bundle_dirs` package, so each skill links as an opaque bundle and vendored `README`/`LICENSE` files ride along). Codex, OpenCode, Pi, Cursor, Amp, Cline, Warp, OpenClaw, and other generic agents read that path natively. Edits propagate live.

Skills are auto-discovered and can be invoked explicitly with `/skill:name` or loaded automatically when the agent detects a matching task. Every auto-loadable skill's description sits in every system prompt; skills with `disable-model-invocation: true` (`summarize`, `technical-writing`, `wait-what`) stay out of it and load only via `/skill:name`.

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
   `council` sat broken for months calling a `Task` tool pi has never had
   (it now runs its members through `mu_delegate`).
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
6. **Usage is evidence, not ground truth.** Load rates track the model: on the
   same project and briefs, mu workers on gpt-5.6-sol loaded `ponytail` in 96%
   of sessions and claude-opus-5-5 in 1% (`verification-before-completion`:
   89% vs 0%). Many loads are not a person asking either: 73% of gpt-5.6-sol's
   direct `ponytail` loads were mu reviewer panes. So split counts by model and
   by worker/delegate/interactive, and keep a skill for an effect you can see in
   the sessions, not for its load count. The data is the museum archive of every
   pi session (`~/.agents/skills/museum`); `usage-audit.py` below counts it.
7. **Vendored means re-syncable.** Pin the upstream commit in the table below
   and watch upstream: the 2026-10 re-sync pulled a real TDD rule from upstream
   `5bf4e78`. Local forks that cut or rewrite upstream text are fine when
   attributed: mark each change with a `<!-- local` comment
   (`<!-- local addition -->` or `<!-- local: what and why -->`) and say what
   changed in the table row, so a re-sync can re-apply it.
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
| A multi-agent *pattern*: fan-out, adversarial review, refute, tournament, loop-until-done, ultrathink | `mu`'s recipes, `~/.agents/skills/mu/recipes/` — skills here link to them, never copy them |

`mu` is symlinked into `~/.agents/skills/mu` from its own repo, not vendored
here. **Skills that bundle their own orchestration are not vendored** — they
duplicate `mu` in prose and lose the task graph, workspaces, and cherry-pick
flow. See *Deliberately not vendored* below.

## Skills

### Locally authored vs vendored

Locally authored skills carry a `version:` field in their frontmatter; vendored
skills do not. From the repo root,
`grep -L '^version:' skills/.agents/skills/*/SKILL.md` lists the vendored set,
with one exception: `unslop` is a local synthesis of two upstreams and carries
`version:`. The vendoring table below is the authoritative list.

### Sync procedure

For re-syncing a vendored skill, or evaluating a new one. Upstreams are cloned
under `~/hacking/<name>/` — diff against those, not the network.

```bash
cd ~/hacking/<upstream> && git pull && git log --oneline -1   # pin the commit
diff -r ~/hacking/<upstream>/<path> skills/.agents/skills/<name>/
```

1. **Diff first.** Local patches are marked with a `<!-- local` comment and
   listed in the vendoring table. Re-apply them on top of new upstream text; don't
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

To re-run the usage audit behind keep/delete calls (zen #6), run
[`usage-audit.py`](usage-audit.py) from the repo:

```bash
skills/usage-audit.py | sort -k1,1nr | head -30
```

It reads the museum store from `~/.config/museum/config.toml` (over ssh when
`[store] host` is set) plus this machine's `~/.pi/agent/sessions`, and prints
sessions per skill × model × kind. A load is a `read` call on
`…/.agents/skills/<name>/SKILL.md`; reads of the `dotfiles/skills/` copy are
edits and are skipped. Kind is `worker` (mu workspace), `delegate` (first user
message starts "You are reviewing"), or `interactive`. Discount recently linked
skills and the sessions doing the counting.

| Skill | Upstream | Notes |
|-------|----------|-------|
| `unslop` | [conorbronsdon/avoid-ai-writing](https://github.com/conorbronsdon/avoid-ai-writing) (MIT) v3.37.0 @ `0469c97` + [cursor/plugins pstack](https://github.com/cursor/plugins/tree/main/pstack) (MIT) `unslop` @ `d0ef80d` | Local synthesis, not byte-comparable to either source; carries `version:` (0.2.0). 2026-10 (panel H1): no longer always-on. The description fires only on explicit cleanup requests; the always-on opener, *Pick the branch*, and the Ambient branch are gone, so explicit cleanup is the only mode, and *Ambient rules* is now *Rewrite rules*. The catalog and zero-dependency detector/validator load as part of that cleanup. The merged license retains both upstream notices. `patterns.js` remains a library called through `node -e`. 2026-10 re-sync: catalog body, severity tiers, `CATEGORIES.md`, and detector JS taken verbatim; pstack's *mannered prose* and *over-compression* rules backported as two bullets in `SKILL.md` |
| `technical-writing` | [cursor/plugins pstack](https://github.com/cursor/plugins/tree/main/pstack) (MIT) @ `d0ef80d` | Upstream `skills/technical-writing`. Keeps upstream's `disable-model-invocation: true`, so it is user-invoked (`/skill:technical-writing`): a 2026-10 restore (panel H3), after auto-loaded workers pushed the swayward docs voice formal. Local: commit messages stay with `commit`; marked `## Related` rows route agent-facing documents to `writing-for-agents` and commits to `commit` |
| `council` | [danielmiessler/LifeOS](https://github.com/danielmiessler/LifeOS) `LifeOS/install/skills/Council` | Upstream v1.1.20 @ `47df8ee` (unchanged at `5e2f2e8`). **De-Clauded**: dropped the voice-notification curl, the `~/.claude/LIFEOS/` customization path, the execution-log JSONL, and the RedTeam cross-references; `name` lowercased to match the directory. Upstream's `subagent_type: general-purpose` calls became the *Running the members* section: one `mu_delegate` call per member per round, in-context when that tool is absent. Also fixed upstream's bare `CouncilMembers.md` / `SKILL.md` references inside `Workflows/` to `../`; Integration points at mu's `tournament` / `refute` recipes. 2026-10 (panel H3): description cut to explicit council/debate requests; "weigh options", "deliberate", "pros and cons" and "what would experts say" stole ordinary trade-off questions |
| `ponytail` | [DietrichGebert/ponytail](https://github.com/DietrichGebert/ponytail) (MIT) | Synced @ `16f2980` (skill unchanged at `552acd5`). One local addition: a *Touch only what you must* section adapted from [forrestchang/andrej-karpathy-skills](https://github.com/forrestchang/andrej-karpathy-skills) (MIT) — upstream ponytail covers what you don't *write*, not what you don't *touch*. 2026-10 (panel H1): description narrowed to explicit triggers (upstream says "Use on ANY coding task"); active for the task it was invoked on, not every response; switch is `/skill:ponytail lite\|full\|ultra`; the "ONE runnable check" mandate defers to `test-driven-development`'s selection gate and the repo's test policy; "pair with Caveman" dropped. Upstream also ships five sibling skills (`-review`, `-audit`, `-debt`, `-gain`, `-help`), deliberately not vendored — see below |
| `summarize` | [steipete/summarize](https://github.com/steipete/summarize) `.agents/skills/summarize` @ `ddbae2bf` | Upstream's canonical skill (previously the older openclaw copy). Local: trigger phrases in the description; `disable-model-invocation: true` (2026-10, panel H3: low use, user-invoked via `/skill:summarize`); *Backend* section pinning OpenCode (`big-pickle`) and the `UpgradeRequired` fix, pointing at `~/dotfiles/summarize/README.md`; *Extract-and-pipe handoff* to pi/codex; dropped the repo-relative doc links and *Ownership* section |
| `writing-for-agents` | [mattpocock/skills](https://github.com/mattpocock/skills) (MIT) `skills/productivity/writing-for-agents` @ `6fd9479` | Plus `SKILL-MECHANICS.md`. Local: dropped the `CLAUDE.md` mentions, added the pstack enforcement ladder, and routes human-facing prose to `technical-writing`, cleanup to `unslop`, and mu worker briefs to mu's `recipes/brief.md`. The marked `## Related` table was updated in 2026-10: `unslop` is the cleanup pass when asked, `technical-writing` is user-invoked. Upstream's `agents/openai.yaml` is Codex-plugin metadata, not vendored |
| `receiving-code-review` | [obra/superpowers](https://github.com/obra/superpowers) (MIT) @ `8ca22db` | Local: "your human partner" → "the user"; two quoted-maxim attributions rewritten as standalone rules |
| `systematic-debugging` | [obra/superpowers](https://github.com/obra/superpowers) (MIT) @ `8ca22db` | Plus `root-cause-tracing.md`, `condition-based-waiting.md` + example, `find-polluter.sh`. Local fork, every change marked. Backported from mattpocock `diagnosing-bugs`: the tight-feedback-loop Phase 1 (formerly an Iron Law addition) and ranked falsifiable hypotheses. 2026-10 (panel H2, ~2000 → ~1050 words): cut Iron Law, When to Use, Phase 2 (its diff-against-a-working-example became a Phase 1 bullet), Red Flags, user Signals, Rationalizations, Quick Reference, "No Root Cause", the three-fixes rule (now one line: repeated failed fixes mean the root-cause model is wrong), and `defense-in-depth.md`. The loop-first rule is now "strongest practical reproduction or evidence loop; if none, say so and work from logs and traces". Phases renumbered 1-3; the Phase 3 check is a test when cheap, else the closest executable check |
| `test-driven-development` | [obra/superpowers](https://github.com/obra/superpowers) (MIT) @ `8ca22db` | Plus `writing-good-tests.md` and earlier retargeting. Local fork, every change marked. Selection gate adapted from pstack `tdd` @ `60c641e`, which rewrites upstream's always-TDD stance: require red-first for requested TDD or cheap bug tests, otherwise use the closest meaningful executable check. Seams, vertical slices and horizontal slicing from mattpocock `tdd`. 2026-10 (panel H2, ~2000 → ~1100 words): cut Iron Law (one sentence in the Overview), "delete means delete", dot graph, Rationalizations, Red Flags, Bug Fix example, Verification Checklist, When Stuck, Final Rule; code pairs and the quality table condensed to prose. Kept the gate, seams, Verify RED/GREEN, and the project's-suite rule. Related points at `code-reviewer`'s Tests section |
| `verification-before-completion` | [obra/superpowers](https://github.com/obra/superpowers) (MIT) @ `8ca22db` | 2026-10 (panel H2, ~1200 → 600 words): cut Iron Law, Red Flags, Rationalization Prevention, Key Patterns, When To Apply. The Gate's RUN step says "fresh for the artifact as it is now" instead of upstream's "run in this message". Kept *Common Failures* (three rows local). Related points at `code-reviewer`'s Tests section. Local pstack @ `60c641e` backport: name and prove the safety invariant for risky indirect changes, or label it unproven. Plus a Muse Code backport (2026-09): *Could the Check Have Failed?* — self-built oracles, the run that disagreed, tuned conditions, proxy evidence, read-only verification. Its sibling rule (*a refusal is a result, don't bypass it*) went to the `refusal` **extension** instead, per the enforcement ladder: it fires on observable facts and must not depend on the agent choosing to remember it |

### Planning & Execution

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:brainstorm` | "brainstorm", "design a feature", "think through an idea" | Work a decision tree in dependency-ordered rounds, then write an approved spec at `docs/specs/`. Hard gate: no code before the user approves a design. Skip genuine one-liners with no design question. Hands the approved spec to mu's `recipes/plan.md` for the task DAG |

### Version Control

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:commit` | "commit", "make a commit", "commit my changes" | Detect the active VCS (jj before git, then hg), draft a message matching the repo's prevailing style, commit non-interactively. Heavy on jj traps: editor hangs (`ui.editor` beats `$EDITOR`), the post-commit working-copy gotcha; undo and conflicts in `jj-recovery.md` |

### Writing

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:unslop` | "unslop", "remove AI-isms", or a request to detect, audit, rewrite, edit, or verify prose | Prose cleanup that keeps meaning, facts, code, and the author's voice. Detailed catalog, detect/rewrite/edit modes, and a deterministic detector/validator |
| `/skill:technical-writing` | **User-invoked: `/skill:technical-writing`**, for human-facing docs, RFCs, READMEs, or PR descriptions | Diátaxis, Google developer style, Simplified Technical English, and Global English for tutorials, how-tos, reference, explanations, RFCs, READMEs, and PR descriptions |
| `/skill:wait-what` | You stopped following a reply. **User-invoked only** — type it | Re-pitch the last message: add the skipped premise, flatten the structure, keep every path/command/number verbatim. Simpler, not shorter |
| `/skill:writing-for-agents` | Creating or editing a skill, or modifying `AGENTS.md` | Prose agents read: context pointers, progressive disclosure, completion criteria, the no-op hunt, and an enforcement ladder that moves recurring constraints out of prose when code can own them |

### Code Quality

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:code-reviewer` | Asked to review code or tests | Find dead code, duplication, unnecessary complexity, leakage, temporal decomposition, reader-held state, and prose constraints code should enforce. Its Tests section catches excessive mocking, fake and weak assertions, and runs a mental mutation check |
| `/skill:test-driven-development` | Explicit TDD requests, or bugs with a cheap meaningful local test | Red-green-refactor when selected; otherwise state why a new test would be weak or expensive and use the closest executable verification |
| `/skill:systematic-debugging` | Any bug, test failure, or unexpected behaviour | Three-phase root-cause discipline: build the strongest feedback loop you can, rank falsifiable hypotheses, then fix at the source. No fixes before investigation |
| `/skill:receiving-code-review` | Getting review feedback, before acting on it | Verify before implementing. Kills performative agreement ("You're absolutely right!") and blind implementation |
| `/skill:verification-before-completion` | About to claim something works | Evidence before claims. For risky indirect changes, name and prove the safety invariant against real code or a running artifact, or call it unproven |

### Multi-Agent

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:council` | Explicit "council", "debate", "multi-agent debate", "multiple expert perspectives" | Multi-agent collaborative-adversarial debate with visible transcripts. For choosing among many candidates or attacking findings, mu's `tournament` / `refute` recipes fit better |

### Simplicity

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:ponytail` | "ponytail", "be lazy", "yagni", "simplest/minimal solution", "do less", complaints about over-engineering or bloat, or a change adding a new abstraction or dependency | Forces the laziest solution that works: YAGNI, stdlib/native first, shortest diff. Levels: lite, full, ultra; lasts for the task |

### Tools & Integrations

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:summarize` | **User-invoked: `/skill:summarize`**, to summarize a URL/article or transcribe a YouTube/video | `summarize` CLI helper for URLs, podcasts, local files, and best-effort transcript extraction. Routed through OpenCode |

## Deliberately not vendored

### superpowers — the orchestration half

`obra/superpowers` ships 14 skills. Four are here; the rest are skipped, and
most of them for one reason: **they are an orchestration stack, and `mu` is the
orchestrator on this machine.**

| Skipped | Why |
|---------|-----|
| `subagent-driven-development` (503 lines) | Fresh-subagent-per-task with two-stage review gates. That is exactly `mu`'s `implement → review → address → ship` DAG, but expressed as prose the agent has to hand-execute rather than a tool with a real task graph, workspaces, and cherry-pick flow |
| `dispatching-parallel-agents` | Parallel fanout over independent tasks — `mu`'s parallel tracks, with automatic diamond-merge |
| `requesting-code-review` | A reviewer-subagent prompt template. `mu` spawns `reviewer-N` roles directly |
| `executing-plans` | Superseded by `brainstorm`'s handoff to mu's `recipes/plan.md`. Its own text says to prefer `subagent-driven-development` when subagents exist |
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
| Watchdog-style reminder prompts | — | **Partly taken** (2026-09). A pattern where a cheap side agent watches the main one and can inject a single corrective note. Taken as three backports rather than a vendored skill (zen #5), each at the rung the enforcement ladder gives it: oracle-independence and read-only verification are judgment, so prose in `verification-before-completion`; handback-vs-stalling and *Latest user intent wins* are judgment about a dialogue, so prose in `brainstorm`; the bypassed-refusal rule is two observable facts, so it is code in `pi/.pi/agent/extensions/refusal.ts`. `goal.ts` already was Muse's goal-reminder, arrived at independently — `agent_end`, a bounded transcript, a fast model with no tools, schema-forced verdict, host-authored note. **The architecture is skipped**: it is orchestration (zen #3 → `mu`), pi has no such primitive (zen #2), and re-sending the conversation on every check is expensive enough to dominate a session's token cost. One detail worth keeping if `mu` ever grows a watchdog: the watcher should not author the note text — a single fixed note cannot stack into noise, and a wrong reminder teaches the agent to ignore the next one |
| Context-packing plugin | — | Skipped. Context infrastructure, not discipline (zen #1): per-turn relevance+novelty scoring packs the window to a budget instead of waiting for compaction, parking bulk to disk with byte-exact recall. Reportedly cuts billed tokens while improving recall of detail mentioned once in a long session. Needs a message-transform hook (opencode's `experimental.chat.messages.transform`) that pi does not expose — zen #2. Revisit if pi gains one |
| [`mattpocock/skills`](https://github.com/mattpocock/skills) `grill-me` / `grilling` | ~200k repo | **Partly taken**, twice. First pass took the recommended-answer-per-question and answer-from-the-repo refinements. Re-checked @ `84fdeff`: `grilling` had since become a **design tree** worked in **rounds** — ask the whole *frontier* (decisions whose prerequisites are settled) at once, recompute, repeat, done when the frontier is empty. That replaced `brainstorm`'s strict one-question-per-message rule, which was paying round-trips to re-derive an ordering the tree already encodes. `grill-me` and `grill-with-docs` are thin aliases into it |
| `mattpocock/skills` `handoff` | — | Skipped. Pi ships a `handoff` **extension** (`examples/extensions/handoff.ts`) that forks a real session — strictly better than a skill that writes a markdown file |
| `mattpocock/skills` main flow (`to-spec`, `to-tickets`, `implement`, `triage`, `wayfinder`) | — | Skipped. An issue-tracker-shaped pipeline needing `setup-matt-pocock-skills` per repo. Overlaps `brainstorm` → `mu` and assumes GitHub/Linear |
| `superpowers` `writing-skills` | — | Skipped. 679 lines for something done a few times a year — read it from `~/hacking/superpowers/` when actually writing one. (mattpocock's `writing-great-skills` was rejected alongside it on the same size grounds; it has since been rewritten to 81 lines as `writing-for-agents` and is now **vendored** — see the table above) |
| `mattpocock/skills` `diagnosing-bugs`, `tdd`, `code-review` | — | **Partly taken.** Three backports rather than three competing skills (zen #4): the tight-feedback-loop Phase 1 and ranked-falsifiable-hypotheses fix into `systematic-debugging`, pre-agreed seams and the horizontal-slicing anti-pattern into `test-driven-development`, and the Fowler smell baseline into `code-reviewer`. Skipped from `code-review`: the Standards/Spec two-axis split and its parallel sub-agents — no issue tracker here, and the fan-out is `mu`'s |
| `mattpocock/skills` `codebase-design`, `improve-codebase-architecture`, `prototype`, `wizard`, `teach`, `research`, `resolving-merge-conflicts` | — | Skipped. Capability, not discipline (zen #1). `research` dispatches a background agent (`mu`'s job); `improve-codebase-architecture` renders a Tailwind/Mermaid HTML report and depends on a `CONTEXT.md` this repo doesn't keep; `resolving-merge-conflicts` is 14 lines of git-only flow in a jj-first repo |
| `code-simplifier`, `pr-review-expert`, `tech-debt-tracker` | various | Skipped. `code-reviewer` + `ponytail` cover this ground and are actually used |
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
| `/eli5`, `/tldr`, `/no-fluff`, `/talk-normal` as prompt macros | Skipped, and they are the anti-pattern. All name the **output**, so the model over-corrects into a caveman register: shorter and no clearer. Deliberate compression had its own skill, `caveman`, until it was removed in 2026-10 (see *Removed*) |

### ponytail — the sibling skills

Upstream `ponytail` ships six skills. Only the mode skill itself is here:

| Skipped | Why |
|---------|-----|
| `ponytail-review`, `ponytail-audit` | `code-reviewer` §3 already covers the same ground (stdlib/native-first, single-implementation abstractions, speculative flexibility) and cross-references `ponytail` as its build-time mirror. Two skills competing for "review this" splits the trigger for no gain |
| `ponytail-debt` | Harvests `ponytail:` comments into a ledger. This repo has **zero** such markers, and the skill's whole mechanism is one grep: `grep -rnE '(#\|//) ?ponytail:' .`. Revisit if the markers ever accumulate |
| `ponytail-gain` | Prints upstream's benchmark medians as an ASCII scoreboard. No local signal |
| `ponytail-help` | Reference card for `/ponytail-*` slash commands and a Claude Code plugin auto-update flow, neither of which exists here |

The `ponytail:` comment convention itself lives in the main skill and stays.

### caveman — the compression siblings

Upstream `JuliusBrussee/caveman` also ships `ultracave` (upstream measures 35% fewer tokens vs 3% for `caveman`) and `megacave`. Neither was vendored, and since `caveman` itself was removed (see below) they are not coming back unless compression is actually wanted; `ultracave` as a user-invoked skill would be the candidate.

## Removed

- **`execute-plan`** (2026-08) — never invoked in any recorded session, and its
  batching section assumed a `Task` subagent tool pi doesn't have. `brainstorm`
  now hands the approved spec to mu's `recipes/plan.md`.
- **`tmux`** (2026-08) — driving agents in tmux panes is `mu`'s job now, and the
  remaining niche (interactive REPL/debugger scraping) had zero recorded uses.
  Recoverable from git history if the REPL case comes back.
- **`caveman`** (2026-10) — no real use in the museum archive, and the only
  other user had removed it (panel H3, `~/.local/share/skills-panel-2026-10-07/VERDICTS.md`).
- **`write-plan`** (2026-10) — never requested by a user; its loads were
  `brainstorm` handoffs and worker reflex. `brainstorm` now hands off to mu's
  `recipes/plan.md` (panel H3).
- **`test-reviewer`** (2026-10) — merged into `code-reviewer`'s Tests section;
  the two loaded together in 1761/1813 sessions (panel H3).
