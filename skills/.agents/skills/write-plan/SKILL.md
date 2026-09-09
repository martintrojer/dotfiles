---
name: write-plan
description: Use when you have an approved spec or clear requirements for a multi-step change, before touching code. Turns a design into a `mu` task DAG of self-contained, independently verifiable tasks (a markdown file only when the user asks for one). Triggers on "write a plan", "create an implementation plan", "plan this feature".
version: 0.3.0
---

# Write Plan — Planning Phase

Write each task for an engineer who is competent but has zero context for this
codebase: they don't know the toolset, the domain, or where anything lives.
Everything they need is in the task or they can't do it — and under `mu` that is
literal, since a worker sees only its own task.

That reader is also the realistic case for *you*, a week or a context-compaction
later.

## Prerequisites

An approved spec or clear requirements. If the requirements are still fuzzy, stop
and use `brainstorm` first — planning a design nobody agreed to wastes both
passes.

## Where the plan goes

**A `mu` task DAG, not a file.** Each plan task becomes a mu task whose notes
carry the full content — files, steps, code, verification. That is the artifact;
there is no markdown to keep in sync with it.

Write `docs/plans/YYYY-MM-DD-<feature-name>.md` **only when the user explicitly
asks for a file** ("write it to a file", "save the plan", "give me a markdown
plan"). Then do the file *instead of* the DAG unless they want both.

Everything below — structure, task sizing, no placeholders — applies either way.
A task's note is the plan section for that task, verbatim.

## Scope check

If the spec covers several independent subsystems, suggest splitting into one
plan per subsystem. Each plan should produce working, testable software on its
own.

## File structure first

Before writing tasks, map which files get created or modified and what each is
responsible for. This is where the decomposition decisions actually get made —
task boundaries fall out of it.

- One clear responsibility per file. Files that change together live together.
- Split by responsibility, not by technical layer.
- Follow the codebase's existing structure. Don't unilaterally restructure — but
  if a file you're already modifying has grown unwieldy, a split is fair to
  include.

## Task right-sizing

A task is the smallest unit that carries its own verification and is worth a
reviewer's gate.

- Fold setup, config, scaffolding, and docs into the task whose deliverable
  needs them — they are not their own tasks.
- Split only where a reviewer could sensibly reject one task while approving its
  neighbour.
- Every task ends with something independently testable.

Steps within a task are one action each, 2–5 minutes: "write the failing test",
"run it, confirm it fails", "implement", "run it, confirm it passes", "commit".

## Plan header

The header is shared context every task needs. In the DAG it goes on a root
task that all others are blocked by; in a file it goes at the top.

```markdown
# <Feature Name> Implementation Plan

**Date:** YYYY-MM-DD
**Spec:** <link or path>

**Goal:** <one sentence — what this builds>

**Architecture:** <2-3 sentences on approach>

## Global Constraints

<Project-wide requirements — version floors, dependency limits, naming rules,
platform requirements. One line each, exact values copied verbatim from the
spec. Every task implicitly includes these.>

---
```

## Task structure

This is the content every task carries, whichever artifact you produce. Shown as
markdown; in the DAG the same content becomes the task's note (see *Building the
DAG*).

````markdown
### Task N: <Component Name>

**Files:**
- Create: `exact/path/to/file.py`
- Modify: `exact/path/to/existing.py:123-145`
- Test: `tests/exact/path/to/test.py`

**Interfaces:**
- Consumes: <what this uses from earlier tasks — exact signatures>
- Produces: <what later tasks rely on — exact names, parameter and return
  types. The implementer sees only their own task; this block is how they
  learn the names their neighbours use.>

- [ ] **Step 1: Write the failing test**

```python
def test_specific_behavior():
    assert function(input) == expected
```

- [ ] **Step 2: Run it, confirm it fails**

Run: `pytest tests/path/test.py::test_name -v`
Expected: FAIL, "function not defined"

- [ ] **Step 3: Minimal implementation**

```python
def function(input):
    return expected
```

- [ ] **Step 4: Run it, confirm it passes**

Run: `pytest tests/path/test.py::test_name -v`
Expected: PASS

- [ ] **Step 5: Commit**

`<conventional commit message>`
````

## Standards to enforce

**TDD** — red, green, refactor. The "run it and watch it fail" step is not
ceremony: a test that has never failed has never been shown to test anything.

**DRY / YAGNI** — flag duplication as it emerges and include the refactoring
task. Equally, cut speculative tasks: a task nobody asked for is a task that
gets built, reviewed, and maintained for nothing.

**Frequent commits** — one per task, atomic and revertible.

## No placeholders

Every step contains the actual content the engineer needs. These are plan
failures, not shorthand:

- "TBD", "TODO", "implement later", "fill in details"
- "Add appropriate error handling" / "handle edge cases" — say which, and how
- "Write tests for the above" without the test code
- "Similar to Task N" — repeat it; tasks get read out of order
- Steps describing *what* without showing *how* (code steps need code blocks)
- References to types or functions no task defines

## Self-review

After the tasks are written — before creating them in mu — check them against the
spec with fresh eyes. This is your own checklist, not a review request. Doing it
first is cheaper: a note is created once, and fixing a bad one means another
note explaining why the first was wrong.

1. **Spec coverage** — walk each spec requirement. Can you name the task that
   implements it? List gaps, then close them.
2. **Placeholder scan** — search for every red flag above. Fix.
3. **Type consistency** — do names and signatures used in later tasks match what
   earlier tasks defined? `clearLayers()` in Task 3 and `clearFullLayers()` in
   Task 7 is a bug you're shipping into the plan.

Fix inline and move on.

## Progress tracker

In the DAG this is free: `mu state -w <ws>` and `mu task tree` are the tracker,
task notes are the mid-flight record. Don't build a second one.

In a file, end with:

```markdown
## Progress

- [ ] Task 1: <description>
- [ ] Task 2: <description>

## Notes
<Context for resuming later — decisions made mid-flight, things that surprised
you.>
```

## Building the DAG

**Read the `mu` skill in full first** (once per session) — it owns the task and
edge semantics this section relies on, and `mu --help` / `mu task add --help`
override both if they disagree. Don't guess flag names from the example below.

Two things to create: **tasks** (`mu task add`, one per plan task) and **edges**
(the `blocks` relation — the only edge type there is).

The note is the task section written out in full — a worker with no other
context must be able to work from `mu task notes <id>` alone. Use a quoted
heredoc so code blocks and `$VAR` survive.

```bash
mu workstream init <feature-name>

mu task add task_0 -w <ws> -t '<Feature>: constraints and architecture' \
  -i 80 -e 0.1 --note "$(cat <<'EOF'
GOAL: <one sentence>
ARCHITECTURE: <2-3 sentences>
CONSTRAINTS:
- <verbatim from spec>
EOF
)"

mu task add task_1 -w <ws> -t 'Task 1: <component>' -i <1-100> -e <days> \
  -b task_0 --note "$(cat <<'EOF'
FILES:
- Create: exact/path/to/file.py
- Test: tests/exact/path/to/test.py
INTERFACES:
- Consumes: <exact signatures>
- Produces: <exact names, param and return types>
STEPS:
1. Write failing test:
   <the actual test code>
2. Run `pytest tests/... -v` — expect FAIL "function not defined"
3. Implement:
   <the actual implementation>
4. Run `pytest tests/... -v` — expect PASS
5. Commit: <conventional commit message>
VERIFY: pytest tests/... -v
EOF
)"
```

### Edges

`-b/--blocked-by` at creation time is the normal path: the blocker already
exists, because you create tasks in dependency order. It takes several ids
(`-b task_1,task_2` or `-b task_1 -b task_2`).

For an edge you can't express at creation — a blocker created later, or one you
realise during self-review — add it after the fact:

```bash
mu task block task_5 -w <ws> --by task_3      # task_3 blocks task_5
mu task unblock task_5 -w <ws> --by task_3    # wrong edge, remove it
mu task tree -w <ws>                          # read the graph back
```

Direction trips people up: the **first** id is the one that's blocked, `--by`
names the blocker. `mu task block A --by B` means A waits for B.

- **Dependencies, not sequence.** Add an edge only where Task N *consumes* what
  Task M *produces*. Plan order is presentation; over-linking serialises tracks
  mu could have run in parallel, which is the whole reason to use the DAG.
- Verify with `mu task tree` before handing off. A missing edge means a worker
  starts on a foundation that doesn't exist yet; a spurious one idles an agent.

### Sizing and gates

- `effort_days` from step count, `impact` from what breaks without it. Guess
  honestly rather than defaulting everything to 50/1.
- Review gates: the reviewer is its own task blocked by the work it reviews, and
  the fix task is blocked by the review.
- Task 0 exists so constraints live in one place. Block real work on it and
  close it immediately — it's a note carrier, not work.
- One note per task at creation. Follow-ups go in later notes; don't rewrite
  history, append to it.

## Handoff

Run `mu state -w <ws>`, report the workstream name and the parallel tracks, and
hand back. Execution is a separate pass with fresh context — spawning workers is
not this pass's job.

When reality disagrees with the plan, the plan is what updates: a task note, not
a silent improvisation. A plan that survives contact unchanged is rare.

## Related

| Skill | When |
|-------|------|
| `brainstorm` | Upstream — produces the spec this plan consumes. Go back if requirements are still fuzzy |
| `test-driven-development` | Writing the per-task red/green steps |
| `ponytail` | Sizing tasks. A task nobody asked for still gets built, reviewed, and maintained |
| `mu` | Loading the plan into a task DAG, then executing it across agents |
