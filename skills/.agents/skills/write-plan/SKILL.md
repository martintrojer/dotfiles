---
name: write-plan
description: Use when you have an approved spec or clear requirements for a multi-step change, before touching code. Turns a design into an ordered set of self-contained, independently verifiable tasks. Triggers on "write a plan", "create an implementation plan", "plan this feature".
version: 0.2.0
---

# Write Plan — Planning Phase

Write the plan for an engineer who is competent but has zero context for this
codebase: they don't know the toolset, the domain, or where anything lives.
Everything they need is in the plan or they can't do the task.

That reader is also the realistic case for *you*, a week or a context-compaction
later.

## Prerequisites

An approved spec or clear requirements. If the requirements are still fuzzy, stop
and use `brainstorm` first — planning a design nobody agreed to wastes both
passes.

## Where plans live

`docs/plans/YYYY-MM-DD-<feature-name>.md` (user preference overrides). Create the
directory if it doesn't exist.

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

After the plan is written, check it against the spec with fresh eyes. This is
your own checklist, not a review request.

1. **Spec coverage** — walk each spec requirement. Can you name the task that
   implements it? List gaps, then close them.
2. **Placeholder scan** — search for every red flag above. Fix.
3. **Type consistency** — do names and signatures used in later tasks match what
   earlier tasks defined? `clearLayers()` in Task 3 and `clearFullLayers()` in
   Task 7 is a bug you're shipping into the plan.

Fix inline and move on.

## Progress tracker

End the plan with:

```markdown
## Progress

- [ ] Task 1: <description>
- [ ] Task 2: <description>

## Notes
<Context for resuming later — decisions made mid-flight, things that surprised
you.>
```

## Handoff

Summarise the plan, confirm where it's saved, and hand back. Execution is a
separate pass with fresh context — work through the tasks in order, tick the
boxes as they land, and stop rather than guess when a task turns out to be
wrong. A plan that survives contact unchanged is rare; when reality disagrees
with the plan, the plan is what updates.

For a plan with independent tracks, or one you want review-gated per task,
hand it to `mu` rather than grinding through it in one context.

## Related

| Skill | When |
|-------|------|
| `brainstorm` | Upstream — produces the spec this plan consumes. Go back if requirements are still fuzzy |
| `test-driven-development` | Writing the per-task red/green steps |
| `ponytail` | Sizing tasks. A task nobody asked for still gets built, reviewed, and maintained |
| `mu` | Executing a multi-track or review-gated plan across agents |
