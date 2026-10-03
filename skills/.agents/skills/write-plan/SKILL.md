---
name: write-plan
description: Use when you have an approved spec or clear requirements for a multi-step change, before touching code. Turns a design into a `mu` task DAG of self-contained, independently verifiable tasks (a markdown file only when the user asks for one). Triggers on "write a plan", "create an implementation plan", "plan this feature".
version: 0.4.0
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

**A `mu` task DAG, not a file.** Follow mu's planning recipe,
`~/.agents/skills/mu/recipes/plan.md`. It owns the mechanics: file map,
task sizing, `task_0`, the note shape with INTERFACES, no placeholders,
edges only for real dependencies, and the self-review. Read it in full
before you create any task. Its sibling `recipes/brief.md` says how to
write a note a zero-context worker can finish from.

Write `docs/plans/YYYY-MM-DD-<feature-name>.md` **only when the user explicitly
asks for a file** ("write it to a file", "save the plan", "give me a markdown
plan"). Then do the file *instead of* the DAG unless they want both. The
recipe's rules apply to the file unchanged: one section per task, same
content as the note would carry.

This skill adds what the recipe leaves to you: the scope check, the plan
header, the TDD step structure, and the standards below.

## Scope check

If the spec covers several independent subsystems, suggest splitting into one
plan per subsystem. Each plan should produce working, testable software on its
own.

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

The steps inside each task follow red-green, one action each, 2–5 minutes. In
the DAG this is the note's `STEPS:` block; in a file it is the task section.

````markdown
### Task N: <Component Name>

**Files:** Create `exact/path/to/file.py`; Test `tests/exact/path/to/test.py`
**Interfaces:** Consumes / Produces, with exact signatures

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

## Progress tracker

In the DAG this is free: `mu state -w <ws>` is the tracker and task notes are
the mid-flight record. In a file, end with:

```markdown
## Progress

- [ ] Task 1: <description>
- [ ] Task 2: <description>

## Notes
<Context for resuming later — decisions made mid-flight, things that surprised
you.>
```

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
| `mu` | `recipes/plan.md` builds the DAG; `recipes/ultrathink.md` executes a large plan with reviews |
