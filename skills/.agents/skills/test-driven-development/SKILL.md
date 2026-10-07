---
name: test-driven-development
description: Use when the user explicitly asks for TDD, a failing test, or a regression test, or when fixing a bug with an obvious cheap local test target. Once selected, require a real red-green cycle before production changes. Skip a new automated test when the path is unclear, expensive, integration-heavy, or weaker than direct executable verification.
---

# Test-Driven Development (TDD)

<!-- local: cut Iron Law, "delete means delete", dot graph, Rationalizations, Red Flags, Bug Fix example, Verification Checklist, When Stuck, Final Rule (skills panel 2026-10-07 H2). -->

## Overview

<!-- local: Iron Law folded into the last sentence. -->
Write the test first. Watch it fail. Write minimal code to pass.

**Core principle:** If you didn't watch the test fail, you don't know if it tests the right thing. Once TDD is selected, no production code without a failing test first.

<!-- local addition: selection gate adapted from pstack `tdd` (MIT) @ 60c641e -->

## Select TDD First

Use this workflow when:

- The user explicitly asks for TDD, a failing test, or a regression test.
- A bug has an obvious, cheap test target already used by the affected code.

Do not force a new test when the path is unclear, expensive, integration-heavy, or mostly mocks; when it needs broad harness setup, production-only state, or unrelated fixture churn; or for generated code, configuration-only changes, and throwaway prototypes.

When TDD does not fit, state why before editing production code and choose the closest executable check: a focused script, an existing integration check, a manual reproduction command, browser automation, a snapshot comparison, or a runtime assertion. Prefer no new test over a test that cannot catch the bug.

## Seams - Where Tests Go

<!-- local addition: adapted from mattpocock/skills `tdd` (MIT), condensed 2026-10. Upstream covers
     what a good test is, but never where it goes or who agrees to that. -->

A **seam** is the public boundary you test at: the interface where you can observe behavior without reaching inside. Tests live at seams, never against internals.

**Test at an agreed seam.** Name the seam before writing the test. Prefer the public boundary and test shape the repository already uses. Ask the user only when choosing a seam would decide product or API direction; otherwise use the existing convention and proceed.

Naming the seam up front puts testing effort on the critical paths and complex logic instead of spreading it over every edge case. Ask: "what's the public interface, and which seam would catch this behavior?"

When the shape of that interface is itself the open question, `brainstorm` it before writing tests against a boundary you don't believe in.

## Red-Green-Refactor

### RED - Write Failing Test

<!-- local: requirements list folded into this sentence. -->
Write one minimal test showing what should happen: one behavior, a name that describes it, real code (no mocks unless unavoidable).

<!-- local: upstream's Good/Bad code pairs condensed to one line each. -->
Good: `test('retries failed operations 3 times')` calls the real `retryOperation` and asserts the result and the attempt count. Bad: `test('retry works')` asserts that a `jest.fn()` mock was called three times; it tests the mock, not the code.

### Verify RED - Watch It Fail

<!-- local: condensed; npm command dropped. -->
**MANDATORY. Never skip.** Run the test and confirm:

- It fails (not errors)
- The failure message is the expected one
- It fails because the feature is missing (not a typo)

**Test passes?** You're testing existing behavior. Fix the test.

**Test errors?** Fix the error, re-run until it fails correctly.

### GREEN - Minimal Code

<!-- local: code examples replaced by this sentence. -->
Write the simplest code that passes the test. No options, features, refactors, or "improvements" beyond it (YAGNI).

### Verify GREEN - Watch It Pass

<!-- local: condensed; project's-suite rule below is a local addition. -->
**MANDATORY.** Confirm:

- The test passes
- Other tests still pass
- Output pristine (no errors, warnings)

**Test fails?** Fix code, not test. **Other tests fail?** Fix now.

**"Other tests" means the project's suite, not just your file.** A green run of the test you wrote is not a green suite. Before you call the change done, run the project's test command (bare `pytest`, `npm test`, `cargo test` — whatever the repo uses) even when your task named only one test file. A scope statement in your task bounds the deliverable, not your verification. Any failure that run shows — including one you didn't cause — goes in your report by name; a red test you watched scroll past and didn't mention is a report falsified by omission.

### REFACTOR - Clean Up

<!-- local: list folded to one line. -->
After green only: remove duplication, improve names, extract helpers. Keep tests green. Don't add behavior.

### Repeat

<!-- local addition: vertical slices, from mattpocock/skills `tdd` (MIT). -->
**One vertical slice at a time.** One seam, one test, one minimal implementation, then repeat. Each test is a tracer bullet that responds to what the last cycle taught you.

<!-- local addition: horizontal slicing, from mattpocock/skills `tdd` (MIT) -->

The failure mode is **horizontal slicing** — writing all the tests first, then all the implementation. Bulk tests verify *imagined* behavior: you test the shape of things rather than what a user does, the tests go insensitive to real changes, and you commit to a test structure before understanding the implementation. Writing the whole suite up front is not "extra thorough TDD"; it is tests-after with the order swapped.

## Good Tests

<!-- local: quality table dropped. -->
When writing or changing any test, read [writing-good-tests.md](writing-good-tests.md) for the rules that keep tests honest:

- Name the production change that would make the test fail — before writing it
- Assert on real behavior, never on mock behavior
- Keep test-only code in test utilities, out of production classes
- Understand a dependency's side effects before mocking it

## Debugging Integration

<!-- local: upstream said "never fix bugs without a test". -->
When a bug has a cheap executable test path, write the failing reproduction and follow the TDD cycle; otherwise record why and use the closest executable regression check. Do not claim TDD when the red step did not happen.

## Related

<!-- local addition -->

| Skill | When |
|-------|------|
| `writing-good-tests.md` (this directory) | Writing or changing any test — mocking depth, mirror assertions, the mutation check |
| `test-reviewer` | Review-time counterpart: given tests that exist, find the ones that lie |
