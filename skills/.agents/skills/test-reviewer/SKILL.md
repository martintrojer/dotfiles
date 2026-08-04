---
name: test-reviewer
description: Use this skill to review tests for false confidence, excessive mocking, meaningless assertions, and weak behavior coverage. Use after writing tests or when a suite passes but bugs still escape.
version: 0.1.0
---

# Test Reviewer

## Purpose

Evaluate whether tests provide real safety signals or merely appear to do so.

## What to Look For

### 1. Excessive Mocking
- Mocks outnumber real objects
- Mock setup mirrors production logic
- Tests pass regardless of implementation correctness
- Mock verification replaces behavior verification

### 2. Fake Testing
- Assertions recreate implementation instead of validating outcomes
- Tests verify that mocks were called, not user-visible behavior
- Tests compare a value to itself or otherwise tautological checks
- Tests that execute code with no meaningful assertion

### 3. Weak Assertions
- Always-true or trivial assertions
- Missing assertions
- Hardcoded expected values that do not constrain behavior
- Assertions that would still pass for broken logic
- **Mirror assertions** — the expected value computed by the same code under
  test, so the check passes whatever that code does
- **Change detectors** — only an intentional decision can fail them (a
  constant's value, exact message wording), so they fire on redesign and sleep
  through bugs

### 4. Test Smells and Coverage Quality
- Excessive irrelevant setup
- Speculative scaffolding, redundant fixtures, or reinvented stdlib bloating the suite
- Brittle tests tightly coupled to implementation details
- Order-dependent or global-state-dependent tests
- Critical paths and edge cases untested

When trimming bloat, keep a single smoke / `assert` self-check as the minimum —
never delete the last line that fails when the logic breaks.

## Review Process

1. Check if tests fail when behavior is broken
2. Evaluate whether mocks are necessary and justified
3. Confirm test independence and deterministic setup
4. Identify gaps in behavior coverage and edge cases
5. Recommend stronger, behavior-focused rewrites

**The mutation check** is the sharpest tool here. Mentally mutate the
production code — wrong constant, wrong branch, missing side effect, empty
return, dropped validation — and confirm at least one test fails for each. A
mutation nothing catches marks the behavior as unprotected, or the test as
tautological.

## Output Format

Use this structure:

### Critical Issues
Tests that provide false confidence.

### Recommended Changes
High-impact rewrites that improve confidence.

### Suggestions
Minor improvements for clarity and maintainability.

For each finding:
- Quote the problematic test code
- Explain what bug could slip through
- Provide a concrete improved test
- State what behavior the improved test verifies

## Related

This skill is review-time triage: given tests that exist, decide which ones lie.

| Skill | When |
|-------|------|
| `test-driven-development` (`writing-good-tests.md`) | Write-time counterpart — the same failure modes as rules to follow while writing. Read it when rewriting a test this skill flagged |
| `ponytail` | Sizing: the smallest test that catches the bug, no fixtures nobody needs |
| `code-reviewer` | The production code in the same diff |
| `systematic-debugging` | A test fails and you don't know why yet |

## Principles

- A test must fail when behavior is wrong
- Prefer behavior assertions over implementation assertions
- Use real dependencies when practical
- One test should validate one behavior
- Passing tests should correlate with working software
