---
name: systematic-debugging
description: Use when encountering any bug, test failure, or unexpected behavior, before proposing fixes
---

# Systematic Debugging

<!-- local: cut Iron Law, When to Use, Phase 2, Red Flags, user Signals, Rationalizations, Quick Reference, "No Root Cause" and defense-in-depth (skills panel 2026-10-07 H2). -->

## Overview

<!-- local: Iron Law folded into the last sentence. -->
**Core principle:** Find the root cause before attempting fixes. Symptom fixes are failure. Do not propose a fix until Phase 1 is done.

## Phase 1: Root Cause Investigation

1. **Read Error Messages Carefully**
   <!-- local: condensed. -->
   - Read warnings and stack traces completely
   - Note line numbers, file paths, error codes

2. **Build a Feedback Loop**

   <!-- local addition: adapted from mattpocock/skills `diagnosing-bugs` (MIT),
        condensed 2026-10. Upstream Phase 1 says "reproduce consistently"; it does not say to build
        the loop first, or how. -->

   **This is the phase.** With a *tight* pass/fail signal that goes red on *this* bug, bisection and instrumentation just consume it. Without one, staring at code will not save you. This is where disproportionate effort pays.

   <!-- local: was "NO HYPOTHESES WITHOUT A RED-CAPABLE LOOP FIRST". -->
   Establish the strongest practical reproduction or evidence loop before changing code.

   Ways to construct one, in roughly this order:

   1. **Failing test** at whatever seam reaches the bug
   2. **Curl / HTTP script** against a running dev server
   3. **CLI invocation** with a fixture input, diffed against a known-good snapshot
   4. **Headless browser script** driving the UI, asserting on DOM/console/network
   5. **Replay a captured trace** — a saved payload or event log, run through the code path in isolation
   6. **Throwaway harness** — minimal subset of the system, one function call
   7. **Property / fuzz loop** for "sometimes wrong output"
   8. **Bisection harness** — automate "boot at state X, check, repeat" so `git bisect run` can drive it
   9. **Differential loop** — same input through two versions or configs, diff

   **Tighten it.** Treat the loop as a product: faster (cache setup, narrow scope), sharper (assert the specific symptom, not "didn't crash"), more deterministic (pin time, seed RNG, isolate the filesystem). A 30-second flaky loop is barely better than none.

   **Non-deterministic bugs:** aim for a higher reproduction rate, not a clean repro. Loop the trigger 100×, parallelise, inject sleeps to narrow timing windows. A 50%-flake bug is debuggable; 1% is not.

   **Done when** you can name **one command** you have already run (show the invocation and its output) that is:

   - **Red-capable** — drives the real code path and asserts the *user's exact symptom*, red on this bug and green once fixed
   - **Deterministic** — same verdict every run
   - **Fast** — seconds, not minutes
   - **Agent-runnable** — you can run it unattended

   <!-- local: was "stop, ask the user, do not hypothesise without a loop". -->
   **When no loop is possible** (production-only, one-off), say so, list what you tried, and work from logs and traces.

   Once it is red, **minimise**: cut inputs, callers, config and steps one at a time, re-running after each cut, until removing anything makes it go green. A minimal repro shrinks the hypothesis space and becomes the regression test in Phase 3.

3. **Check Recent Changes**

   <!-- local: second bullet folded in from the cut Phase 2 (Pattern Analysis). -->
   - Diff, recent commits, new dependencies, config and environment changes
   - Diff the broken case against a similar working example, listing every difference

4. **Gather Evidence in Multi-Component Systems**

   <!-- local: bash example cut, recipe condensed. -->
   When the system has several components (CI → build → signing, API → service → database), instrument every boundary before proposing fixes:

   ```
   For EACH component boundary: log data entering and leaving,
   and the environment/config that propagated.
   Run once to see WHERE it breaks, then investigate that component.
   ```

5. **Trace Data Flow**

   <!-- local: condensed to prose. -->
   When the error is deep in the call stack, trace backward: where does the bad value originate, what called this with it? Keep tracing up to the source and fix there, not at the symptom. See `root-cause-tracing.md` for the full technique.

## Phase 2: Hypothesis and Testing

<!-- local: was Phase 3; Phase 2 cut. -->

1. **Form Ranked Hypotheses**

   <!-- local addition: adapted from mattpocock/skills `diagnosing-bugs` (MIT).
        Upstream says "form single hypothesis", which instructs the anchoring
        failure it should prevent. -->

   <!-- local: fixed 3-5 count and show-the-user step dropped. -->
   Generate several *before* testing any. Single-hypothesis generation anchors on the first plausible idea.

   Each must be **falsifiable** — state the prediction it makes:

   > "If X is the cause, then changing Y will make the bug disappear."

   If you can't state the prediction, the hypothesis is a vibe. Discard or sharpen it.

2. **Test Minimally**
   <!-- local: condensed. -->
   - The SMALLEST change that tests the hypothesis
   - One variable at a time

3. **Verify Before Continuing**
   <!-- local: absorbs upstream "When You Don't Know". -->
   - Worked? → Phase 3
   - Didn't? Form a NEW hypothesis; don't stack fixes
   - Don't understand X? Say so and research; don't pretend

## Phase 3: Implementation

<!-- local: was Phase 4; steps 2-3 condensed. -->
Fix the root cause, not the symptom:

1. **Create a Red Check**

   <!-- local: was "failing test case, MUST have before fixing". -->
   Use the red-capable check from Phase 1: a test when cheap, else the closest executable check (see `test-driven-development`'s selection gate). Have it before fixing.

2. **Implement Single Fix**
   - Address the identified root cause, ONE change at a time
   - No "while I'm here" improvements or bundled refactoring

3. **Verify Fix**
   - The check passes, no other tests broke, the issue is resolved
   - Use `verification-before-completion` before claiming success

<!-- local: replaces upstream "3+ failed fixes = question the architecture". -->
Repeated failed fixes mean the root-cause model is wrong; go back to Phase 1.

## Supporting Techniques

<!-- local: defense-in-depth technique removed. -->
In this directory:

- **`root-cause-tracing.md`** - Trace bugs backward through call stack to find original trigger
- **`condition-based-waiting.md`** - Replace arbitrary timeouts with condition polling

## Related

<!-- local addition -->

| Skill | When |
|-------|------|
| `verification-before-completion` | Before claiming the bug is fixed |
| `test-driven-development` | Phase 3's failing reproduction test |
| `commit` | The commit body should carry the root cause, not the symptom |
