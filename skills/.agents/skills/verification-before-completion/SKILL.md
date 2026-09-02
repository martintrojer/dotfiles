---
name: verification-before-completion
description: Use when about to claim work is complete, fixed, or passing, before committing or creating PRs - requires running verification commands and confirming output before making any success claims; evidence before assertions always
---

# Verification Before Completion

## Overview

**Core principle:** Evidence before claims, always.

**Violating the letter of this rule is violating the spirit of this rule.**

## The Iron Law

```
NO COMPLETION CLAIMS WITHOUT FRESH VERIFICATION EVIDENCE
```

If you haven't run the verification command in this message, you cannot claim it passes.

## The Gate Function

```
BEFORE claiming any status or expressing satisfaction:

1. IDENTIFY: What command proves this claim?
2. RUN: Execute the FULL command (fresh, complete)
3. READ: Full output, check exit code, count failures
4. VERIFY: Does output confirm the claim?
   - If NO: State actual status with evidence
   - If YES: State claim WITH evidence
5. ONLY THEN: Make the claim

Skip any step = lying, not verifying
```

## Common Failures

| Claim | Requires | Not Sufficient |
|-------|----------|----------------|
| Tests pass | Test command output: 0 failures | Previous run, "should pass" |
| Linter clean | Linter output: 0 errors | Partial check, extrapolation |
| Build succeeds | Build command: exit 0 | Linter passing, logs look good |
| Bug fixed | Test original symptom: passes | Code changed, assumed fixed |
| Regression test works | Red-green cycle verified | Test passes once |
| Agent completed | VCS diff shows changes | Agent reports "success" |
| Requirements met | Line-by-line checklist | Tests passing |
| Output is correct | Comparison against something you did not author | Your own script agreeing with itself |
| Endpoint serves | A real request and its response body | A `LISTEN` line, an open socket |
| Artifact renders | The image read back | A screenshot saved but never opened |
| Check satisfied | The check passing | The check skipped, forced, or disabled |

## Red Flags - STOP

- Using "should", "probably", "seems to"
- Expressing satisfaction before verification ("Great!", "Perfect!", "Done!", etc.)
- About to commit/push/PR without verification
- Trusting agent success reports
- Relying on partial verification
- Thinking "just this once"
- Tired and wanting work over
- Reaching for `--force`, `--no-verify`, `--skip`, or a `dangerously-` flag after something refused
- Comparing output against a reference you configured the same way as the artifact
- **ANY wording implying success without having run verification**

## Rationalization Prevention

| Excuse | Reality |
|--------|---------|
| "Should work now" | RUN the verification |
| "I'm confident" | Confidence ≠ evidence |
| "Just this once" | No exceptions |
| "Linter passed" | Linter ≠ compiler |
| "Agent said success" | Verify independently |
| "I'm tired" | Exhaustion ≠ excuse |
| "Partial check is enough" | Partial proves nothing |
| "Different words so rule doesn't apply" | Spirit over letter |
| "My script says it matches" | You wrote the script from the assumption under test |
| "The diff was tiny" | Nonzero is nonzero — it disagreed |
| "The check was wrong" | Report the refusal; don't route around it |
| "It worked on my fixture" | You tuned on that fixture |

## Key Patterns

**Tests:**
```
✅ [Run test command] [See: 34/34 pass] "All tests pass"
❌ "Should pass now" / "Looks correct"
```

**Regression tests (TDD Red-Green):**
```
✅ Write → Run (pass) → Revert fix → Run (MUST FAIL) → Restore → Run (pass)
❌ "I've written a regression test" (without red-green verification)
```

**Build:**
```
✅ [Run build] [See: exit 0] "Build passes"
❌ "Linter passed" (linter doesn't check compilation)
```

**Requirements:**
```
✅ Re-read plan → Create checklist → Verify each → Report gaps or completion
❌ "Tests pass, phase complete"
```

**Agent delegation:**
```
✅ Agent reports success → Check VCS diff → Verify changes → Report actual state
❌ Trust agent report
```

### Could the Check Have Failed?

<!-- local addition. -->

The gate above asks whether you ran something. This asks whether the run was
capable of telling you no. A check that cannot fail is not evidence.

- **A self-built oracle is not a run.** Re-running your own fit or script, or
  comparing against a reference you configured the same way as the artifact,
  only re-encodes your own reading of the problem. Independent means: the repo's
  own tests, a golden file, a named external source, a second method, or an
  input you did not tune on.
- **Read the result, don't just get one.** A nonzero `diff`/`cmp`, differing
  sizes, or a missed tolerance is a failure even when the command exited. The
  run that ran and disagreed is worse than no run, because it looks like
  evidence.
- **Leave the conditions you control.** Before claiming done, run a fresh
  process from the real path with tuning and helper files removed. When the task
  calls a property unknown — shape, size, scale, seed — execute once against a
  case you did not tune on.
- **Proxy evidence is not observation.** Derived statistics (pixel histograms,
  unique-colour counts, resolution) do not establish a rendered end-state, and a
  listening socket does not establish that an endpoint serves. Read the image
  back; make the request and read the body.
- **Verification is read-only with respect to the deliverable.** No commits,
  resets, `gc`, or history rewrites while verifying. If verifying forces a
  change, make the change and restart from the first check — a pass observed
  mid-mutation describes something that no longer exists.
- **Exercise each item, not the family.** "All endpoints", "the arrow keys",
  "the main flows" is one claim standing in for many. Name and drive each
  control, command, or route separately; a summary is not evidence for its
  members.

### A Refusal Is a Result

<!-- local addition. -->

When a test, linter, hook, type check, permission prompt, or review has
**refused** an action, retrying it with that check skipped, forced, bypassed, or
disabled is not verification — it is the removal of verification. Flags whose
whole purpose is to get past the refusal (`--force`, `--no-verify`, `--skip`,
`-f`, `dangerously-*`, a loosened threshold, a deleted assertion) turn a red
signal into a silent one.

This holds **even when the underlying action was clearly in scope and asked
for**. Being authorized to land the change is not authorization to stop checking
it.

Report the refusal, with its exact message, and either fix the cause or ask.
Stating "the pre-commit hook rejects this because X" is a complete, useful
answer. Bypassing it and reporting success is a false one.

### Prove the Safety Invariant

<!-- local addition: distilled from pstack's blast-radius skill (MIT). -->

For a risky change whose effects extend beyond the diff, name the one fact its
safety depends on. Push that fact as far down this evidence ladder as practical:

1. Point to the exact source or contract.
2. Walk the failure path and show why it cannot reach.
3. Run the real code in a focused script or test.
4. Exercise the path in the running artifact.

Report the rung reached. If the fact cannot be executed or observed, label it
`unproven`; do not round a plausible explanation up to verification.

## When To Apply

**ALWAYS before:**
- ANY variation of success/completion claims
- ANY expression of satisfaction
- ANY positive statement about work state
- Committing, PR creation, task completion
- Moving to next task
- Delegating to agents

**Rule applies to:**
- Exact phrases
- Paraphrases and synonyms
- Implications of success
- ANY communication suggesting completion/correctness

## Related

<!-- local addition -->

| Skill | When |
|-------|------|
| `commit` | The Test Plan field is where an unverified claim becomes permanent |
| `test-driven-development` | "The test passes" requires having watched it fail first |
| `systematic-debugging` | "Bug fixed" requires the original symptom retested, not just code changed |
| `test-reviewer` | The check ran and passed, but you suspect it could not have failed |
