---
name: verification-before-completion
description: Use when about to claim work is complete, fixed, or passing, before committing or creating PRs - requires running verification commands and confirming output before making any success claims; evidence before assertions always
---

# Verification Before Completion

<!-- local: ritual sections cut, gate kept (skills panel 2026-10-07 H2). -->

**Core principle:** No completion claim without fresh verification evidence.

## The Gate Function

<!-- local: RUN freshness replaces upstream "run in this message". -->

```
BEFORE claiming any status:

1. IDENTIFY: What command proves this claim?
2. RUN: The FULL command, fresh for the artifact as it is now:
   rerun after any change; a recent run of an unchanged artifact counts.
3. READ: Full output, exit code, failures
4. VERIFY: Does output confirm the claim? Report actual status + evidence.
5. ONLY THEN: Make the claim
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

<!-- local: last three rows added. -->

## Could the Check Have Failed?

<!-- local addition. -->

A check that cannot fail is not evidence.

- **A self-built oracle is not a run.** Your own script, or a reference configured like the artifact, re-encodes your reading. Use the repo's tests, a golden file, an external source, or a second method.
- **Read the result.** A nonzero `diff` or missed tolerance fails even at exit 0.
- **Leave the conditions you control.** Run fresh from the real path on an input you did not tune on.
- **Proxy evidence is not observation.** Read the image back; make the request and read the body.
- **Verification is read-only.** If verifying forces a change, restart from the first check.
- **Exercise each item, not the family.** "All endpoints" is many claims; drive each.

## Prove the Safety Invariant

<!-- local addition: distilled from pstack's blast-radius skill (MIT). -->

For a risky change reaching beyond the diff, name the one fact its safety depends on. Push it down this ladder as far as practical:

1. Point to the exact source or contract.
2. Walk the failure path and show why it cannot reach.
3. Run the real code in a focused script or test.
4. Exercise the path in the running artifact.

Report the rung reached; if unobservable, label it `unproven`.

## Related

<!-- local addition -->

| Skill | When |
|-------|------|
| `commit` | The Test Plan makes an unverified claim permanent |
| `test-driven-development` | "The test passes" needs a watched failure first |
| `systematic-debugging` | "Bug fixed" needs the original symptom retested |
| `code-reviewer` (its Tests section) | The check passed but you suspect it could not fail |
| `refusal` extension (`pi/.pi/agent/extensions/refusal.ts`) | Catches bypassing a refused gate |
