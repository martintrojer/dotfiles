# Quick Workflow

Fast single-round perspective check. Use for sanity checks and quick feedback.

## Announce

Output this line before starting:

```
Running the **Quick** workflow in the **Council** skill to get fast perspectives...
```

## Prerequisites

- Topic or question to evaluate
- Optional: Custom council members

## Members

Write 4 member briefs inline (name + role + stance). See `CouncilMembers.md` for writing them and `SKILL.md` § Running the members for the execution modes.

## Execution

### Step 1: Write & Announce Quick Council

Write 4 member briefs tailored to the topic, then announce:

```markdown
## Quick Council: [Topic]

**Council Members:** [List member names with one-line roles]
**Mode:** Single round (fast perspectives)
```

### Step 2: Parallel Perspective Gathering

Launch all council members — in parallel if the harness has subagents, otherwise write each take yourself in sequence.

**Each member's input is their brief PLUS:**
```
QUICK COUNCIL CHECK

Topic: [The topic]

[Relevant context for the topic]

Give your immediate take from your specialized perspective:
- Key concern, insight, or recommendation
- 30-50 words max
- Be direct and specific

This is a quick sanity check, not a full debate.
```

### Step 3: Output Perspectives

```markdown
### Perspectives

**[Agent 1 Name] ([traits]):**
[Brief take]

**[Agent 2 Name] ([traits]):**
[Brief take]

**[Agent 3 Name] ([traits]):**
[Brief take]

**[Agent 4 Name] ([traits]):**
[Brief take]

### Quick Summary

**Consensus:** [Do they generally agree? On what?]
**Concerns:** [Any red flags raised?]
**Recommendation:** [Proceed / Reconsider / Need full debate]
```

## When to Escalate

If the quick check reveals significant disagreement or complex trade-offs, recommend:

```
This topic has enough complexity for a full council debate.
Run: "Council: [topic]" for 3-round structured discussion.
```

## Timing

- Total: 15-30 seconds with parallel subagents (single round). In-context, one response.

## Done

Quick perspectives gathered. Use for fast validation; escalate to DEBATE for complex decisions.
