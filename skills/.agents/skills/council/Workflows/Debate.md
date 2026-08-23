# Debate Workflow

Full structured multi-agent debate with 3 rounds and visible transcript.

## Announce

Output this line before starting:

```
Running the **Debate** workflow in the **Council** skill to run multi-agent debate...
```

## Prerequisites

- Topic or question to debate
- Optional: Custom council member descriptions (otherwise auto-composed)

## Members

Council members are custom personas you write inline. Write four different briefs tailored to the topic — a persona-less agent produces bland agreement. See `../CouncilMembers.md` for writing them and `../SKILL.md` § Running the members for the execution modes.

Below, "launch N members" means: dispatch N subagents if the harness has them, otherwise write each member's section yourself in sequence.

## Execution

### Step 0: Write the Council Members

Before any debate rounds, analyze the topic, decide the 4 perspectives that create the most productive friction, and write a brief for each: a name, their role/expertise, the stance they hold, and what they'll push on. No tool call — you write these directly. See `../CouncilMembers.md` for the slot guidance and an example brief.

### Step 1: Announce the Council

Output the debate header with the member names:

```markdown
## Council Debate: [Topic]

**Council Members:** [List member names with their one-line role descriptions]
**Rounds:** 3 (Positions -> Responses -> Synthesis)
```

### Step 2: Round 1 - Initial Positions

Launch 4 members (one per composed council brief).

**Each member's input is their brief PLUS:**
```
COUNCIL DEBATE - ROUND 1: INITIAL POSITIONS

Topic: [The topic being debated]

[Full topic context — include relevant background, data, quotes, etc. that the agent needs to form an informed opinion]

Give your initial position on this topic from your specialized perspective.
- Speak in first person as your character
- Be specific and substantive (100-150 words)
- State your key concern, recommendation, or insight
- You'll respond to other council members in Round 2
```

**Output each response as it completes:**
```markdown
### Round 1: Initial Positions

**[Agent 1 Name] ([trait description]):**
[Response]

**[Agent 2 Name] ([trait description]):**
[Response]

**[Agent 3 Name] ([trait description]):**
[Response]

**[Agent 4 Name] ([trait description]):**
[Response]
```

### Step 3: Round 2 - Responses & Challenges

Launch 4 members with the Round 1 transcript included.

**Each member's input is their brief PLUS:**
```
COUNCIL DEBATE - ROUND 2: RESPONSES & CHALLENGES

Topic: [The topic being debated]

Here's what the council said in Round 1:
[Full Round 1 transcript]

Now respond to the other council members:
- Reference specific points they made ("I disagree with [Name]'s point about X...")
- Challenge assumptions or add nuance
- Build on points you agree with
- Maintain your specialized perspective
- 100-150 words

The value is in genuine intellectual friction -- engage with their actual arguments.
```

### Step 4: Round 3 - Synthesis

Launch 4 members with the Round 1 + Round 2 transcripts.

**Each member's input is their brief PLUS:**
```
COUNCIL DEBATE - ROUND 3: SYNTHESIS

Topic: [The topic being debated]

Full debate transcript so far:
[Round 1 + Round 2 transcripts]

Final synthesis from your perspective:
- Where does the council agree?
- Where do you still disagree with others?
- What's your final recommendation given the full discussion?
- 100-150 words

Be honest about remaining disagreements -- forced consensus is worse than acknowledged tension.
```

### Step 5: Council Synthesis

After all rounds complete, synthesize the debate:

```markdown
### Council Synthesis

**Areas of Convergence:**
- [Points where 3+ members agreed]
- [Shared concerns or recommendations]

**Remaining Disagreements:**
- [Points still contested between members]
- [Trade-offs that couldn't be resolved]

**Recommended Path:**
[Based on convergence and weight of arguments, the recommended approach is...]
```

If the debate ran in-context, add one line noting it — a convergence you
reached by simulating four members is weaker evidence than four independent
agents landing in the same place, and the reader should know which they got.

## Timing

With parallel subagents:

- Writing member briefs: inline (orchestrator writes 4 briefs)
- Round 1: ~10-20 seconds (parallel)
- Round 2: ~10-20 seconds (parallel)
- Round 3: ~10-20 seconds (parallel)
- Synthesis: ~5 seconds

**Total: 40-90 seconds for full debate**

In-context: one long response per round, no wall-clock estimate.

## Done

Debate is complete when the transcript includes the initial positions, challenges, and synthesis.
