---
name: brainstorm
description: Use before any non-trivial creative work — new features, new components, new behaviour, or reworking existing behaviour. Explores intent, constraints and design through one-question-at-a-time dialogue, then produces an approved spec. Triggers on "brainstorm", "design a feature", "think through an idea", "help me plan", or any vague concept that needs refining before code gets written.
version: 0.2.0
---

# Brainstorm — Design Phase

Turn an idea into a design the user has actually agreed to, through dialogue
rather than a wall of proposal. You are not here to answer immediately; you are
here to ask the question that makes the answer obvious.

<HARD-GATE>
Do not write code, scaffold files, or start implementing until you have
presented a design and the user has approved it. This holds regardless of how
simple the task looks.
</HARD-GATE>

## "This is too simple to need a design"

No. Every non-trivial change goes through this, including one-file utilities and
config changes. Simple-looking work is exactly where unexamined assumptions
cost the most, because nobody thought to check. The design can be three
sentences — but present it and get a yes before building.

The escape hatch is scope, not ceremony: a genuine one-liner with no design
question in it doesn't need this skill at all. If you're unsure which you have,
you have a design question.

## Process

### 1. Recon before questions

Never open with "what would you like to build?" — find out first.

- Read the files the change would touch, and trace the actual flow end to end
- Check recent commits (`jj log`, `git log`) for what's in motion right now
- Note conventions, tech stack, and existing patterns worth following
- Look for prior art in-repo: the thing may half-exist already

Come to the first question already knowing the shape of the codebase.

### 2. Scope check — before detail questions

If the request spans several independent subsystems, say so immediately.
Don't burn questions refining a detail of something that needs splitting first.

Help decompose: what are the independent pieces, how do they relate, what order
should they be built in. Then brainstorm the *first* piece properly. Each piece
gets its own spec.

### 3. Clarifying questions — one at a time

- **One question per message.** Not two, not a numbered list of six. A batch
  of parallel questions loses the dependency order — an early answer often
  reshapes which questions still matter.
- **Carry your own recommended answer** with each question. The user should be
  reacting to a proposal, not staring at a blank prompt.
- **Answer it from the repo if the repo can answer it.** Never ask the user to
  explain what already exists in the code.
- Prefer multiple choice; open-ended is fine when the space is genuinely open.
- Aim at purpose, constraints, and success criteria — not implementation trivia
  you could decide yourself.
- Stop when you can state what "done" looks like. Extra questions are a cost.

Good: "Is the priority here A) fewest moving parts, or B) handling the
multi-user case from day one? I'd take A — nothing in the repo suggests a
second user yet, and B is reversible."
Bad: "What are your thoughts on the architecture?"

### 4. Propose 2–3 approaches

With trade-offs, conversationally. Lead with your recommendation and say why.
YAGNI ruthlessly — cut speculative features from every option before presenting
it, and don't present an option you think is bad just to make three.

### 5. Present the design in sections

Scale each section to its actual complexity: a sentence if it's obvious, up to
200–300 words if it's genuinely subtle. Check in after each section rather than
delivering the whole thing and asking "thoughts?".

Cover, as applicable: architecture, components and their boundaries, data flow,
error handling, testing approach, and what's explicitly out of scope.

**Design for isolation:** break the system into units with one clear purpose
each, communicating through defined interfaces. For each unit you should be able
to say what it does, how it's used, and what it depends on. If you can't
describe a unit without describing its internals, the boundary is wrong.

**In existing codebases:** follow the patterns already there. Where existing
code genuinely obstructs the work — a file that's grown unwieldy, a tangled
responsibility — fold a targeted improvement into the design. Don't propose
unrelated refactoring.

### 6. Write the spec

Save to `docs/specs/YYYY-MM-DD-<topic>.md` (user preference overrides). Create
the directory if needed.

Contents: problem statement, the approach and why, scope boundaries (in and
out), key decisions with rationale, open questions, and a rough implementation
checklist.

Write the *why* down. The what is recoverable from the code later; the why is
not.

### 7. Self-review the spec

Fresh eyes on what you just wrote:

1. **Placeholders** — any "TBD", vague requirement, or section you skipped? Fix.
2. **Consistency** — do any two sections contradict? Does the architecture match
   the feature descriptions?
3. **Scope** — is this one implementation plan's worth, or does it still need
   splitting?
4. **Ambiguity** — could any requirement be read two ways? Pick one, make it
   explicit.

Fix inline. Don't re-review.

### 8. User review gate

> "Spec written to `<path>`. Have a look before we turn it into a plan — tell me
> what you want changed."

Wait. If they want changes, make them and re-run step 7.

## Guidelines

- Curious, not prescriptive. The user decides; you make the decision informed.
- Surface trade-offs explicitly, including ones that count against your
  recommendation.
- Never ask a question whose answer you could have found by reading the repo.

## Next step

Once the spec is approved: "Ready to break this into an implementation plan?
The `write-plan` skill turns this into ordered, verifiable tasks."

## Related

| Skill | When |
|-------|------|
| `write-plan` | Downstream — the only skill this one hands off to |
| `council` | Several viable approaches and no clear winner. Debate them, then brainstorm the survivor |
| `ponytail` | Step 4. YAGNI applies hardest at design time, when cutting costs nothing |
