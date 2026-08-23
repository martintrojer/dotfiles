# Council Members

Council members are custom personas you write inline. There is no composition tool or trait matrix. Write each member's brief for the topic; a generic trait lookup produces weaker roles.

## Why inline briefs, not generic personas

A persona-less agent has no stake in the topic and produces bland agreement. Council needs members who disagree on the merits. The friction comes from each member having a distinct role, expertise, and stance — which you supply in the brief. Write four different briefs; never run four identical ones.

## How to Create Council Members

### Step 1: Analyze the Topic

Decide which perspectives would create useful disagreement for this debate. Design the roles around the topic, not from a generic list.

**Example — "Should we use WebSockets or SSE?"**
- Real-time systems architect who defends push-first bidirectional transport
- Frontend-DX advocate who wants the simplest thing that ships
- Ops/reliability skeptic who distrusts long-lived connections
- Industry researcher who weighs precedent and adoption data

**Example — "Is AI overhyped?"**
- AI infrastructure builder who ships with these tools daily
- Security practitioner skeptic who has seen the failure modes
- Pragmatic engineer focused on real-world trade-offs
- Evidence-based researcher who wants the numbers

### Step 2: Write Each Member's Brief

For each member, write 2–4 sentences with a name, role, expertise, stance, and claims to challenge. Use that paragraph as the persona.

Example brief:
> **Mara — real-time systems architect.** Believes push-first. Will defend WebSocket bidirectionality and attack SSE's connection-count limits and reconnection story. Speaks precisely, cites protocol behavior.

### Step 3: Run the members

Each member's input is the brief you wrote, plus the round instructions and the topic context. See **Running the members** in `SKILL.md` for the three execution modes — in-context (default), parallel subagents, or `mu` panes.

In-context, that means writing this before you answer as them:

```
[member brief]

[round instructions]

Topic: [topic + relevant context]
```

Then answer in that member's voice, in full, before moving to the next member.

## Default Perspective Slots

When the user doesn't specify members, cover these four perspectives — but write each one tailored to the topic, not as a generic role:

| Slot | Purpose |
|------|---------|
| **Builder** | Has built things in this domain; argues from what actually ships |
| **Skeptic** | Challenges assumptions, finds the flaws and failure modes |
| **Pragmatist** | Implementation reality, cost, and trade-offs |
| **Analyst** | Data, precedent, and external evidence |

The slots are a starting guide. Adjust the mix to the topic — a pure design question may want two builders and a user advocate instead.
