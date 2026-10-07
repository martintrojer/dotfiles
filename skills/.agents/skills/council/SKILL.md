---
name: council
description: "Multi-agent debate with round-by-round transcripts: topic-briefed members argue a 3-round DEBATE or 1-round QUICK check. Use only when explicitly asked for a council, debate, multi-agent debate, or multiple expert perspectives."
---

# Council Skill

> Vendored from [danielmiessler/LifeOS](https://github.com/danielmiessler/LifeOS)
> `install/skills/Council` (upstream v1.1.20). De-Clauded for this machine: no
> voice-notification curl, no `~/.claude/` customization path, no execution-log
> JSONL, no RedTeam cross-references. See **Running the members** below — members
> run as `mu_delegate` panes, or in-context when that tool is absent.

<!-- local: description narrowed to explicit council/debate requests; "weigh options", "deliberate", "pros and cons" and "what would experts say" stole ordinary trade-off questions -->

## What It Does

Runs a multi-agent debate. Custom-composed agents discuss a topic over rounds, respond to each other's points, and expose weak arguments through substantive disagreement. You get a visible round-by-round transcript plus a synthesis. DEBATE runs three rounds; QUICK runs one for a fast perspective check.

## The Problem

When you ask one model for an opinion, you get one frame and one set of blind spots. Asking for "pros and cons" gives you a flat list with no one pushing back. Deliberation needs distinct experts who disagree on the merits, so weak arguments surface before you commit. Generic built-in agents tend to produce bland agreement; this skill composes topic-specific agents with conflicting positions.

## How It Works

Members discuss the topic in rounds and respond to specific claims from earlier rounds.

## Members Are Custom Briefs

Write each council member inline as a short brief — a name, a role, a stance, and what they'll push on. A generic persona is topic-ignorant and produces bland agreement. The friction comes from four *different* briefs, each with real domain expertise and a distinct analytical angle.

See `CouncilMembers.md` for the slot guidance and an example brief.

## Running the members

Upstream assumes a `Task`/`Agent` tool with `subagent_type`. Pi has no such
built-in; in pi the members run as `mu` delegates when the `mu_delegate` tool
is available, and in-context otherwise:

| Mode | When | How |
|------|------|-----|
| **Delegates (default when available)** | `mu_delegate` is in your tool list (an unmanaged pi with `mu link pi`; it is absent inside `mu` panes) | One `mu_delegate` call per member per round: `brief` = the member brief, `task` = round instructions + full topic context + the transcript so far (a delegate starts with no context). Members within a round run in parallel as visible `scratch` panes the user can attach to and steer. Each answer arrives as a follow-up message; do not poll. Rounds stay sequential: wait for all members' follow-ups before sending the next round |
| **In-context (fallback)** | No `mu_delegate` | You play every member yourself, one section at a time. Write the brief, then answer *as* that member before moving to the next. Do not peek ahead — draft each member's Round-N text in full before starting the next member's |
| **mu crew** | Long debate you want to interrogate between rounds | Drive it with the `mu` skill, one long-lived agent per member |

Other harnesses (Claude Code, Codex) can use their native task tool in the
delegate slot; those are hidden subagents, with no pane to watch or steer.

In-context loses true independence — you know what the other members will say.
Compensate by committing to each brief's stance hard, and by writing the
weakest member's position first so it doesn't get retro-fitted to the
conclusion. The transcript is still the deliverable either way.


## Workflow Routing

Route to the appropriate workflow based on the request.

| Trigger | Workflow |
|---------|----------|
| Full structured debate (3 rounds, visible transcript) | `Workflows/Debate.md` |
| Quick consensus check (1 round, fast) | `Workflows/Quick.md` |

Council is collaborative-adversarial: members debate to find the best path.
Pure adversarial attack on a single idea is out of scope — say so rather than
bending a debate into a teardown.

## Quick Reference

| Workflow | Purpose | Rounds | Output |
|----------|---------|--------|--------|
| **DEBATE** | Full structured discussion | 3 | Complete transcript + synthesis |
| **QUICK** | Fast perspective check | 1 | Initial positions only |

## Context Files

| File | Content |
|------|---------|
| `CouncilMembers.md` | How to write council member briefs inline |
| `RoundStructure.md` | Three-round debate structure and timing |
| `OutputFormat.md` | Transcript format templates |

## Core Philosophy

**Origin:** Council compares informed positions through direct challenges. Domain-specific members respond to each other's claims instead of listing independent opinions.

**Agents:** Every council member is a custom brief you write for the topic. This gives each member a distinct role, stance, and domain expertise. Generic agents produce generic debate; topic-specific briefs produce sharp, informed debate.

**Speed:** With delegates, execution is parallel within rounds and sequential between them — a 3-round debate of 4 members is 12 agent calls but only 3 sequential waits (40-90 seconds). In-context, it's one pass and correspondingly slower to read but cheaper to run.

## Examples

```
"Council: Should we use WebSockets or SSE?"
-> Write 4 member briefs (real-time architect, frontend-DX, ops skeptic, analyst)
-> DEBATE workflow -> 3-round transcript

"Quick council check: Is this API design reasonable?"
-> Write 4 member briefs with API-relevant roles
-> QUICK workflow -> Fast perspectives

"Council: Is AI overhyped?"
-> Write briefs: AI builder, security skeptic, pragmatic engineer, evidence analyst
-> DEBATE workflow -> 3-round transcript
```

## Integration

**Works well with:**
- **`brainstorm`** - Council to pick between approaches, then brainstorm the winner into a spec
- **`mu`** - Supplies `mu_delegate` for the members; drive a crew directly when you want long-lived members you can interrogate. To pick one winner from many candidates rather than debate, use mu's `recipes/tournament.md`; to attack a plan or finding, `recipes/refute.md`

## Practices

1. Use QUICK for sanity checks, DEBATE for important decisions
2. Write each member's brief around the specific topic, not a generic role
3. Give each member a distinct stance — four identical agents produce no friction

## Gotchas

- **Council members are inline briefs.** There is no composition tool. Write four different topic-specific briefs; a bare persona-less agent produces bland agreement.
- **Debates need substantive disagreement.** If all members agree, the topic may not warrant Council.
- **More agents ≠ better debate.** 4-6 well-briefed members outperform 12 generic ones.
- **In-context mode is self-debate.** You are simulating disagreement, not sampling it. Worth doing, worth not overtrusting — a convergence you reached alone is weaker evidence than four independent agents landing in the same place.
