---
name: council
description: "Multi-agent collaborative debate producing visible round-by-round transcripts with real intellectual friction — members are topic-briefed custom agents, run as a 3-round DEBATE or a 1-round QUICK check, to find the best path. USE WHEN council, debate, multiple perspectives, weigh options, deliberate, get different views, what would experts say, pros and cons."
---

# Council Skill

> Vendored from [danielmiessler/LifeOS](https://github.com/danielmiessler/LifeOS)
> `install/skills/Council` (upstream v1.1.20). De-Clauded for this machine: no
> voice-notification curl, no `~/.claude/` customization path, no execution-log
> JSONL, no RedTeam cross-references. See **Running the members** below — the
> parallel-subagent path is optional here.

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
built-in, so pick whichever of these the current harness supports:

| Mode | When | How |
|------|------|-----|
| **In-context (default)** | Always available; use unless told otherwise | You play every member yourself, one section at a time. Write the brief, then answer *as* that member before moving to the next. Do not peek ahead — draft each member's Round-N text in full before starting the next member's |
| **Parallel subagents** | Harness exposes a subagent/Task tool (pi ships one as an uninstalled example extension; Claude Code and Codex have one natively) | One agent per member, prompt = brief + round instructions + topic. Rounds stay sequential; members within a round run parallel |
| **tmux crew** | Long debate you want to steer mid-flight | Drive it with the `mu` skill, one pane per member |

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

**Speed:** With parallel subagents, execution is parallel within rounds and sequential between them — a 3-round debate of 4 members is 12 agent calls but only 3 sequential waits (40-90 seconds). In-context, it's one pass and correspondingly slower to read but cheaper to run.

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
- **`mu`** - When you want the members as long-lived tmux panes you can interrogate

## Practices

1. Use QUICK for sanity checks, DEBATE for important decisions
2. Write each member's brief around the specific topic, not a generic role
3. Give each member a distinct stance — four identical agents produce no friction

## Gotchas

- **Council members are inline briefs.** There is no composition tool. Write four different topic-specific briefs; a bare persona-less agent produces bland agreement.
- **Debates need substantive disagreement.** If all members agree, the topic may not warrant Council.
- **More agents ≠ better debate.** 4-6 well-briefed members outperform 12 generic ones.
- **In-context mode is self-debate.** You are simulating disagreement, not sampling it. Worth doing, worth not overtrusting — a convergence you reached alone is weaker evidence than four independent agents landing in the same place.
