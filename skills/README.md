# Agent Skills

Shared agent skills following the [Agent Skills standard](https://agentskills.io).

## Distribution

Two paths:

- **Universal path (Codex, OpenCode, Pi, Cursor, Amp, Cline, Warp, OpenClaw, generic):** `./dotfiles-sync --apply` symlinks each `<name>/` into `~/.agents/skills/<name>`. All these agents read that path natively. Edits propagate live (symlink, not copy).
- **Claude Code:** the same `skills/` tree is bundled into the dotfiles Claude plugin. Install via `claude plugin marketplace add martintrojer/dotfiles && claude plugin install mtrojer@dotfiles`. Re-run `claude plugin install mtrojer@dotfiles` after each push to refresh — `./dotfiles-sync --apply` prints these commands as a closing hint.

Skills are auto-discovered and can be invoked explicitly with `/skill:name` or loaded automatically when the agent detects a matching task.

## Skills

> `ponytail` is vendored from
> [DietrichGebert/ponytail](https://github.com/DietrichGebert/ponytail) (MIT).
> `avoid-ai-writing` is vendored from
> [conorbronsdon/avoid-ai-writing](https://github.com/conorbronsdon/avoid-ai-writing)
> (MIT, v3.10.0 @ `6e1369d`) — `SKILL.md` plus the zero-dependency `detector/`
> engine (`node detector/patterns.js`).
> Keep edits to vendored skills minimal so they stay easy to re-sync against upstream.

### Planning & Execution

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:brainstorm` | "brainstorm", "design a feature", "think through" | Collaborative dialogue to refine vague ideas into technical specs |
| `/skill:write-plan` | "write a plan", "plan this feature" | Break an approved design into executable implementation tasks |
| `/skill:execute-plan` | "execute the plan", "run the plan" | Follow through on a plan created by `/skill:write-plan` |

### Writing

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:avoid-ai-writing` | "remove AI-isms", "clean up AI writing", "make this sound less like AI" | Audit/rewrite text to strip AI writing patterns. Detect, rewrite, and edit-in-place modes; optional voice profiles; bundled deterministic `detector/patterns.js` |

### Code Quality

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:code-reviewer` | "review this code", after refactors | Find dead code, duplication, unnecessary complexity, non-idiomatic patterns |
| `/skill:test-reviewer` | "review tests", after writing tests | Catch false confidence, excessive mocking, meaningless assertions |

### Multi-Agent

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:council` | "council", "debate", "weigh options" | Multi-agent collaborative-adversarial debate with visible transcripts |

### Caveman Mode (Token Efficiency)

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:caveman` | "caveman mode", "be brief" | Ultra-compressed communication (~75% token savings). Levels: lite, full, ultra |
| `/skill:ponytail` | "ponytail", "be lazy", "yagni", "simplest solution" | Forces the laziest solution that works: YAGNI, stdlib/native first, shortest diff. Levels: lite, full, ultra |

### Tools & Integrations

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:summarize` | "use summarize.sh", "summarize this URL/article", "transcribe this YouTube/video" | `summarize.sh` workflow helper for URLs, podcasts, local files, and best-effort transcript extraction |
| `/skill:tmux` | Interactive CLI needed | Remote-control tmux sessions by sending keystrokes and scraping output |
