# Agent Skills

Shared agent skills following the [Agent Skills standard](https://agentskills.io).

## Distribution

Two paths:

- **Universal path (Codex, OpenCode, Pi, Cursor, Amp, Cline, Warp, OpenClaw, generic):** `./stow-all.py --apply` symlinks each `<name>/` into `~/.agents/skills/<name>`. All these agents read that path natively. Edits propagate live (symlink, not copy).
- **Claude Code:** the same `skills/` tree is bundled into the dotfiles Claude plugin. Install via `claude plugin marketplace add martintrojer/dotfiles && claude plugin install mtrojer@dotfiles`. Re-run `claude plugin install mtrojer@dotfiles` after each push to refresh — `./stow-all.py --apply` prints these commands as a closing hint.

Skills are auto-discovered and can be invoked explicitly with `/skill:name` or loaded automatically when the agent detects a matching task.

## Skills

### Planning & Execution

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:brainstorm` | "brainstorm", "design a feature", "think through" | Collaborative dialogue to refine vague ideas into technical specs |
| `/skill:write-plan` | "write a plan", "plan this feature" | Break an approved design into executable implementation tasks |
| `/skill:execute-plan` | "execute the plan", "run the plan" | Follow through on a plan created by `/skill:write-plan` |

### Code Quality

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:code-reviewer` | "review this code", after refactors | Find dead code, duplication, unnecessary complexity, non-idiomatic patterns |
| `/skill:test-reviewer` | "review tests", after writing tests | Catch false confidence, excessive mocking, meaningless assertions |
| `/skill:changelog` | "generate a changelog", "release notes" | Document changes between versions using git history and tags |

### Multi-Agent

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:council` | "council", "debate", "weigh options" | Multi-agent collaborative-adversarial debate with visible transcripts |

### Caveman Mode (Token Efficiency)

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:caveman` | "caveman mode", "be brief" | Ultra-compressed communication (~75% token savings). Levels: lite, full, ultra |

### Tools & Integrations

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:summarize` | "use summarize.sh", "summarize this URL/article", "transcribe this YouTube/video" | `summarize.sh` workflow helper for URLs, podcasts, local files, and best-effort transcript extraction |
| `/skill:tmux` | Interactive CLI needed | Remote-control tmux sessions by sending keystrokes and scraping output |
