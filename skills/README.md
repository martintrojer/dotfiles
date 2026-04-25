# Agent Skills

Shared agent skills following the [Agent Skills standard](https://agentskills.io).

Distributed as part of the dotfiles repo via three install paths (each handled by the upstream tool):

- **Codex / OpenCode / Cursor / OpenClaw / generic**: `npx skills add martintrojer/dotfiles` — fans the skills out to `~/.agents/skills/` and per-agent paths.
- **Claude Code**: `claude plugin marketplace add martintrojer/dotfiles && claude plugin install mtrojer@dotfiles` — reads `skills/` directly from the plugin source.
- **Pi**: `pi install git:github.com/martintrojer/dotfiles` — reads `skills/` per the `pi.skills` manifest in the repo-root `package.json`.

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
