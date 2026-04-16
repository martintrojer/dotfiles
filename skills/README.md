# Agent Skills

Shared agent skills following the [Agent Skills standard](https://agentskills.io). Symlinked into `~/.agents/skills/` by stow.

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
| `/skill:caveman-commit` | "write a commit", "/commit" | Compressed commit messages in Conventional Commits format |
| `/skill:caveman-review` | "review this PR", "/review" | One-line code review comments: location, problem, fix |
| `/skill:caveman-help` | "caveman help" | Quick-reference card for all caveman modes and commands |

### Tools & Integrations

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/skill:github` | GitHub tasks (PRs, issues, CI) | `gh` CLI cheatsheet for PRs, issues, runs, and API queries |
| `/skill:summarize` | "summarize this URL/file" | Fetch URL or convert PDF/DOCX/HTML to Markdown via `markitdown` |
| `/skill:librarian` | Remote git repo references | Cache remote repos under `~/.cache/checkouts/` for local reference |
| `/skill:tmux` | Interactive CLI needed | Remote-control tmux sessions by sending keystrokes and scraping output |
