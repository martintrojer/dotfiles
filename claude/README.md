# Claude Code Personal Plugin

Personal Claude Code extensions including agents, skills, commands, and hooks.

## Installation (Local Marketplace)

1. Add the marketplace:
   ```
   /plugin marketplace add ~/dotfiles/claude/mtrojer-plugin
   ```

2. Install the plugin:
   ```
   /plugin install mtrojer@local
   ```

## Contents

### Agents
- **code-reviewer** - Reviews code for dead code, duplication, and non-idiomatic patterns
- **test-reviewer** - Reviews tests for excessive mocking and meaningless assertions

### Skills
- **brainstorm** - Collaborative feature design and ideation
- **changelog** - Generate changelogs from git history
- **write-plan** - Create implementation plans from approved designs
- **execute-plan** - Execute plans created by write-plan

### Commands
- **/commit** - Guided commit workflow

### Hooks
- **Stop** - Reminds to run format, lint, and tests after file modifications

## Statusline

`statusline-command.sh` - Custom statusline showing git branch, jj bookmark, and model info (configured via `.claude/settings.json`).
