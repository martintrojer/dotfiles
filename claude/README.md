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

## Installation (Codex Skills)

Codex reads skills from `~/.codex/skills`. This repo includes a dedicated `codex` stow package for these skills.

1. Stow the package:
   ```bash
   stow codex
   ```

2. Restart Codex to pick up the new skills.

## Contents

### Agents
- **code-reviewer** - Wrapper agent that delegates to the `code-reviewer` skill
- **test-reviewer** - Wrapper agent that delegates to the `test-reviewer` skill

### Skills
- **brainstorm** - Collaborative feature design and ideation
- **changelog** - Generate changelogs from git history
- **write-plan** - Create implementation plans from approved designs
- **execute-plan** - Execute plans created by write-plan
- **code-reviewer** - Code quality review for dead code, duplication, and unnecessary complexity
- **test-reviewer** - Test quality review for excessive mocking and weak assertions

### Commands
- **/commit** - Guided commit workflow

### Hooks
- **Stop** - Reminds to run format, lint, and tests after file modifications

## Statusline

`statusline-command.sh` - Custom statusline showing git branch, jj bookmark, and model info (configured via `.claude/settings.json`).
