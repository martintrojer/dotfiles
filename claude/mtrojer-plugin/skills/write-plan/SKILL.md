---
name: write-plan
description: This skill should be used when the user asks to "write a plan", "create an implementation plan", "plan this feature", or has an approved design that needs to be broken into executable tasks for implementation.
version: 0.1.0
superpowers: writing-plans
---

# Write Plan - Planning Phase

## Purpose

Generate a highly detailed, executable implementation plan tailored for a "junior engineer" with zero context. Every task must be self-contained and actionable.

## Prerequisites

- An approved design document or clear feature specification
- Understanding of the project structure and conventions

## Plan Structure

Save the plan as: `docs/plans/YYYY-MM-DD-<feature-name>.md`

### Plan Template

```markdown
# Implementation Plan: <Feature Name>

**Date:** YYYY-MM-DD
**Design Doc:** <link or reference>
**Estimated Tasks:** N

## Overview
<1-2 sentence summary>

## Tasks

### Task 1: <Short description>
**File:** `path/to/file.ext`
**Time:** ~X minutes

**Steps:**
1. <Specific action>
2. <Specific action>

**Code:**
\`\`\`language
// Exact code to add/modify
\`\`\`

**Verify:**
\`\`\`bash
<command to verify task is complete>
\`\`\`

**Commit:** `<conventional commit message>`

---
```

## Granularity Requirements

Each task should be:
- 2-5 minutes of work
- Focused on a single file or concern
- Include exact file paths
- Include code snippets where applicable
- Include verification command (test, lint, build, curl, etc.)
- Include suggested commit message

## Standards to Enforce

### TDD (Red-Green-Refactor)
1. Write failing test first
2. Write minimal code to pass
3. Refactor if needed
4. Each cycle is a separate task

### DRY (Don't Repeat Yourself)
- Flag potential duplication
- Include refactoring tasks when patterns emerge

### Frequent Commits
- One commit per task
- Use conventional commit format
- Keep commits atomic and revertible

## Plan Metadata

At the end of the plan, include:

```markdown
## Progress Tracker

- [ ] Task 1: <description>
- [ ] Task 2: <description>
...

## Notes
<Any context for resuming work later>
```

## Output

- Create the plan file in `docs/plans/`
- Create the directory if it doesn't exist
- Summarize the plan for the user
- Confirm the plan is ready for execution

## Next Step

Once the plan is complete, suggest:
"Plan ready. Invoke the `execute-plan` skill to start implementing tasks sequentially."
