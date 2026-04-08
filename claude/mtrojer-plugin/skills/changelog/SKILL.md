---
name: changelog
description: This skill should be used when the user asks to "generate a changelog", "update changelog", "create release notes", or needs help documenting changes between versions using git history and tags.
version: 0.1.0
---

# Changelog Generator

## Purpose

Generate or update CHANGELOG.md using git history and tags.

## Workflow

### 1. Analyze Existing Changelog
- Read `CHANGELOG.md` if it exists
- Identify the last documented version/tag
- Note the format and style used

### 2. Get Git Tags
- Run `git tag --sort=-creatordate` to list tags (newest first)
- Show recent tags as potential starting points

### 3. Ask Starting Point
Prompt the user: "Where should I start generating the changelog from?"

Suggest options based on:
- Last tag in CHANGELOG.md
- Recent git tags
- A specific commit hash
- "From the beginning"

### 4. Gather Commits
- Run `git log --oneline <start>..HEAD`
- Group by type if using conventional commits (feat, fix, docs, etc.)
- Otherwise group by general category

### 5. Generate Changelog Entry
- Match existing CHANGELOG.md format, or use Keep a Changelog format
- Include: version header, date, grouped changes
- Be terse but descriptive

### 6. Remind About Tagging
If HEAD is not tagged, ask:
"HEAD is not tagged. Create a release tag? What version?"

Suggest version based on:
- Change types (major/minor/patch per semver)
- Previous tag version

### 7. Update CHANGELOG.md
- Prepend new entry
- Preserve existing entries
