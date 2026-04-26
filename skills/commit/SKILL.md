---
name: commit
description: >
  Use this skill when the user asks to "commit", "make a commit", "commit my changes",
  "create a commit", or invokes /commit. Detects the active VCS (git, jj, or hg),
  inspects the working copy, drafts a commit message in the project's standard format
  (terse summary + detailed paragraph + test plan), and creates the commit. Honors
  staged-files-only when files are already staged in git.
version: 0.1.0
---

# Commit

Create a commit for the current changes in a consistent format, regardless of which version control system is in use.

## Detect VCS

Detect which version control system is in use by running status commands (try in this order):

1. **git**: `git status`
2. **jj**: `jj status`
3. **hg**: `hg status`

Use the first one that succeeds.

## Staged Files

- If files are already staged, only commit those staged files.
- If no files are staged, commit all changes.

For git:

- Check with `git diff --cached --name-only` (staged) vs `git diff --name-only` (unstaged).
- If nothing staged, use `git add -A` before committing.

For jj:

- jj tracks all changes automatically; use `jj commit` to commit the working copy.

For hg:

- Check with `hg status`.
- If nothing staged, `hg commit` commits all changes by default.

## View Changes

Depending on the VCS:

- **git**: `git diff --cached` (staged) or `git diff` (unstaged).
- **jj**: `jj diff`.
- **hg**: `hg diff`.

## Commit Message Format

```
<terse-summary>

<detailed-summary>

Test Plan:
<test-plan>
```

## Instructions

1. Detect which VCS is in use.
2. View the current changes.
3. Write a **terse summary** (first line): a concise one-line description of the change (max 72 chars).
4. Write a **detailed summary**: a brief paragraph explaining what changed and why. Keep it terse and to the point. Avoid flowery language.
5. Write a **test plan**: describe how this change was tested. Acceptable values include:
   - "CI" — if the change is covered by existing CI tests.
   - "Ran tests" — if you ran the test suite locally.
   - Specific test commands or manual testing steps if applicable.

## Create Commit

Depending on the VCS:

- **git**: `git commit -m "<message>"`.
- **jj**: `jj commit -m "<message>"` or `jj describe -m "<message>"`.
- **hg**: `hg commit -m "<message>"`.

## Example

```
Add user authentication middleware

JWT auth for API routes. Middleware validates tokens on protected
endpoints and attaches user context to requests.

Test Plan:
Ran tests
```
