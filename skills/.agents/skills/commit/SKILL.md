---
name: commit
description: >
  Use this skill when the user asks to "commit", "make a commit", "commit my changes",
  "create a commit", or invokes /commit. Detects the active VCS (git, jj, or hg),
  inspects the working copy, drafts a commit message in the project's standard format
  (terse summary + detailed paragraph + test plan), and creates the commit. Honors
  staged-files-only when files are already staged in git. Handles jj-specific traps
  (interactive editors, conflict states, the operation log, the post-commit working
  copy gotcha) without dropping into TUIs that block non-interactive shells.
version: 0.2.0
---

# Commit

Create a commit for the current changes in a consistent format, regardless of which version control system is in use.

## Detect VCS

Try in this order; use the first that succeeds:

1. `git status`
2. `jj status`
3. `hg status`

## Inspect changes

| VCS | Status | Diff |
|-----|--------|------|
| git | `git status -s` | `git diff --cached` (staged) or `git diff` (unstaged) |
| jj  | `jj status`      | `jj diff` |
| hg  | `hg status`      | `hg diff` |

For git, also check `git diff --cached --name-only` vs `git diff --name-only` to know whether anything is staged. If something is staged, only commit those files; if nothing is staged, `git add -A` first.

For jj, the entire working copy is the change — there's no staging step. `jj status` shows everything that will land in the commit.

For hg, `hg status` shows everything; `hg commit` commits all changes by default.

## Commit message format

```
<terse-summary>

<detailed-summary>

Test Plan:
<test-plan>
```

- **Terse summary** (first line): one-line description, max 72 chars. This line is what shows up in `jj log` / `git log --oneline` and is the load-bearing field — make it count.
- **Detailed summary**: brief paragraph(s) explaining what changed and why. Terse, avoid flowery language. Bullet lists are fine for multiple discrete points.
- **Test plan**: how this was tested. Acceptable values:
  - `CI` — covered by existing CI tests.
  - `Ran tests` — ran the test suite locally.
  - Specific commands or manual steps if applicable.

## Create the commit

Use the **non-interactive form for every VCS** — never invoke a command that would drop into `$EDITOR`, because this skill may run inside a non-interactive shell where TUIs hang.

| VCS | Command |
|-----|---------|
| git | `git commit -m "<terse>" -m "<detailed-and-test-plan>"` (or use `-F -` and pipe a heredoc for full multi-line control) |
| jj  | `jj commit -m "$(cat <<'EOF' ... EOF)"` (heredoc inside `$()` keeps the multi-line message intact) |
| hg  | `hg commit -m "<message>"` |

For multi-line messages with shell metacharacters or backticks, **always use a heredoc inside `$(cat <<'EOF' ... EOF)`** rather than escaping inline. The single-quoted `'EOF'` delimiter prevents shell expansion inside the heredoc body, so backticks, `$`, etc. survive.

## VCS notes

### jj-specific traps and idioms

jj has several commands that drop into `$EDITOR` by default and will hang a non-interactive shell. Always pass the non-interactive equivalent.

| Command | Trap | Use instead |
|---------|------|-------------|
| `jj describe` | opens `$EDITOR` | `jj describe -m "..."` or `jj describe --stdin <<<"..."` |
| `jj commit` | opens `$EDITOR` for the new description | `jj commit -m "..."` |
| `jj squash` | opens `$EDITOR` to merge source + dest messages | `jj squash -m "..."` (use a fresh combined message) or `jj squash --use-destination-message` (keep the dest's existing message and discard the source's) |
| `jj split` | opens TUI to pick which changes go where | Don't run interactively. Either tell the user "I'll leave the split to you" or do it manually: `jj new -B @` to insert an empty parent, then `jj move --from @ --to @- <paths>` |
| `jj resolve` | opens the configured merge tool (often a TUI) | Edit conflict markers in files directly, then `jj squash` the resolution into the conflicted commit |
| `jj diffedit` / `jj absorb` | TUIs | Avoid in scripted use |

**The post-commit working copy gotcha (READ THIS).** After `jj commit -m`, `jj describe -m`, or `jj squash -m`, the working copy is left **sitting on the just-finalized commit**, not on a fresh empty commit. Any further edit you make becomes additional content *on that same commit*, not a new commit on top. This is the single most common rake to step on.

The fix: **`jj new` immediately after every "finalize this commit" operation** to create a fresh empty WC on top.

```bash
jj describe -m "..."   # set the description of @
jj new                 # CRITICAL: create a fresh WC; otherwise next
                       #           edit lands inside the just-described
                       #           commit
```

`jj commit -m "..."` *does* create a new empty WC automatically as part of its semantics ("commit @ and start a new one") — so you don't need `jj new` after `jj commit`. You DO need it after `jj squash`, `jj describe`, and `jj edit <rev>`.

When in doubt: run `jj st` and check what `Working copy (@)` reports. If it has a description that looks like the work you just finalized, you're sitting on it — `jj new` to escape.

**The operation log is your friend.** Every state-changing jj command (commit, describe, squash, rebase, abandon, even `jj edit`) is reversible via `jj undo`. If something goes wrong:

```bash
jj undo               # rewind the most recent operation
jj op log             # see the operation history
jj op restore <id>    # rewind to a specific operation (more surgical
                      # than a chain of jj undo's, especially when the
                      # operation you want to undo is several steps back)
```

Use this aggressively. It's especially useful when:
- A `jj squash` merged things you didn't want merged, or sent content to the wrong commit.
- A `jj rebase` produced unexpected conflicts and you want to back out.
- You ran `jj abandon` on the wrong commit.
- An interactive command hung and you killed it mid-state.
- You forgot `jj new` after a finalize operation and accumulated edits in the wrong commit.

`jj op restore <id>` is often cleaner than `jj undo`-ing N times. Find the snapshot before the bad operation in `jj op log` and restore directly.

**Conflict states are silent.** A `jj rebase` (or implicit rebase from `jj edit`-ing a non-leaf commit) can leave descendants in conflict without aborting. Always check `jj st` after operations that touch the commit graph; look for `(conflict)` markers. Resolve by editing the conflict markers in-place. jj uses its own conflict marker format for 2-sided conflicts:

```
<<<<<<< conflict 1 of 1
+++++++ <commit-id> "description" (rebase destination)
<dest content>
%%%%%%% diff from: <ancestor> ... to: <source>
 <unchanged context>
-<removed line>
+<added line>
>>>>>>> conflict 1 of 1 ends
```

Pick the right side (or merge them by hand), delete all the marker lines, then `jj squash` the WC into the conflicted commit to fold the resolution back.

**`jj edit <rev>` is destructive.** It makes the named rev your working copy, and *every save mutates that rev's contents directly*. Descendants get auto-rebased and may conflict. Prefer `jj new <rev>` ("branch off this rev as a new working commit") unless the user explicitly wants to amend `<rev>` in place.

**`jj commit` vs `jj describe`:**
- `jj commit -m "..."` finalizes the current WC as a commit and creates a fresh empty WC on top. Use this when the work is done and you want to start the next thing.
- `jj describe -m "..."` only sets the description of the current WC; doesn't create a new WC. Use this when you want to keep iterating on the same change. **Follow with `jj new` if you want to start a new change after.**

**Verify the squash landed where you think.** Especially when iterating on a stack, run `jj log -r 'mutable() & ~empty()'` after a squash to confirm the destination commit's description and content are what you expect. Easy mistake: squashing into `@-` (the parent of WC) lands in the wrong commit if you're confused about where the WC is parented.

**Stack inspection cheat sheet:**
```bash
jj log -r 'mutable()'                # all your local commits up to the trunk
jj log -r 'mutable() & ~empty()'     # same, but skip empty WC commits
jj log -r '@-..@ | conflicts()'      # focus on conflicted commits
jj st                                # working copy + parent summary
jj diff -r <rev>                     # what's in a specific commit
jj diff -r <rev> --stat              # just the file list
```

### git-specific notes

- `git commit -m` only takes the first `-m` as the subject. Use multiple `-m` flags to add paragraphs (each `-m` becomes a separate paragraph), OR pipe a full message via `git commit -F -`.
- If the user has staged files, commit only those (`git commit` without `-a`). If nothing is staged, `git add -A && git commit ...`.
- Don't `git push` unless the user explicitly asks.

### hg-specific notes

- `hg commit` with no `-m` opens `$EDITOR`; always pass `-m "..."`.
- `hg commit` commits everything by default; there's no staging area.

## Example

```
Add user authentication middleware

JWT auth for API routes. Middleware validates tokens on protected
endpoints and attaches user context to requests.

Test Plan:
Ran tests
```

## When things go wrong

- **A jj command hung?** It probably opened `$EDITOR` or a TUI. Kill it (Ctrl-C if interactive; the operation log will show whether the state was committed). Then re-run with the appropriate `-m` / `--use-destination-message` / `--stdin` flag.
- **Created the wrong commit?** `jj undo` (jj) or `git reset --soft HEAD~1` (git) or `hg rollback` (hg, if no other operations have happened since).
- **Working copy in a conflict state after a rebase?** Inspect `jj st` for affected files, edit the conflict markers, `jj squash` to fold the resolution back into the conflicted commit. Or `jj undo` the rebase if you want to back out entirely.
- **Forgot `jj new` after `jj describe`/`jj squash` and now your latest edits landed in the wrong commit?** Either: (a) `jj split <paths>` to peel them out into a fresh commit (non-interactive form); or (b) `jj op restore <pre-edit-snapshot>` to rewind, then redo with proper `jj new` discipline.
- **Pushed to the wrong branch?** Out of scope for this skill — handle separately.
