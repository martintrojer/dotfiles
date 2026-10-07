# jj recovery

Undo, conflicts, and stack repair for the `commit` skill. Every command here is non-interactive.

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
- You forgot `jj new` after `jj describe` or `jj edit` and accumulated edits in the wrong commit.

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

Pick the right side (or merge them by hand), delete all the marker lines, then `jj squash --use-destination-message` the WC into the conflicted commit to fold the resolution back.

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
