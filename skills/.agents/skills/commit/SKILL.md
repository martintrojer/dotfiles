---
name: commit
description: >
  Use when the user asks to commit. Detects jj, git, or hg; matches the repo's
  message style; writes an honest test plan; commits non-interactively.
version: 0.4.0
---

# Commit

Create a commit for the current changes in a consistent format, regardless of which version control system is in use.

## Before you commit

- **Don't claim it works without evidence.** The Test Plan is a factual record,
  not an intention. If you write `Ran tests`, you ran them *in this session* and
  saw them pass. See the `verification-before-completion` skill — this is the
  most common place its rule gets broken, because the commit message is where
  the claim gets written down permanently.
- **Run the repo's gate first** if it has one — `make check-all`, `just check`,
  a `pre-commit` config, whatever the project uses. Check `AGENTS.md` /
  `CONTRIBUTING.md` if unsure.
- Commit only what the user asked for. Unrelated drift in the working copy gets
  mentioned, not swept in — especially under jj, where there is no staging area
  to hold it back.

## Detect VCS

Try in this order; use the first that succeeds:

1. `jj root` — a `.jj` dir means jj even when `.git` also exists. In a colocated repo `git status` succeeds too, so checking git first picks the wrong tool.
2. `git rev-parse --show-toplevel`
3. `hg root`

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
<scope>: <terse summary>

<detailed summary>

Test Plan:
<test plan>
```

### Subject line

**Match the repo.** Read `git log --oneline -30` (or `jj log`) before writing
anything and copy the prevailing shape. Don't impose a convention the project
doesn't use.

Where the repo uses a scope prefix, the shape is `scope: lowercase imperative`,
max ~72 chars. This is the load-bearing field — it's all anyone sees in a log.

The best subjects name **the change and its reason**, not just the area
touched. Where it fits, `X, not Y` / `X instead of Y` says what was rejected:

```
flatpak: gate daily update on real connectivity, not network-online.target
dotfiles-sync: refuse a directory target in apply_link instead of IsADirectoryError
sway: float portal file dialogs, which no rule could ever match
make: syntax-check zsh files, which no gate covered
```

Each of those answers "why was this needed?" in the subject alone. Compare the
weak forms: `fix flatpak timer`, `update sway config`, `improve error handling`.

### Body

Explain **why**, not what — the diff already carries the what. Terse, no
flowery language. Bullets are fine for discrete points.

For a non-obvious bug, the body's job is the causal chain: what was observed,
why it happened, why the fix addresses the cause rather than the symptom. If
you can't write that chain, you may not have found the root cause yet — see
`systematic-debugging`.

Worth stating explicitly when true:

- What was rejected and why (saves the next person re-litigating it)
- Why a fix lives where it does, if the obvious spot was somewhere else
- Anything the diff makes look wrong but is deliberate

### Test Plan

What was *actually* done, not what should be done:

- `CI` — covered by existing CI tests.
- `Ran tests` — ran the suite locally, this session, and it passed.
- Specific commands, before/after numbers, or manual steps.

Be concrete where concreteness is available. `make check-all (62 tests, green)`
beats `Ran tests`. Before/after measurements beat "verified it's faster".

**For new tests, state that you watched them fail.** A test never observed
failing has not been shown to test anything (`test-driven-development`). The
strongest form names the mutation:

```
Test Plan:
make check-all (62 tests, green). Each new test verified by breaking the
code it covers: no-op'ing the prune fails 2, restoring the rglob fails the
unmanaged-subtree test.
```

If something is untested, say so here. An honest gap is information; a false
`Ran tests` is a lie that outlives the session.

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

jj has several commands that drop into an editor by default and will hang a non-interactive shell. Always pass the non-interactive equivalent.

**Never rely on `EDITOR` to stop jj.** jj's `ui.editor` config beats `$EDITOR`, so `EDITOR=false jj squash` still opens the configured editor and hangs. Pass `-m` / `--use-destination-message`, or use `JJ_EDITOR=false` when probing whether a command wants an editor (it then exits 1 instead of hanging).

| Command | Trap | Use instead |
|---------|------|-------------|
| `jj describe` | opens `$EDITOR` | `jj describe -m "..."` or `jj describe --stdin <<<"..."` |
| `jj commit` | opens `$EDITOR` for the new description | `jj commit -m "..."` |
| `jj squash` | opens `$EDITOR` to merge source + dest messages | `jj squash -m "..."` (use a fresh combined message) or `jj squash --use-destination-message` (keep the dest's existing message and discard the source's) |
| `jj split` | opens TUI to pick which changes go where, and `$EDITOR` without `-m` | `jj split -m "msg" <paths>` (the listed paths go into the first commit; no editor). Afterwards @ is the second commit, holding the remaining changes and @'s old description (none if it had none) |
| `jj resolve` | opens the configured merge tool (often a TUI) | Edit conflict markers in files directly, then `jj squash --use-destination-message` the resolution into the conflicted commit |
| `jj diffedit` | TUI | Avoid in scripted use |

**The post-commit working copy gotcha (READ THIS).** After `jj describe -m` or `jj edit <rev>`, the working copy is left **sitting on that commit**, not on a fresh empty commit. Any further edit you make becomes additional content *on that same commit*, not a new commit on top. This is the single most common rake to step on.

The fix: **`jj new` immediately after `jj describe` or `jj edit`** once you are done with that commit, to create a fresh empty WC on top.

```bash
jj describe -m "..."   # set the description of @
jj new                 # CRITICAL: create a fresh WC; otherwise next
                       #           edit lands inside the just-described
                       #           commit
```

`jj commit -m "..."` already leaves a fresh empty @ ("commit @ and start a new one"), and so does `jj squash -m "..."` with no paths. `jj squash -m "..." <paths>` moves only those paths, so @ keeps the remaining changes. None of these need `jj new`.

When in doubt: run `jj st` and check what `Working copy (@)` reports. If it has a description that looks like the work you just finalized, you're sitting on it — `jj new` to escape.

For undo, conflicts, `jj edit`, or a stack gone wrong, read [jj-recovery.md](jj-recovery.md).

### git-specific notes

- `git commit -m` only takes the first `-m` as the subject. Use multiple `-m` flags to add paragraphs (each `-m` becomes a separate paragraph), OR pipe a full message via `git commit -F -`.
- If the user has staged files, commit only those (`git commit` without `-a`). If nothing is staged, `git add -A && git commit ...`.
- Don't `git push` unless the user explicitly asks.

### hg-specific notes

- `hg commit` with no `-m` opens `$EDITOR`; always pass `-m "..."`.
- `hg commit` commits everything by default; there's no staging area.

## Example

```
auth: validate token expiry with <=, not <

The middleware treated a token expiring exactly on the boundary second as
still valid, so a request arriving in that window was authenticated against
an expired token. The comparison is the whole bug; every caller routes
through this one check, so fixing it here covers all of them.

Test Plan:
Ran tests (48, green). New boundary test verified by reverting the operator
to `<` and watching it fail.
```

Note what the subject does: names the fix *and* the rejected alternative in
72 characters. The body gives the causal chain and says why the fix is at the
shared choke point. The test plan proves the new test can fail.

## When things go wrong

- **A jj command hung?** It probably opened `$EDITOR` or a TUI. Kill it (Ctrl-C if interactive; the operation log will show whether the state was committed). Then re-run with the appropriate `-m` / `--use-destination-message` / `--stdin` flag.
- **Created the wrong commit?** `jj undo` (jj) or `git reset --soft HEAD~1` (git) or `hg rollback` (hg, if no other operations have happened since).
- **Working copy in a conflict state after a rebase?** Inspect `jj st` for affected files, edit the conflict markers, `jj squash --use-destination-message` to fold the resolution back into the conflicted commit. Or `jj undo` the rebase if you want to back out entirely.
- **Forgot `jj new` after `jj describe`/`jj edit` and now your latest edits landed in the wrong commit?** Either: (a) `jj split -m "msg" <paths>` to peel them out into a fresh commit (bare `jj split <paths>` opens the editor); or (b) `jj op restore <pre-edit-snapshot>` to rewind, then redo with proper `jj new` discipline.
- **Pushed to the wrong branch?** Out of scope for this skill — handle separately.
- **Asked to commit work you haven't verified?** Say so and run the gate, or
  write the honest Test Plan (`Not tested — <why>`). Don't write `Ran tests`
  speculatively.

## Related skills

| Skill | When |
|-------|------|
| `verification-before-completion` | Before writing any Test Plan. Evidence precedes the claim |
| `test-driven-development` | New tests in the diff — the watch-it-fail step is what the Test Plan attests to |
| `systematic-debugging` | Bug fix. The body should carry the root cause, not the symptom |
