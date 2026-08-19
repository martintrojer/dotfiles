---
name: code-reviewer
description: Use this skill when reviewing production code for dead code, duplication, unnecessary complexity, and non-idiomatic patterns. Use after implementing a feature, after refactors, or whenever code quality feedback is requested.
version: 0.3.0
---

# Code Reviewer

## Purpose

Provide direct, actionable code-quality review focused on maintainability, simplicity, and idiomatic style.

## What to Look For

### 1. Dead Code
- Unused functions, methods, classes, variables, imports
- Unreachable branches and redundant conditionals
- Commented-out code that should be deleted
- Legacy paths or feature flags that are never used

### 2. Duplication
- Copy-pasted logic with small variations
- Repeated constants and magic values
- Near-duplicate blocks that should be shared
- Cases where DRY improves clarity without over-abstraction

### 3. Unnecessary Complexity (Over-Engineering)
- Over-engineered abstractions and indirection
- Premature generalization or pattern cargo-culting
- Abstractions with a single implementation, config nobody sets, layers with one caller (YAGNI)
- Hand-rolled code the standard library already ships
- Dependencies or code doing what the platform/language already does natively
- Speculative features and dead flexibility kept "for later"
- Functions/classes with too many responsibilities
- Clever code that hurts readability

### 4. Idiomatic and Functional Improvements
- Use language/framework conventions
- Prefer standard library over custom utility code
- Suggest pure functions, composition, immutability when appropriate
- Favor clear control flow (early returns, reduced nesting)

### 5. Smell Baseline

<!-- local addition: the Fowler smell set from mattpocock/skills `code-review`
     (MIT). Named smells are cheaper and sharper than prose categories. -->

A fixed set of code smells (Fowler, *Refactoring*, ch.3) that applies even when
a repo documents no standards of its own. Each reads *what it is* → *how to
fix*; match them against the diff.

- **Mysterious Name** — a function, variable, or type whose name doesn't reveal
  what it does or holds. → rename it; if no honest name comes, the design is murky.
- **Duplicated Code** — the same logic shape in more than one hunk or file.
  → extract the shared shape, call it from both.
- **Feature Envy** — a method that reaches into another object's data more than
  its own. → move the method onto the data it envies.
- **Data Clumps** — the same few fields or params keep travelling together.
  → bundle them into one type, pass that.
- **Primitive Obsession** — a primitive or string standing in for a domain
  concept. → give the concept its own small type.
- **Repeated Switches** — the same `switch`/`if`-cascade on the same type recurs.
  → replace with polymorphism, or one map both sites share.
- **Shotgun Surgery** — one logical change forces scattered edits across many
  files. → gather what changes together into one module.
- **Divergent Change** — one module is edited for several unrelated reasons.
  → split so each module changes for one reason.
- **Speculative Generality** — abstraction, parameters, or hooks for needs that
  don't exist. → delete it; inline back until a real need shows.
- **Message Chains** — long `a.b().c().d()` navigation the caller shouldn't
  depend on. → hide the walk behind one method on the first object.
- **Middle Man** — a class or function that mostly just delegates onward.
  → cut it, call the real target directly.
- **Refused Bequest** — a subclass that ignores or overrides most of what it
  inherits. → drop the inheritance, use composition.

Two rules bind the baseline:

- **The repo overrides.** A documented repo standard always wins. Where it
  endorses something the baseline would flag, suppress the smell.
- **Always a judgement call.** Each smell is a labelled heuristic ("possible
  Feature Envy"), never a hard violation — and, like any standard here, skip
  anything tooling already enforces.

### 6. Design Shape and Enforcement

<!-- local addition: distilled from pstack's design red flags,
     minimize-reader-load, and encode-lessons-in-structure (MIT). -->

- **Information leakage** — one representation, policy, protocol, or storage
  detail is known by several modules. Keep it behind one boundary and expose the
  domain concept instead.
- **Temporal decomposition** — modules are split by execution order (`load`,
  `validate`, `transform`, `save`) while repeating the same knowledge. Group
  code by ownership of the data and its invariants.
- **Reader-held state** — understanding a value requires tracing layers or
  remembering mutable facts from elsewhere. Cut pass-through layers, shrink
  mutable scope, and derive instead of synchronizing.
- **Prose-only constraint** — a comment says `must`, `never`, `keep in sync`, or
  `do not remove`, but nothing enforces it. Prefer a type, test, lint rule,
  metadata flag, runtime check, or canonical helper. Keep prose when the rule
  genuinely requires judgment.

## Review Process

1. Scan for obvious issues (dead code, duplicate blocks)
2. Evaluate architecture for unnecessary complexity
3. Check language/framework idiomaticity
4. Pass the smell baseline and design-shape checks over the diff
5. Propose simplifications with concrete alternatives
6. Prioritize by impact

**Scope check first.** One diff or one module is a single-context job — just do
it. A repo-wide audit, or several independent areas each wanting a full pass,
fans out badly in one context: quality decays as the window fills, and the last
area reviewed gets the worst attention. Split it across `mu` reviewers, one per
area, and synthesise the findings.

## Output Format

Use this structure:

### Critical Issues
Problems that materially harm correctness or maintainability.

### Recommended Changes
Improvements that would significantly improve quality.

### Suggestions
Minor cleanups and idiomatic refinements.

For each finding:
- Quote the specific code
- Explain why it is a problem
- Propose a concrete fix
- Note the principle being applied, or name the smell

## Related

| Skill | When |
|-------|------|
| `ponytail` | Build-time mirror of this skill — the over-engineering checks above are its ladder run in reverse. Deletion is the highest-value outcome |
| `test-reviewer` | The diff touches tests. Different failure modes, different skill |
| `systematic-debugging` | Review turns up a bug — find the root cause before proposing the fix |
| `receiving-code-review` | You are on the receiving end of this output |
| `mu` | Repo-wide audit or several independent areas — one reviewer per area, then synthesise |

## Principles

- Deletion is often the highest-value refactor
- Simplicity beats cleverness
- Abstractions must justify their cost
- Idiomatic code is easier to maintain
- Be specific and actionable
