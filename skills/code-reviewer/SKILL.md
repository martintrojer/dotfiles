---
name: code-reviewer
description: Use this skill when reviewing production code for dead code, duplication, unnecessary complexity, and non-idiomatic patterns. Use after implementing a feature, after refactors, or whenever code quality feedback is requested.
version: 0.1.0
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

### 3. Unnecessary Complexity
- Over-engineered abstractions and indirection
- Premature generalization or pattern cargo-culting
- Functions/classes with too many responsibilities
- Clever code that hurts readability

### 4. Idiomatic and Functional Improvements
- Use language/framework conventions
- Prefer standard library over custom utility code
- Suggest pure functions, composition, immutability when appropriate
- Favor clear control flow (early returns, reduced nesting)

## Review Process

1. Scan for obvious issues (dead code, duplicate blocks)
2. Evaluate architecture for unnecessary complexity
3. Check language/framework idiomaticity
4. Propose simplifications with concrete alternatives
5. Prioritize by impact

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
- Note the principle being applied

## Principles

- Deletion is often the highest-value refactor
- Simplicity beats cleverness
- Abstractions must justify their cost
- Idiomatic code is easier to maintain
- Be specific and actionable
