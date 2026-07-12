---
name: brainstorm
description: This skill should be used when the user asks to "brainstorm", "design a feature", "think through an idea", "help me plan", or has a vague concept that needs refining into a technical specification through collaborative dialogue.
version: 0.1.0
---

# Brainstorm - Design Phase

## Purpose

Transform vague ideas into concrete technical specifications through Socratic, collaborative dialogue. Do not give immediate answers—guide the user to clarity through targeted questions.

## Process

### 1. Autonomous Recon
Before asking questions:
- Scan project files to understand architecture and patterns
- Check recent commits to see current development focus
- Identify relevant existing code, components, or patterns
- Note tech stack, conventions, and constraints

### 2. Clarification Phase
Ask targeted, single questions to refine the idea:
- Use multiple-choice options when possible
- Focus on one aspect at a time
- Cover: constraints, scope, user needs, edge cases
- Ask about brand identity or UI patterns if relevant
- Never ask more than one question per turn

Example question types:
- "What's the primary goal: A) speed, B) flexibility, C) simplicity?"
- "Who is the main user of this feature?"
- "What existing pattern should this follow?"

### 3. Sectional Review
Present the emerging design in small chunks:
- Keep each section to 200-300 words
- Wait for user validation before proceeding
- Sections might include:
  - Problem statement
  - Proposed solution overview
  - Data model / API shape
  - UI/UX considerations
  - Edge cases and error handling
  - Testing approach

### 4. Outcome
Produce a finalized design document that serves as the source of truth:
- Clear problem statement
- Technical approach with rationale
- Scope boundaries (what's in, what's out)
- Key decisions made and why
- Open questions or future considerations
- Implementation checklist

## Guidelines

- Be curious, not prescriptive
- Surface trade-offs explicitly
- Reference existing project patterns when relevant
- Keep the user in control of decisions
- Document the "why" behind each choice

## Next Step

Once the design document is finalized and approved, suggest:
"Design complete. Ready to create an implementation plan? Invoke the `write-plan` skill to break this into executable tasks."
