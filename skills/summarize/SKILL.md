---
name: summarize
description: Summarize or extract text/transcripts from URLs, podcasts, and local files (great fallback for “transcribe this YouTube/video”).
homepage: https://summarize.sh
---

# Summarize

Fast CLI to summarize URLs, local files, and YouTube links.

## When to use (trigger phrases)

Use this skill immediately when the user asks any of:

- “use summarize.sh”
- “what’s this link/video about?”
- “summarize this URL/article”
- “transcribe this YouTube/video” (best-effort transcript extraction; no `yt-dlp` needed)

## Quick start

```bash
summarize "https://example.com" --model google/gemini-3-flash-preview
summarize "/path/to/file.pdf" --model google/gemini-3-flash-preview
summarize "https://youtu.be/dQw4w9WgXcQ" --youtube auto
```

## YouTube: summary vs transcript

Best-effort transcript (URLs only):

```bash
summarize "https://youtu.be/dQw4w9WgXcQ" --youtube auto --extract
```

If the user asked for a transcript but it’s huge, return a tight summary first, then ask which section/time range to expand.

## CLI fallback / override guidance

Repo config prefers CLI backends in this order when available:

1. Codex (`cli/codex/gpt-5.4-mini`)
2. OpenCode (`cli/opencode/opencode/big-pickle`)
3. Claude Code (`cli/claude`, summarize default `sonnet`)

Use these rules:

- If Codex/OpenCode are missing or unusable but Claude Code exists, use `summarize "$INPUT" --cli claude`.
- `--cli claude` resolves to summarize's built-in Claude default (`sonnet`), not a host-specific Claude override.
- `--cli codex` still uses the repo-pinned Codex model from config.
- If the user explicitly wants the host CLI's own default model instead of summarize's configured one, bypass summarize's LLM step and use extract-and-pipe handoff.
- `pi` is not a native summarize CLI provider today, so for pi use extract-and-pipe handoff, not `--cli`.

Examples:

```bash
summarize "$INPUT" --cli claude
summarize "$INPUT" --model cli/claude

{ printf 'Summarize the following content:\n\n'; summarize "$INPUT" --extract --format md; } | claude -p
{ printf 'Summarize the following content:\n\n'; summarize "$INPUT" --extract --format md; } | pi --print --no-session --no-context-files
{ printf 'Summarize the following content:\n\n'; summarize "$INPUT" --extract --format md; } | codex exec --skip-git-repo-check -
```

For giant pages/transcripts, consider `--max-extract-characters <n>` or write the extracted Markdown to a temp file first.

## Useful flags

- `--cli [claude|codex|opencode]`
- `--length short|medium|long|xl|xxl|<chars>`
- `--max-output-tokens <count>`
- `--extract`
- `--json` (machine readable)
- `--firecrawl auto|off|always` (fallback extraction)
- `--youtube auto` (Apify fallback if `APIFY_API_TOKEN` set)

## Config

Optional config file: `~/.summarize/config.json`

Optional services:

- `FIRECRAWL_API_KEY` for blocked sites
- `APIFY_API_TOKEN` for YouTube fallback
