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

## Backend

Everything routes through **OpenCode** (`cli/opencode/opencode/big-pickle`),
the only enabled CLI provider. Free, no API keys, nothing to keep running.

Just run `summarize "$INPUT"` — the config resolves on its own. Don't pass
`--cli` or `--model` unless the user asks for a specific backend.

Config lives in the dotfiles `summarize/` package; see `summarize/README.md`
for why pi is deliberately *not* the backend.

## Extract-and-pipe handoff

When the user wants a different model, a custom prompt, or another CLI's own
defaults rather than summarize's summarization step, skip the LLM stage and
pipe:

```bash
{ printf 'Summarize the following content:\n\n'; summarize "$INPUT" --extract --format md; } | pi --print --no-session --no-context-files
{ printf 'Summarize the following content:\n\n'; summarize "$INPUT" --extract --format md; } | codex exec --skip-git-repo-check -
```

For giant pages/transcripts, add `--max-extract-characters <n>` or write the
extracted Markdown to a temp file first.

## Useful flags

- `--length short|medium|long|xl|xxl|<chars>`
- `--max-output-tokens <count>`
- `--extract`
- `--json` (machine readable)
- `--firecrawl auto|off|always` (fallback extraction)
- `--youtube auto` (Apify fallback if `APIFY_API_TOKEN` set)

## Config

`~/.summarize/config.json` is a symlink into the dotfiles `summarize/` package —
edit it there, not in `$HOME`.

Optional services (neither is configured here):

- `FIRECRAWL_API_KEY` for blocked sites
- `APIFY_API_TOKEN` for YouTube fallback
