---
name: summarize
description: "Summarize CLI: URLs, files, podcasts, YouTube, transcripts, media, extraction, and JSON output. Use for \"summarize this URL/article\", \"what's this link/video about?\", \"transcribe this YouTube/video\"."
---

# Summarize

Use the `summarize` CLI on `PATH` as the canonical interface.

## Backend

<!-- local addition -->

Everything routes through **OpenCode** (`cli/opencode/opencode/big-pickle`),
the only enabled CLI provider. Free, no API keys, nothing to keep running.
Run `summarize "$INPUT"`; the config resolves on its own. Don't pass `--cli`
or `--model` unless the user asks for a specific backend.

OpenCode's free tier rejects outdated clients. If the summary step fails with
`UpgradeRequired`, upgrade `opencode`.

Config lives in the dotfiles `summarize/` package (`~/.summarize/config.json` is
a symlink into it; edit the package copy). See `summarize/README.md` for why pi
is deliberately *not* the backend.

## Start

1. Confirm the command and current contract:

   ```bash
   summarize --version
   summarize --help
   ```

2. Inspect model/provider readiness when a summary needs an LLM:

   ```bash
   summarize status
   summarize status --json
   ```

3. Run the narrowest workflow below. Quote URLs and paths. Add `--timeout 2m` for slow remote or media inputs.

Never print, request, or copy API-key values. `summarize status` reports availability without exposing secrets.

## Summarize

Web page or remote document:

```bash
summarize "https://example.com/article"
summarize "https://example.com/report.pdf" --length short
```

Local file or stdin:

```bash
summarize "./report.pdf"
summarize "./recording.m4a"
printf '%s\n' "Long text to summarize" | summarize -
```

Use `--plain` for unrendered Markdown/text. Use `--language`, `--length`, `--prompt`, or `--prompt-file` only when the task requires an override. Use `--cli <provider>` only when the user requests a specific provider.

## Extract without a summary

Use `--extract` to stop after extraction or transcription:

```bash
summarize "https://example.com/article" --extract --format md
summarize "./report.pdf" --extract --format md
summarize "https://youtu.be/VIDEO_ID" --extract --format md
```

`--extract` does not support stdin. Extraction can still call configured transcription, OCR, Firecrawl, or Markdown services; it only skips the final summary call. `--markdown-mode llm` also invokes an LLM to reshape extracted text.

## YouTube, audio, and video

Default transcript selection:

```bash
summarize "https://youtu.be/VIDEO_ID"
summarize "https://youtu.be/VIDEO_ID" --extract --format md --timestamps
```

Use `--youtube web` to require web captions or `--youtube yt-dlp` to require the download/transcription path. Keep `auto` unless the user needs a specific source.

Local or remote audio/video:

```bash
summarize "./interview.mp3" --extract
summarize "./interview.mp4" --extract --timestamps
summarize "./interview.mp3" --extract --diarize
```

`--transcriber auto` is the default. Use an explicit transcriber only when requested or diagnosing a provider. Diarization may require configured ElevenLabs or OpenAI access. Speaker identification is a separate opt-in step; do not infer identities without evidence.

For slides:

```bash
summarize "https://youtu.be/VIDEO_ID" --slides
summarize "./talk.mp4" --slides --extract
```

Slide extraction may require `yt-dlp`; OCR requires `tesseract`.

## Extract-and-pipe handoff

<!-- local addition -->

When the user wants a different model, a custom prompt, or another CLI's own
defaults rather than summarize's summarization step, skip the LLM stage and
pipe:

```bash
{ printf 'Summarize the following content:\n\n'; summarize "$INPUT" --extract --format md; } | pi --print --no-session --no-context-files
{ printf 'Summarize the following content:\n\n'; summarize "$INPUT" --extract --format md; } | codex exec --skip-git-repo-check -
```

For giant pages or transcripts, add `--max-extract-characters <n>` or write the
extracted Markdown to a temp file first. If the user asked for a transcript but
it is huge, return a tight summary first, then ask which section or time range
to expand.

## JSON for automation

Use JSON when another command or agent will parse the result:

```bash
summarize "https://example.com" --json --metrics off > result.json
jq -r '.summary // .extracted.content // empty' result.json
```

The stable top-level envelope contains `input`, `env`, `extracted`, `prompt`, `llm`, `metrics`, and `summary`. `summary` or `llm` can be `null` when extraction or a no-model path handles the input. In `--extract --json` mode, read extracted text from `.extracted.content`.

JSON stays on stdout. Progress, warnings, and finish metrics stay on stderr. Do not merge stderr into stdout before parsing. Use `--metrics detailed` only when the task needs usage details.

## Configuration and dependencies

Precedence: CLI flags, process environment, `~/.summarize/config.json`, built-in defaults. Prefer flags for one run; change config only when the user asks for a persistent default.

Useful diagnostics:

```bash
summarize status --verbose
summarize status --probe
summarize "INPUT" --verbose
```

Plain web summaries need no media tools. Media paths may use `ffmpeg`, `yt-dlp`, local Whisper/ONNX, or configured cloud transcription. Website fallback may use Firecrawl. Confirm the exact missing capability from the error before installing tools or changing config.

Inputs may be sent to the selected model, extraction, OCR, or transcription provider. For confidential material, confirm the approved provider or use an approved local path before running the command.

## Verify

After every run:

- Require exit status `0`.
- Require non-empty summary or extracted content.
- For JSON, parse stdout with `jq` or another JSON parser.
- For source-sensitive work, inspect `extracted`, `llm`, and stderr diagnostics rather than assuming the selected path.
- Re-run the exact final command after changing provider, config, or flags.

For current option details, run `summarize --help` or read the docs at <https://github.com/steipete/summarize/tree/main/docs>.
