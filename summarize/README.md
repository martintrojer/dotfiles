# summarize

Local configuration for the `summarize.sh` workflow helper.

This package is used for the summarization pipeline that feeds URLs, PDFs,
DOCX, and HTML through `markitdown` and returns Markdown suitable for the rest
of the agent workflow. It is treated as a first-class tool in day-to-day use,
not just a one-off utility.

## Contents

- `config.json`: CLI routing and model selection for the summarize workflow

## CLI routing

Repo default keeps the existing CLI preference order, but now also allows
Claude Code as a fallback when Codex/OpenCode are not available:

1. `cli/codex/gpt-5.4-mini`
2. `cli/opencode/opencode/big-pickle`
3. `cli/claude` (no repo model pin; uses summarize's built-in Claude default, currently `sonnet`)

That means plain `summarize ...` still prefers Codex/OpenCode on the machines
that have them, while Claude-only hosts can still use the stowed config.

## Overrides

Use Claude Code directly from summarize:

```bash
summarize "$INPUT" --cli claude
summarize "$INPUT" --model cli/claude
```

Because `cli.claude.model` is intentionally unset, those use summarize's
built-in Claude default instead of a repo-pinned Claude model.

If you specifically want the host CLI's own default model instead of anything
pinned in `~/.summarize/config.json`, bypass summarize's LLM step and hand the
extracted Markdown to the other CLI:

```bash
{ printf 'Summarize the following content:\n\n'; summarize "$INPUT" --extract --format md; } | claude -p
{ printf 'Summarize the following content:\n\n'; summarize "$INPUT" --extract --format md; } | pi --print --no-session --no-context-files
{ printf 'Summarize the following content:\n\n'; summarize "$INPUT" --extract --format md; } | codex exec --skip-git-repo-check -
```

Notes:

- `--cli codex` still respects the pinned `cli.codex.model` from config; use the extract-and-pipe flow when you want Codex's own default.
- `pi` is not a native summarize CLI provider today, so extract-and-pipe is the supported override path there.
- For very large pages/transcripts, cap the payload with `--max-extract-characters <n>` or write `--extract --format md` output to a temp file first.

## Notes

- Keep the package minimal; the helper is intentionally config-driven.
- If the workflow changes, update this package README alongside the config so the behavior stays documented near the stowed files.
