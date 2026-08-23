# summarize

Local configuration for [`summarize`](https://github.com/steipete/summarize)
(`@steipete/summarize`, installed globally via npm under mise). Point it at a
URL, PDF, local file, or YouTube link and it returns Markdown.

The agent-facing usage guide is the `summarize` skill
(`skills/.agents/skills/summarize/`); this README covers the config only.

## Contents

- `config.json`: model routing for the summarize CLI

## Routing: OpenCode only

```
model:       cli/opencode/opencode/big-pickle
cli.enabled: ["opencode"]
```

Plain `summarize "$INPUT"` uses this route without extra flags. It needs no API
key or local model service.

### Why not pi

pi was tried and rejected. summarize spawns pi with `--no-extensions`, which
disables the `modelbridge` custom provider; since no plain API keys are set on
this host, pi is then left with no usable model and every run dies on
`No API key found`. It can be forced to work with

```json
"pi": { "extraArgs": ["--extension", "~/.pi/agent/extensions/modelbridge.ts"] }
```

but summarize then depends on `modelbridge` at `127.0.0.1:3000` and uses paid
tokens for each summary. OpenCode avoids both constraints without overriding
`--no-extensions`.

## Overrides

For a different model, a custom prompt, or another CLI's own defaults, skip
summarize's LLM stage and pipe the extracted Markdown:

```bash
{ printf 'Summarize the following content:\n\n'; summarize "$INPUT" --extract --format md; } | pi --print --no-session --no-context-files
{ printf 'Summarize the following content:\n\n'; summarize "$INPUT" --extract --format md; } | codex exec --skip-git-repo-check -
```

Cap huge payloads with `--max-extract-characters <n>`, or write `--extract
--format md` to a temp file first.

## Notes

- Keep the package minimal; the helper is intentionally config-driven.
- `~/.summarize/config.json` is a symlink to this package. Edit the package copy,
  not the link in `$HOME`.
- If the routing changes, update this README and the `summarize` skill together.
