# summarize

Local configuration for the `summarize.sh` workflow helper.

This package is used for the summarization pipeline that feeds URLs, PDFs,
DOCX, and HTML through `markitdown` and returns Markdown suitable for the rest
of the agent workflow. It is treated as a first-class tool in day-to-day use,
not just a one-off utility.

## Contents

- `config.json`: CLI routing and model selection for the summarize workflow

## Notes

- Keep the package minimal; the helper is intentionally config-driven.
- If the workflow changes, update this package README alongside the config so
  the behavior stays documented near the stowed files.
