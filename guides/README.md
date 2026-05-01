# `guides/` — interactive learning guides

Source for the per-tool learning guides that used to live as hand-written
HTML in `docs/`. One Markdown file per tool, rendered to static HTML by
`build.py` (stdlib-only, no third-party deps).

## Files

| Path             | Purpose                                         |
| ---------------- | ----------------------------------------------- |
| `UPPER.md`       | guide source, one per tool (UPPER matches the   |
|                  | original `docs/UPPER_LEARNING_GUIDE.html` name) |
| `build.py`       | renderer — Markdown subset + TOML quiz blocks   |
| `template.html`  | wrapper template (placeholder substitution)     |
| `style.css`      | Catppuccin Mocha palette, single source         |
| `quiz.js`        | quiz engine, single source                      |
| `build/`         | gitignored output (`make build-guides`)         |

Output filenames are lowercased (`UPPER.md` → `build/upper.html`) so URLs
stay readable.

## Source format

A guide is plain Markdown with a small constrained subset:

- `#` / `##` / `###` headings
- `-` bullet lists (single level)
- inline `` `code ``, `**bold**`, `[text](url)`
- ` ```quiz ` fenced blocks whose body is TOML

A `quiz` block is a TOML `[[questions]]` array:

````markdown
```quiz
[[questions]]
q = "Which key opens help?"
options = ["`?` only", "`F1` or `~`", "`Ctrl-h`"]
answer = 1
why = "Both keys are bound to help in the default keymap."
```
````

`answer` is the zero-based index into `options`. `why` shows under the
question after grading.

## Build / serve

From the repo root:

```sh
make build-guides   # render guides/*.md → guides/build/*.html
make serve-guides   # build then `python3 -m http.server` in guides/build
```

`build.py --check` parses every source and validates every quiz block
without writing output (used as the lint pass).
