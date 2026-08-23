# `guides/`: interactive learning guides

This directory contains one Markdown guide per tool. `build.py` renders the
guides as static HTML without third-party dependencies. These guides replace
the handwritten HTML that used to live in `docs/`.

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

`build.py --check` renders every source, validates every quiz block, and
asserts that the resulting HTML is well-formed without writing output. The
check fails if an inline transform interleaves tags or `render_block` drops a
`</ul>`.

Renderer edge cases (code spans vs. emphasis, quiz validation messages,
well-formedness) are covered by `guides/test_build.py`, which
`make check-guides` runs too:

```sh
python3 -m unittest discover -s guides -p 'test_*.py'
```
