#!/usr/bin/env python3
"""Render theme regions across the dotfiles repo from `docs/palette.toml`.

Each consumer config file (sway, waybar, tmux, foot, fuzzel, mako,
swaylock, btop, eza, zsh, plus a handful of Python helpers) carries
a marker pair around the section that needs theme hex values:

    # THEME BEGIN: <name>
    ...marked region...
    # THEME END: <name>

The comment syntax of each marker line is the file's own comment
syntax. The `<name>` is opaque to the file but tells the renderer
which template under `_dotfiles_sync/themes/<name>.tmpl` to render.
Templates are plain text with ``{{mocha.<colorname>}}`` placeholders.

Run modes:
    --check   Print drift; exit 1 if any file is out of sync.
    --write   Rewrite drifting regions in place. Default.

The repo Makefile wires this up as `make theme` (write) and
`make check-theme` (check). Per `AGENTS.md`, this lives under
`_dotfiles_sync/` because it's repo control plane, not user-facing.

Why not import the rendered values at runtime instead of materializing
them into each config? Because the apps that read these files
(swaybg, fuzzel, mako, btop, zsh, …) don't speak Python and can't
indirect through `docs/palette.toml`. Materializing keeps the apps
oblivious; the indirection is purely an authoring convenience.
"""

from __future__ import annotations

import argparse
import difflib
import re
import sys
import tomllib
from collections.abc import Callable, Iterable
from dataclasses import dataclass
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
PALETTE_PATH = REPO_ROOT / "docs" / "palette.toml"
TEMPLATES_DIR = Path(__file__).resolve().parent / "themes"

# Marker pattern. The `<comment-prefix>` is whatever the file uses
# (`#`, `//`, `/*`); the `<comment-suffix>` is whatever closes it
# (typically nothing, or `*/` for CSS). The name capture is the
# part the renderer uses to locate the template. The full marker
# line (whitespace and comment chars included) is preserved
# verbatim during rewrite — only the content *between* the two
# markers is replaced.
MARKER_BEGIN_RE = re.compile(
    r"^(?P<prefix>.*?)THEME BEGIN:\s*(?P<name>[A-Za-z0-9_\-]+)\s*(?P<suffix>.*)$"
)
MARKER_END_RE = re.compile(
    r"^(?P<prefix>.*?)THEME END:\s*(?P<name>[A-Za-z0-9_\-]+)\s*(?P<suffix>.*)$"
)

PLACEHOLDER_RE = re.compile(
    r"\{\{\s*(?P<group>\w+)\.(?P<color>\w+)"
    r"(?:\s*\|\s*(?P<filter>\w+))?\s*\}\}"
)

# Supported placeholder filters. Add new ones here — a typo'd filter
# name fails loud rather than silently passing the raw hex through.
FILTERS: dict[str, Callable[[str], str]] = {
    # Strip the leading '#' from a hex string. swaylock's config
    # syntax wants bare 6-char hex (no '#'), often with an alpha
    # suffix appended in the template: `{{mocha.red|nohash}}ff`.
    "nohash": (lambda v: v[1:] if v.startswith("#") else v),
}


# ---------------------------------------------------------------------
# File registry
# ---------------------------------------------------------------------
#
# Each consumer file is declared explicitly. Adding a new consumer:
#   1. Add a marker pair to the file.
#   2. Add a template under _dotfiles_sync/themes/<name>.tmpl.
#   3. Append a Consumer() entry below.
#
# Files NOT in this list are not touched by the renderer, even if they
# contain hex values. Vendored theme files (yazi flavors, bat tmTheme)
# stay vendored; they're upstream artifacts, not our authoring surface.


@dataclass(frozen=True)
class Consumer:
    """A file with one or more named theme regions."""

    path: Path
    regions: tuple[str, ...]


def repo(rel: str) -> Path:
    return REPO_ROOT / rel


CONSUMERS: tuple[Consumer, ...] = (
    Consumer(repo("sway/.config/sway/config"), ("sway-palette",)),
    Consumer(repo("waybar/.config/waybar/style.css"), ("waybar-palette",)),
    Consumer(
        repo("waybar/.config/waybar/config.jsonc"), ("waybar-calendar-colors",)
    ),
    Consumer(repo("mako/.config/mako/config"), ("mako-colors",)),
    Consumer(repo("swaylock/.config/swaylock/config"), ("swaylock-colors",)),
    Consumer(repo("tmux/.tmux.conf"), ("tmux-palette",)),
    Consumer(repo("zsh/.zsh/tools.zsh"), ("zsh-prompt-colors",)),
    Consumer(repo("foot/.config/foot/foot.ini"), ("foot-colors",)),
    Consumer(repo("fuzzel/.config/fuzzel/fuzzel.ini"), ("fuzzel-colors",)),
    Consumer(repo("btop/.config/btop/themes/current.theme"), ("btop-colors",)),
    Consumer(repo("eza/.config/eza/theme.yml"), ("eza-colors",)),
    Consumer(
        repo("tmux/.config/tmux/scripts/status-hostname"),
        ("status-hostname-colors",),
    ),
    Consumer(
        repo("tmux/.config/tmux/scripts/status-ram"),
        ("status-ram-colors",),
    ),
    Consumer(
        repo("tmux/.config/tmux/scripts/tms"),
        ("tms-palette",),
    ),
    Consumer(
        repo("fedora/bin/.local/bin/wallpaper"),
        ("wallpaper-fallback-color",),
    ),
    Consumer(
        repo("sway/.config/sway/scripts/lock-screen"),
        ("lock-screen-fallback-color",),
    ),
    Consumer(
        repo("sway/.config/sway/scripts/session-wallpaper"),
        ("session-wallpaper-fallback-color",),
    ),
)


# ---------------------------------------------------------------------
# Palette loading and template expansion
# ---------------------------------------------------------------------


def load_palette(path: Path = PALETTE_PATH) -> dict[str, dict[str, str]]:
    """Parse the TOML palette. Returns a nested dict keyed by group then color."""
    with path.open("rb") as fp:
        raw = tomllib.load(fp)
    if not isinstance(raw, dict):
        raise SystemExit(f"render_theme: {path} is not a TOML table")
    return raw


def expand_template(text: str, palette: dict[str, dict[str, str]], where: str) -> str:
    """Substitute ``{{group.color}}`` placeholders. Loud failure on misses."""

    def replace(match: re.Match[str]) -> str:
        group = match.group("group")
        color = match.group("color")
        filter_name = match.group("filter")
        if group not in palette:
            raise SystemExit(f"render_theme: {where}: unknown group `{group}`")
        if color not in palette[group]:
            raise SystemExit(
                f"render_theme: {where}: unknown color `{group}.{color}` "
                f"(known: {sorted(palette[group])})"
            )
        value = palette[group][color]
        if filter_name is None:
            return value
        if filter_name not in FILTERS:
            raise SystemExit(
                f"render_theme: {where}: unknown filter `{filter_name}` "
                f"(known: {sorted(FILTERS)})"
            )
        return FILTERS[filter_name](value)

    return PLACEHOLDER_RE.sub(replace, text)


def render_region(name: str, palette: dict[str, dict[str, str]]) -> str:
    tmpl_path = TEMPLATES_DIR / f"{name}.tmpl"
    if not tmpl_path.is_file():
        raise SystemExit(f"render_theme: missing template `{tmpl_path}`")
    body = tmpl_path.read_text(encoding="utf-8")
    return expand_template(body, palette, where=str(tmpl_path))


# ---------------------------------------------------------------------
# Marker-bounded region rewrite
# ---------------------------------------------------------------------


@dataclass
class RegionEdit:
    """One marked region in a file: the byte range to replace."""

    name: str
    begin_line: int  # 0-indexed, the marker line itself (kept)
    end_line: int    # 0-indexed, the marker line itself (kept)


def find_regions(path: Path, expected: Iterable[str]) -> list[RegionEdit]:
    """Locate marked regions, validating the BEGIN/END pairing.

    `expected` is the consumer's declared region names; this is just
    used as a sanity check so a typo in the registry surfaces clearly.
    """
    lines = path.read_text(encoding="utf-8").splitlines()
    found: list[RegionEdit] = []
    open_marker: tuple[int, str] | None = None
    for idx, line in enumerate(lines):
        if (m := MARKER_BEGIN_RE.match(line)) is not None:
            name = m.group("name")
            if open_marker is not None:
                raise SystemExit(
                    f"render_theme: {path}: BEGIN `{name}` at line {idx + 1} "
                    f"while `{open_marker[1]}` is still open"
                )
            open_marker = (idx, name)
        elif (m := MARKER_END_RE.match(line)) is not None:
            name = m.group("name")
            if open_marker is None:
                raise SystemExit(
                    f"render_theme: {path}: END `{name}` at line {idx + 1} "
                    f"with no matching BEGIN"
                )
            if open_marker[1] != name:
                raise SystemExit(
                    f"render_theme: {path}: END `{name}` at line {idx + 1} "
                    f"does not match BEGIN `{open_marker[1]}`"
                )
            found.append(RegionEdit(name=name, begin_line=open_marker[0], end_line=idx))
            open_marker = None
    if open_marker is not None:
        raise SystemExit(
            f"render_theme: {path}: BEGIN `{open_marker[1]}` "
            f"at line {open_marker[0] + 1} has no matching END"
        )
    found_names = [r.name for r in found]
    expected_list = list(expected)
    if sorted(found_names) != sorted(expected_list):
        raise SystemExit(
            f"render_theme: {path}: marker mismatch. "
            f"file has {found_names}, registry expects {expected_list}"
        )
    return found


def apply_edits(
    path: Path,
    edits: list[RegionEdit],
    palette: dict[str, dict[str, str]],
) -> tuple[str, str]:
    """Return (old_text, new_text) for `path` after applying every edit.

    The marker lines themselves are preserved verbatim — only the
    content strictly between them is replaced with the rendered
    template body.
    """
    old_text = path.read_text(encoding="utf-8")
    lines = old_text.splitlines(keepends=True)

    # Apply edits bottom-up so earlier offsets stay valid.
    for edit in sorted(edits, key=lambda e: e.begin_line, reverse=True):
        rendered = render_region(edit.name, palette)
        # The template renders to a block of full lines; ensure trailing
        # newline so splice math stays simple.
        if not rendered.endswith("\n"):
            rendered += "\n"
        rendered_lines = rendered.splitlines(keepends=True)
        # Replace lines (begin_line, end_line) exclusive of the marker
        # lines themselves.
        lines[edit.begin_line + 1 : edit.end_line] = rendered_lines

    return old_text, "".join(lines)


# ---------------------------------------------------------------------
# Driver
# ---------------------------------------------------------------------


def process(
    consumers: Iterable[Consumer],
    palette: dict[str, dict[str, str]],
    *,
    check: bool,
) -> int:
    """Run check or write across every consumer. Return process exit code."""
    drift = 0
    for c in consumers:
        if not c.path.is_file():
            print(f"render_theme: missing file: {c.path}", file=sys.stderr)
            drift += 1
            continue
        edits = find_regions(c.path, c.regions)
        old_text, new_text = apply_edits(c.path, edits, palette)
        if old_text == new_text:
            continue
        rel = c.path.relative_to(REPO_ROOT)
        if check:
            drift += 1
            print(f"DRIFT: {rel}")
            sys.stdout.writelines(
                difflib.unified_diff(
                    old_text.splitlines(keepends=True),
                    new_text.splitlines(keepends=True),
                    fromfile=f"a/{rel}",
                    tofile=f"b/{rel}",
                )
            )
        else:
            c.path.write_text(new_text, encoding="utf-8")
            print(f"wrote {rel}")
    return 1 if drift else 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(prog="render-theme", description=__doc__)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument(
        "--check",
        action="store_true",
        help="check for drift; exit 1 if any file is out of sync",
    )
    mode.add_argument(
        "--write",
        action="store_true",
        help="render templates in place (default if neither flag given)",
    )
    args = parser.parse_args(argv)
    palette = load_palette()
    check = args.check  # --write is just the default; no special handling
    return process(CONSUMERS, palette, check=check)


if __name__ == "__main__":
    raise SystemExit(main())
