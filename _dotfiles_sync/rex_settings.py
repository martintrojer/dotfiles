"""Keep the Rex settings this repo cares about in the com.superlogical.rex domain.

Rex keeps its settings in macOS user defaults, beside window frames, session
state, and update timestamps, so there is no file to link. This repo owns the
keys in REX_SETTINGS plus one custom theme (rex/*.rex-theme.json, exported from
the app after importing Ghostty's theme of the same name). --check reports
drift, --apply writes the owned keys and adds or replaces that theme in
Workspace.customThemes, keeping any other custom themes. No-op off macOS.

Rex caches its settings while running, and may write its own copy back when
it quits. Quit Rex before --apply, or set the value in its Settings afterwards.
"""

from __future__ import annotations

import json
import logging
import platform
import plistlib
import subprocess
from collections.abc import Mapping
from dataclasses import dataclass
from pathlib import Path
from typing import Final, Protocol

from .config import REPO_ROOT, lazy_header

LOGGER: Final[logging.Logger] = logging.getLogger("dotfiles-sync")

ISSUE_ID: Final[str] = "rex-settings"
DOMAIN: Final[str] = "com.superlogical.rex"
THEMES_KEY: Final[str] = "Workspace.customThemes"
THEME_PATH: Final[Path] = REPO_ROOT / "rex" / "catppuccin-mocha.rex-theme.json"

# Mirrors ghostty/.config/ghostty/config. Ghostty's font-thicken, opacity, and
# blur have no Rex equivalent.
REX_SETTINGS: Final[dict[str, object]] = {
    "Workspace.terminalFontName": "GeistMono Nerd Font Mono",
    "Workspace.terminalFontPointSize": 16.0,
    # Ghostty: font-feature = -liga
    "Workspace.terminalAllowsLigatures": False,
    # Ghostty: unfocused-split-opacity. "both" fades the pane surface and its
    # text; "surface" leaves text at full strength, a subtler dim.
    "Workspace.inactivePaneFadeMode": "surface",
}


class Defaults(Protocol):
    def read(self) -> dict[str, object]: ...
    def write(self, key: str, value: object) -> None: ...


class MacDefaults:
    """The real domain, through /usr/bin/defaults so cfprefsd sees each write."""

    def read(self) -> dict[str, object]:
        out = subprocess.run(
            ["defaults", "export", DOMAIN, "-"], capture_output=True, check=True
        ).stdout
        return plistlib.loads(out)

    def write(self, key: str, value: object) -> None:
        # bool before int: bool is an int subclass.
        if isinstance(value, bool):
            arg = ["-bool", "true" if value else "false"]
        elif isinstance(value, float):
            arg = ["-float", repr(value)]
        elif isinstance(value, int):
            arg = ["-int", str(value)]
        elif isinstance(value, bytes):
            arg = ["-data", value.hex()]
        else:
            arg = ["-string", str(value)]
        subprocess.run(["defaults", "write", DOMAIN, key, *arg], check=True)


@dataclass(frozen=True)
class RexSettings:
    owned: Mapping[str, object]
    theme_path: Path

    def _theme(self) -> dict[str, object]:
        return json.loads(self.theme_path.read_text())

    def _wanted(self, current: Mapping[str, object]) -> dict[str, object]:
        """Every key whose stored value differs from what this repo wants."""
        theme = self._theme()
        want: dict[str, object] = {
            **self.owned,
            # The theme only works if the app is pointed at it.
            f"Workspace.{theme['appearance']}ThemeID": theme["id"],
        }
        drift = {k: v for k, v in want.items() if current.get(k) != v}

        raw = current.get(THEMES_KEY)
        themes = json.loads(raw) if isinstance(raw, bytes | str) else []
        if theme not in themes:
            kept = [t for t in themes if t.get("id") != theme["id"]]
            # Compact, as Rex's own encoder writes it.
            drift[THEMES_KEY] = json.dumps(
                [*kept, theme], separators=(",", ":")
            ).encode()
        return drift

    def check(
        self, *, verbose: bool, ignore: set[str], defaults: Defaults | None = None
    ) -> bool:
        if ISSUE_ID in ignore or (defaults is None and platform.system() != "Darwin"):
            return False
        current = (defaults or MacDefaults()).read()
        drift = self._wanted(current)
        header = lazy_header(ISSUE_ID)
        for key, want in drift.items():
            header()
            shown = "<theme list>" if key == THEMES_KEY else json.dumps(want)
            LOGGER.warning(f"DRIFT: {DOMAIN} {key} want={shown} (--ignore {ISSUE_ID})")
        if not drift and verbose:
            LOGGER.debug(f"OK: {DOMAIN} has {', '.join(self.owned)} and its theme")
        return bool(drift)

    def apply(self, *, verbose: bool, defaults: Defaults | None = None) -> None:
        if defaults is None and platform.system() != "Darwin":
            return
        backend = defaults or MacDefaults()
        drift = self._wanted(backend.read())
        for key, value in drift.items():
            backend.write(key, value)
        if drift:
            LOGGER.info(f"Updated {DOMAIN}: {', '.join(drift)} (restart Rex)")
        elif verbose:
            LOGGER.debug(f"OK: {DOMAIN} already up to date")


_SETTINGS: Final[RexSettings] = RexSettings(owned=REX_SETTINGS, theme_path=THEME_PATH)


check_rex_settings = _SETTINGS.check
apply_rex_settings = _SETTINGS.apply
