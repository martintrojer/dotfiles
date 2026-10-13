"""Check and merge owned keys into a JSON object some other tool rewrites.

Callers name the file, the issue id, and the keys. --check reports drift,
--apply writes those keys, and every other key stays the tool's. Dict values
merge one level deep; anything else replaces.
"""

from __future__ import annotations

import json
import logging
from collections.abc import Mapping
from dataclasses import dataclass
from pathlib import Path
from typing import Final

from .config import lazy_header

LOGGER: Final[logging.Logger] = logging.getLogger("dotfiles-sync")


def _merged(current: object, want: object) -> object:
    """want laid over current; dicts merge one level, anything else replaces."""
    if isinstance(want, dict) and isinstance(current, dict):
        return {**current, **want}
    return want


@dataclass(frozen=True)
class JsonKeys:
    issue_id: str
    path: Path
    owned: Mapping[str, object]
    # Match the tool's own writer. A different ending makes its next save
    # rewrite the whole file.
    trailing_newline: bool = False

    def _load(self, path: Path) -> dict[str, object] | None:
        """Current object: {} when missing, None when unusable (already reported)."""
        if not path.exists():
            return {}
        try:
            data = json.loads(path.read_text())
        except (OSError, json.JSONDecodeError) as exc:
            lazy_header(self.issue_id)()
            LOGGER.warning(f"UNREADABLE: {path}: {exc} (--ignore {self.issue_id})")
            return None
        if not isinstance(data, dict):
            lazy_header(self.issue_id)()
            LOGGER.warning(
                f"INVALID: {path} is not a JSON object (--ignore {self.issue_id})"
            )
            return None
        return data

    def _drift(self, current: Mapping[str, object]) -> list[str]:
        return [
            key
            for key, want in self.owned.items()
            if current.get(key) != _merged(current.get(key), want)
        ]

    def check(self, target: Path, *, verbose: bool, ignore: set[str]) -> bool:
        if self.issue_id in ignore:
            return False
        path = target / self.path
        current = self._load(path)
        if current is None:
            return True
        drift = self._drift(current)
        if drift:
            header = lazy_header(self.issue_id)
            for key in drift:
                header()
                LOGGER.warning(
                    f"DRIFT: {path} {key}={json.dumps(current.get(key))} "
                    f"want={json.dumps(self.owned[key])} (--ignore {self.issue_id})"
                )
            return True
        if verbose:
            LOGGER.debug(f"OK: {path} has {', '.join(self.owned)}")
        return False

    def apply(self, target: Path, *, verbose: bool) -> None:
        path = target / self.path
        current = self._load(path)
        if current is None:
            return
        drift = self._drift(current)
        if not drift:
            if verbose:
                LOGGER.debug(f"OK: {path} already has {', '.join(self.owned)}")
            return
        current.update(
            {key: _merged(current.get(key), self.owned[key]) for key in drift}
        )
        path.parent.mkdir(parents=True, exist_ok=True)
        text = json.dumps(current, indent=2)
        if self.trailing_newline:
            text += "\n"
        path.write_text(text)
        LOGGER.info(f"Updated {path}: {', '.join(drift)}")
