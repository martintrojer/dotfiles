"""Behavior tests for the Rex defaults merge.

The silent failure: --apply rewriting Workspace.customThemes and dropping
themes imported in the app, or storing the list in a shape Rex cannot decode.
Rex would just fall back to its default theme.
"""

from __future__ import annotations

import json
import unittest

from _dotfiles_sync import rex_settings
from _dotfiles_sync.rex_settings import LOGGER, THEMES_KEY


class FakeDefaults:
    def __init__(self, data: dict[str, object]) -> None:
        self.data = data

    def read(self) -> dict[str, object]:
        return dict(self.data)

    def write(self, key: str, value: object) -> None:
        self.data[key] = value


class RexSettingsTest(unittest.TestCase):
    def test_apply_adds_theme_and_keeps_others(self) -> None:
        other = {"id": "custom-other", "name": "Other", "appearance": "light"}
        fake = FakeDefaults(
            {
                THEMES_KEY: json.dumps([other]).encode(),
                "Workspace.lightThemeID": "custom-other",
                "NSWindow Frame Workspace": "kept",
            }
        )
        with self.assertLogs(LOGGER):
            self.assertTrue(
                rex_settings.check_rex_settings(
                    verbose=False, ignore=set(), defaults=fake
                )
            )
        with self.assertLogs(LOGGER):
            rex_settings.apply_rex_settings(verbose=False, defaults=fake)

        theme = json.loads(rex_settings.THEME_PATH.read_text())
        stored = fake.data[THEMES_KEY]
        assert isinstance(stored, bytes)  # Rex reads this key as NSData
        self.assertEqual(json.loads(stored), [other, theme])
        self.assertEqual(fake.data["Workspace.darkThemeID"], theme["id"])
        self.assertEqual(fake.data["Workspace.lightThemeID"], "custom-other")
        self.assertEqual(fake.data["NSWindow Frame Workspace"], "kept")
        for key, want in rex_settings.REX_SETTINGS.items():
            self.assertEqual(fake.data[key], want)
        self.assertFalse(
            rex_settings.check_rex_settings(verbose=False, ignore=set(), defaults=fake)
        )

    def test_apply_replaces_stale_copy_of_theme(self) -> None:
        theme = json.loads(rex_settings.THEME_PATH.read_text())
        stale = {**theme, "name": "old"}
        fake = FakeDefaults({THEMES_KEY: json.dumps([stale]).encode()})
        with self.assertLogs(LOGGER):
            rex_settings.apply_rex_settings(verbose=False, defaults=fake)
        stored = fake.data[THEMES_KEY]
        assert isinstance(stored, bytes)
        self.assertEqual(json.loads(stored), [theme])


if __name__ == "__main__":
    unittest.main()
