#!/usr/bin/env python3
"""Focused regression tests for the waybar weather cache and failure policy.

Scope per docs/DECISIONS.md ("No unit tests for the control-plane and helper
scripts" + its 2026-08-01 amendment): the cache round-trip, because a cache
that silently fails to load renders an empty module that looks exactly like
"no network" -- the user cannot tell a bug from the weather being unavailable.
Plus ``guarded_render``, which is now the thing standing between a raised
exception and a blank bar.

``compact_condition``'s 40-branch code table and the bulk of ``parse_weather``
are deliberately not covered: a wrong branch there is visible in the bar the
moment you look at it, and the table is data, not logic.
"""

from __future__ import annotations

import dataclasses
import importlib.machinery
import importlib.util
import io
import json
import sys
import tempfile
import time
import types
import unittest
from contextlib import redirect_stdout
from pathlib import Path
from typing import ClassVar
from unittest import mock

SCRIPTS = Path(__file__).resolve().parent.parent


def load_script(name: str, path: Path) -> types.ModuleType:
    """Import a colocated script by path (they have no .py extension)."""
    loader = importlib.machinery.SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    assert spec is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    loader.exec_module(module)
    return module


sys.path.insert(0, str(SCRIPTS))
weather = load_script("waybar_weather_under_test", SCRIPTS / "weather")
state = load_script("waybar_state_under_test", SCRIPTS / "_state.py")

FULL = {
    "fetched_at": 1700000000,
    "place": "Oslo",
    "temp_c": 2,
    "feels_like_c": -3,
    "condition": "Snow",
    "summary": "Light snow",
    "wind": "11 km/h",
    "humidity": "88",
    "uv_index": "1",
    "sunrise": "08:12 AM",
    "sunset": "04:03 PM",
    "min_temp_c": -4,
    "max_temp_c": 3,
    "rain_chance": 0,
    "snow_chance": 80,
    "precip_mm": 1.2,
    "wind_direction": "NNE",
    "visibility_km": 4,
}

# Split by whether the dataclass gives the field a default, rather than by a
# hand-maintained list -- a hand-maintained list is exactly what broke before.
REQUIRED_FIELDS = [
    f.name
    for f in dataclasses.fields(weather.Weather)
    if f.default is dataclasses.MISSING
]
OPTIONAL_FIELDS = [
    f.name
    for f in dataclasses.fields(weather.Weather)
    if f.default is not dataclasses.MISSING
]


class WeatherCacheLoading(unittest.TestCase):
    def read(self, text: str | None):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "weather.json"
            if text is not None:
                path.write_text(text, encoding="utf-8")
            with mock.patch.object(weather, "CACHE_FILE", path):
                return weather.read_cache()

    def test_a_full_cache_round_trips(self) -> None:
        cached = self.read(json.dumps(FULL))
        assert cached is not None
        self.assertEqual(cached.place, "Oslo")
        self.assertEqual(cached.snow_chance, 80)
        self.assertEqual(cached.visibility_km, 4)

    def test_an_old_schema_cache_missing_every_optional_field_still_loads(self) -> None:
        # This is the regression: the old read_cache hand-patched each optional
        # name to None before Weather(**data), so forgetting one raised a
        # TypeError that the surrounding except swallowed -- silently throwing
        # the cache away and rendering an empty module.
        cached = self.read(json.dumps({k: FULL[k] for k in REQUIRED_FIELDS}))
        assert cached is not None
        self.assertEqual(cached.place, "Oslo")
        self.assertIsNone(cached.visibility_km)

    def test_dropping_any_one_optional_field_never_discards_the_cache(self) -> None:
        # The old bug was per-field, so check per-field: a cache written before
        # any single optional field existed must still load. Fails if a `= None`
        # default is ever removed from the dataclass.
        self.assertTrue(OPTIONAL_FIELDS, "expected optional Weather fields")
        for key in OPTIONAL_FIELDS:
            with self.subTest(dropped=key):
                partial = {k: v for k, v in FULL.items() if k != key}
                self.assertIsNotNone(
                    self.read(json.dumps(partial)),
                    f"dropping {key} discarded the cache",
                )

    def test_unwritable_missing_and_corrupt_caches_are_none_not_tracebacks(
        self,
    ) -> None:
        self.assertIsNone(self.read(None))
        self.assertIsNone(self.read("{ truncated"))
        self.assertIsNone(self.read("[1, 2]"))
        self.assertIsNone(self.read('{"place": "Oslo"}'))

    def test_unknown_extra_keys_do_not_raise(self) -> None:
        # A field written by a newer script must not escape as a TypeError.
        # Dropping the cache is acceptable, tolerating it is better; only the
        # traceback is forbidden, so do not pin either outcome.
        self.read(json.dumps({**FULL, "from_the_future": 1}))

    def test_write_then_read_is_the_identity(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "weather.json"
            original = weather.Weather(**FULL)
            with (
                mock.patch.object(weather, "CACHE_FILE", path),
                mock.patch.object(weather, "CACHE_DIR", Path(tmp)),
            ):
                weather.write_cache(original)
                self.assertEqual(weather.read_cache(), original)


class WeatherRendering(unittest.TestCase):
    def render(self, cache) -> dict:
        buf = io.StringIO()
        with (
            mock.patch.object(weather, "maybe_refresh", lambda _c: None),
            redirect_stdout(buf),
        ):
            weather.render(cache)
        return json.loads(buf.getvalue())

    def test_a_stale_cache_renders_the_collapsed_payload(self) -> None:
        stale = weather.Weather(
            **{**FULL, "fetched_at": int(time.time()) - weather.CACHE_MAX_AGE - 1}
        )
        self.assertEqual(self.render(stale)["class"], ["empty"])
        self.assertEqual(self.render(None)["class"], ["empty"])

    def test_a_fresh_cache_renders_the_place_and_temperature(self) -> None:
        fresh = weather.Weather(**{**FULL, "fetched_at": int(time.time())})
        payload = self.render(fresh)
        self.assertEqual(payload["class"], ["ambient"])
        self.assertEqual(payload["text"], "Snow 2°")
        self.assertIn("Oslo", payload["tooltip"])


class GuardedRender(unittest.TestCase):
    """The bar is a JSON pipe: a renderer that raises must still emit JSON."""

    FALLBACK: ClassVar[dict[str, object]] = {
        "text": "",
        "tooltip": "",
        "class": ["empty"],
    }

    def run_guarded(self, render):
        buf = io.StringIO()
        with tempfile.TemporaryDirectory() as tmp:
            with (
                mock.patch.object(state, "STATE_DIR", Path(tmp)),
                redirect_stdout(buf),
            ):
                rc = state.guarded_render("probe", render, self.FALLBACK)
            logs = list(Path(tmp).glob("*.log"))
            log_text = logs[0].read_text(encoding="utf-8") if logs else ""
        return rc, buf.getvalue(), log_text

    def test_a_healthy_renderer_is_passed_through_untouched(self) -> None:
        rc, out, log = self.run_guarded(lambda: (print('{"text": "ok"}'), 0)[1])
        self.assertEqual((rc, out.strip()), (0, '{"text": "ok"}'))
        self.assertEqual(log, "")

    def test_an_expected_failure_emits_parseable_fallback_json(self) -> None:
        def boom() -> int:
            raise OSError("sysfs went away")

        rc, out, log = self.run_guarded(boom)
        self.assertEqual(rc, 0)
        self.assertEqual(json.loads(out), self.FALLBACK)
        self.assertIn("OSError", log)

    def test_every_expected_error_is_caught(self) -> None:
        for exc in state.EXPECTED_ERRORS:
            with self.subTest(exc=exc.__name__):

                def boom(exc=exc) -> int:
                    raise exc("boom")

                rc, out, _ = self.run_guarded(boom)
                self.assertEqual(rc, 0)
                self.assertEqual(json.loads(out), self.FALLBACK)

    def test_a_programmer_error_still_crashes_loudly(self) -> None:
        def boom() -> int:
            raise TypeError("wrong argument type")

        with self.assertRaises(TypeError):
            self.run_guarded(boom)

    def test_breadcrumbs_are_throttled_to_one_per_window(self) -> None:
        with (
            tempfile.TemporaryDirectory() as tmp,
            mock.patch.object(state, "STATE_DIR", Path(tmp)),
        ):
            for _ in range(3):
                state.log_error("probe", "same failure")
            lines = (Path(tmp) / "probe.log").read_text(encoding="utf-8")
        self.assertEqual(len(lines.strip().splitlines()), 1)

    def test_an_unwritable_state_dir_never_breaks_the_logger(self) -> None:
        with mock.patch.object(state, "STATE_DIR", Path("/proc/nope/waybar")):
            state.log_error("probe", "message")  # must not raise


if __name__ == "__main__":
    unittest.main()
