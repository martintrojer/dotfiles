#!/usr/bin/env python3
"""Focused behavior tests for setup-sunshine.sh."""

from __future__ import annotations

import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).parents[1] / "config/setup-sunshine.sh"

FAKE_FIREWALL = r"""#!/usr/bin/env python3
import json
import os
import sys
from pathlib import Path

root = Path(os.environ["FAKE_FIREWALL_STATE"])
root.mkdir(exist_ok=True)
permanent = root / "permanent"
runtime = root / "runtime"
log = root / "log"
args = sys.argv[1:]
with log.open("a") as stream:
    stream.write(json.dumps(args) + "\n")

def rules(path):
    return set(path.read_text().splitlines()) if path.exists() else set()

def value(prefix):
    return next(arg.removeprefix(prefix) for arg in args if arg.startswith(prefix))

if any(arg.startswith("--get-zone-of-interface=") for arg in args):
    if os.environ.get("FAKE_FIREWALL_ZONE_QUERY_FAIL"):
        raise SystemExit(1)
    print(os.environ.get("FAKE_FIREWALL_ZONE", "home"))
    raise SystemExit
if "--reload" in args:
    runtime.write_text(permanent.read_text() if permanent.exists() else "")
    raise SystemExit
if any(arg.startswith("--query-port=") or arg.startswith("--query-service=") for arg in args):
    raise SystemExit(1)

path = permanent if "--permanent" in args else runtime
current = rules(path)
if any(arg.startswith("--query-rich-rule=") for arg in args):
    raise SystemExit(0 if value("--query-rich-rule=") in current else 1)
if any(arg.startswith("--add-rich-rule=") for arg in args):
    current.add(value("--add-rich-rule="))
    path.write_text("\n".join(sorted(current)) + "\n")
    raise SystemExit
if any(arg.startswith("--remove-rich-rule=") for arg in args):
    current.discard(value("--remove-rich-rule="))
    path.write_text("\n".join(sorted(current)) + ("\n" if current else ""))
    raise SystemExit

raise SystemExit(f"unsupported firewall-cmd arguments: {args}")
"""


class SetupSunshineTests(unittest.TestCase):
    def test_apply_is_scoped_idempotent_verifiable_and_revertible(self) -> None:
        with tempfile.TemporaryDirectory() as raw_tmp:
            root = Path(raw_tmp)
            bin_dir = root / "bin"
            state = root / "state"
            bin_dir.mkdir()
            state.mkdir()

            firewall = bin_dir / "firewall-cmd"
            firewall.write_text(FAKE_FIREWALL)
            firewall.chmod(0o755)
            sudo = bin_dir / "sudo"
            sudo.write_text('#!/bin/sh\nexec "$@"\n')
            sudo.chmod(0o755)

            env = {
                **os.environ,
                "PATH": f"{bin_dir}:{os.environ['PATH']}",
                "FAKE_FIREWALL_STATE": str(state),
                "SUNSHINE_FIREWALL_ZONE": "home",
                "SUNSHINE_FIREWALL_INTERFACE": "eth-test",
                "SUNSHINE_LAN_CIDR": "10.23.0.0/24",
            }

            for command in ([str(SCRIPT)], [str(SCRIPT)], [str(SCRIPT), "--verify"]):
                subprocess.run(
                    command, env=env, check=True, capture_output=True, text=True
                )

            rules = (state / "permanent").read_text().splitlines()
            self.assertEqual(len(rules), 10)
            self.assertTrue(
                all('source address="10.23.0.0/24"' in rule for rule in rules)
            )

            calls = [
                json.loads(line) for line in (state / "log").read_text().splitlines()
            ]
            mutations = [
                call
                for call in calls
                if any(arg.startswith(("--add-", "--remove-")) for arg in call)
            ]
            self.assertTrue(mutations)
            self.assertTrue(all("--zone=home" in call for call in mutations))

            (state / "log").write_text("")
            subprocess.run(
                [str(SCRIPT), "--verify"],
                env=env,
                check=True,
                capture_output=True,
                text=True,
            )
            verify_calls = [
                json.loads(line) for line in (state / "log").read_text().splitlines()
            ]
            rich_rule_queries = [
                call
                for call in verify_calls
                if any(arg.startswith("--query-rich-rule=") for arg in call)
            ]
            self.assertEqual(
                sum("--permanent" not in call for call in rich_rule_queries), 10
            )
            self.assertEqual(
                sum("--permanent" in call for call in rich_rule_queries), 10
            )

            subprocess.run(
                [str(SCRIPT), "--revert"],
                env=env,
                check=True,
                capture_output=True,
                text=True,
            )
            self.assertEqual((state / "permanent").read_text(), "")

            failed_verify = subprocess.run(
                [str(SCRIPT), "--verify"],
                env=env,
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertNotEqual(failed_verify.returncode, 0)

            wrong_zone = subprocess.run(
                [str(SCRIPT)],
                env={**env, "FAKE_FIREWALL_ZONE": "public"},
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertNotEqual(wrong_zone.returncode, 0)
            self.assertEqual((state / "permanent").read_text(), "")

            unassigned_zone = subprocess.run(
                [str(SCRIPT)],
                env={**env, "FAKE_FIREWALL_ZONE_QUERY_FAIL": "1"},
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertNotEqual(unassigned_zone.returncode, 0)
            self.assertIn(
                "eth-test is in firewalld zone 'none', expected 'home'.",
                unassigned_zone.stderr,
            )
            self.assertEqual((state / "permanent").read_text(), "")


if __name__ == "__main__":
    unittest.main()
