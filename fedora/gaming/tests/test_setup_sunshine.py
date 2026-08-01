#!/usr/bin/env python3
"""Focused behavior tests for setup-sunshine.sh."""

from __future__ import annotations

import json
import os
import shutil
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

# Broad host-wide ports/services left by an older version of the script,
# seeded as `port/proto` or `service:name` lines. The apply path is supposed
# to remove these before adding their LAN-scoped rich-rule replacements.
broad = root / "broad"
if not broad.exists():
    broad.write_text(os.environ.get("FAKE_FIREWALL_BROAD", ""))

def broad_entries():
    return [line for line in broad.read_text().splitlines() if line]

def drop_broad(entry):
    if entry not in broad_entries():
        raise SystemExit(f"removing something that is not present: {entry}")
    broad.write_text("".join(f"{e}\n" for e in broad_entries() if e != entry))

if any(arg.startswith("--get-zone-of-interface=") for arg in args):
    if os.environ.get("FAKE_FIREWALL_ZONE_QUERY_FAIL"):
        raise SystemExit(1)
    print(os.environ.get("FAKE_FIREWALL_ZONE", "home"))
    raise SystemExit
if "--reload" in args:
    runtime.write_text(permanent.read_text() if permanent.exists() else "")
    raise SystemExit
if any(arg.startswith("--query-port=") for arg in args):
    raise SystemExit(0 if value("--query-port=") in broad_entries() else 1)
if any(arg.startswith("--query-service=") for arg in args):
    raise SystemExit(0 if "service:" + value("--query-service=") in broad_entries() else 1)
if any(arg.startswith("--remove-port=") for arg in args):
    drop_broad(value("--remove-port="))
    raise SystemExit
if any(arg.startswith("--remove-service=") for arg in args):
    drop_broad("service:" + value("--remove-service="))
    raise SystemExit

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
    def fixture(self) -> tuple[Path, dict[str, str]]:
        """A scratch firewalld: fake state dir + env pointing the script at it.

        Returns (state dir, env). `state/permanent` holds rich rules,
        `state/broad` holds legacy host-wide ports/services, `state/log` every
        firewall-cmd invocation.
        """
        root = Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, root, True)
        bin_dir = root / "bin"
        state = root / "state"
        bin_dir.mkdir()
        state.mkdir()

        firewall = bin_dir / "firewall-cmd"
        firewall.write_text(FAKE_FIREWALL)
        firewall.chmod(0o755)
        sudo = bin_dir / "sudo"
        # Mimic real sudo's flag handling: -n/-v are the credential probes
        # the script uses to tell "no auth" apart from "no such rule".
        sudo.write_text(
            "#!/bin/sh\n"
            "while [ $# -gt 0 ]; do\n"
            "  case $1 in\n"
            "    -n) shift ;;\n"
            "    -v) exit 0 ;;\n"
            "    *) break ;;\n"
            "  esac\n"
            "done\n"
            "[ $# -eq 0 ] && exit 0\n"
            'exec "$@"\n'
        )
        sudo.chmod(0o755)

        env = {
            **os.environ,
            "PATH": f"{bin_dir}:{os.environ['PATH']}",
            "FAKE_FIREWALL_STATE": str(state),
            "SUNSHINE_FIREWALL_ZONE": "home",
            "SUNSHINE_FIREWALL_INTERFACE": "eth-test",
            "SUNSHINE_LAN_CIDR": "10.23.0.0/24",
        }
        return state, env

    def test_legacy_broad_rules_are_removed_before_scoped_ones_are_added(self) -> None:
        """Migration path: an older version of this script opened the ports
        host-wide. Apply must delete every one of those before installing the
        LAN-scoped rich rules, or the host stays exposed to the whole internet
        while looking correctly configured.
        """
        state, env = self.fixture()
        legacy = [
            *(f"{port}/tcp" for port in (47984, 47989, 47990, 48010)),
            *(f"{port}/udp" for port in (47998, 47999, 48000, 48002, 48010)),
            "service:mdns",
        ]
        (state / "broad").write_text("".join(f"{entry}\n" for entry in legacy))

        subprocess.run(
            [str(SCRIPT)], env=env, check=True, capture_output=True, text=True
        )

        self.assertEqual((state / "broad").read_text(), "")
        self.assertEqual(len((state / "permanent").read_text().splitlines()), 10)

        calls = [json.loads(line) for line in (state / "log").read_text().splitlines()]
        removals = [
            arg
            for call in calls
            for arg in call
            if arg.startswith(("--remove-port=", "--remove-service="))
        ]
        self.assertCountEqual(
            removals,
            [f"--remove-port={entry}" for entry in legacy if entry != "service:mdns"]
            + ["--remove-service=mdns"],
        )
        # Every removal is scoped to the configured zone and is permanent;
        # a runtime-only removal would come back on the next firewalld reload.
        for call in calls:
            if any(
                arg.startswith(("--remove-port=", "--remove-service=")) for arg in call
            ):
                self.assertIn("--zone=home", call)
                self.assertIn("--permanent", call)

        # Second run has nothing left to clean up: the fake fails loudly if the
        # script tries to remove something absent.
        (state / "log").write_text("")
        subprocess.run(
            [str(SCRIPT)], env=env, check=True, capture_output=True, text=True
        )
        repeat = [json.loads(line) for line in (state / "log").read_text().splitlines()]
        self.assertFalse(
            [
                call
                for call in repeat
                if any(arg.startswith(("--remove-", "--add-")) for arg in call)
            ]
        )

    def run_script(self, env: dict[str, str], *args: str, expect_ok: bool = True):
        result = subprocess.run(
            [str(SCRIPT), *args], env=env, check=False, capture_output=True, text=True
        )
        if expect_ok:
            self.assertEqual(result.returncode, 0, result.stderr)
        else:
            self.assertNotEqual(result.returncode, 0, result.stdout)
        return result

    @staticmethod
    def calls(state: Path) -> list[list[str]]:
        return [json.loads(line) for line in (state / "log").read_text().splitlines()]

    def test_apply_installs_lan_scoped_rules_and_is_idempotent(self) -> None:
        state, env = self.fixture()
        self.run_script(env)

        rules = (state / "permanent").read_text().splitlines()
        self.assertEqual(len(rules), 10)
        self.assertTrue(all('source address="10.23.0.0/24"' in rule for rule in rules))
        mutations = [
            call
            for call in self.calls(state)
            if any(arg.startswith(("--add-", "--remove-")) for arg in call)
        ]
        self.assertTrue(mutations)
        self.assertTrue(all("--zone=home" in call for call in mutations))

        # A second apply must not touch anything.
        (state / "log").write_text("")
        self.run_script(env)
        self.assertEqual((state / "permanent").read_text().splitlines(), rules)
        self.assertFalse(
            [
                call
                for call in self.calls(state)
                if any(arg.startswith(("--add-", "--remove-")) for arg in call)
            ]
        )

    def test_verify_checks_every_rule_in_both_scopes(self) -> None:
        state, env = self.fixture()
        self.run_script(env)

        (state / "log").write_text("")
        self.run_script(env, "--verify")
        queries = [
            call
            for call in self.calls(state)
            if any(arg.startswith("--query-rich-rule=") for arg in call)
        ]
        # Runtime and permanent both matter: a rule present only at runtime
        # disappears on reboot, one only permanent is not active yet.
        self.assertEqual(sum("--permanent" not in call for call in queries), 10)
        self.assertEqual(sum("--permanent" in call for call in queries), 10)

    def test_revert_removes_every_rule_and_verify_then_fails(self) -> None:
        state, env = self.fixture()
        self.run_script(env)
        self.run_script(env, "--revert")

        self.assertEqual((state / "permanent").read_text(), "")
        failed = self.run_script(env, "--verify", expect_ok=False)
        self.assertIn("missing runtime rule", failed.stderr)

    def test_apply_refuses_a_zone_that_does_not_own_the_interface(self) -> None:
        state, env = self.fixture()

        wrong = self.run_script(
            {**env, "FAKE_FIREWALL_ZONE": "public"}, expect_ok=False
        )
        self.assertIn("expected 'home'", wrong.stderr)
        self.assertFalse((state / "permanent").exists())

        unassigned = self.run_script(
            {**env, "FAKE_FIREWALL_ZONE_QUERY_FAIL": "1"}, expect_ok=False
        )
        self.assertIn(
            "eth-test is in firewalld zone 'none', expected 'home'.",
            unassigned.stderr,
        )
        self.assertFalse((state / "permanent").exists())

    def test_unavailable_sudo_is_reported_not_read_as_missing_rules(self) -> None:
        """Auth failure must not masquerade as an unconfigured firewall.

        firewall-cmd exits 1 both when a rule is absent and when sudo cannot
        authenticate. Without an explicit credential probe, --verify reports a
        correctly configured host as having zero rules -- the worst direction
        for a firewall check to be wrong in.
        """
        with tempfile.TemporaryDirectory() as raw_tmp:
            bin_dir = Path(raw_tmp) / "bin"
            bin_dir.mkdir()
            # sudo that can never authenticate, like a non-interactive shell
            # with no cached credentials.
            sudo = bin_dir / "sudo"
            sudo.write_text(
                "#!/bin/sh\necho 'sudo: a password is required' >&2\nexit 1\n"
            )
            sudo.chmod(0o755)
            firewall = bin_dir / "firewall-cmd"
            firewall.write_text("#!/bin/sh\nexit 0\n")
            firewall.chmod(0o755)

            result = subprocess.run(
                [str(SCRIPT), "--verify"],
                env={**os.environ, "PATH": f"{bin_dir}:{os.environ['PATH']}"},
                stdin=subprocess.DEVNULL,
                check=False,
                capture_output=True,
                text=True,
            )

            self.assertNotEqual(result.returncode, 0)
            self.assertIn("sudo", result.stderr.lower())
            self.assertNotIn("missing runtime rule", result.stderr)
            self.assertNotIn("missing permanent rule", result.stderr)
            self.assertNotIn("zone 'none'", result.stderr)


if __name__ == "__main__":
    unittest.main()
