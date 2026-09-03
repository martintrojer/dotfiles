/**
 * Pure-helper tests for refusal.ts.
 *
 * Scope: the four side-effect-free helpers that carry the precision of the
 * rule. The wiring (`tool_result` -> map -> `tool_call` -> nudge) needs an
 * ExtensionAPI and is exercised by using pi, not here.
 *
 * These matter more than most helper tests: a false positive nudges you about a
 * legitimate `--force` and trains you to ignore the extension, so the negative
 * cases below are the real subject.
 */

import { test } from "node:test";
import assert from "node:assert/strict";

import { actionKey, bypassFlags, commandSegments, isGateOnlyFlag, looksLikeGate } from "../refusal.ts";

// --- commandSegments -----------------------------------------------------

test("commandSegments splits on every shell connector", () => {
	assert.deepEqual(commandSegments("a && b"), ["a", "b"]);
	assert.deepEqual(commandSegments("a || b"), ["a", "b"]);
	assert.deepEqual(commandSegments("a; b"), ["a", "b"]);
	assert.deepEqual(commandSegments("a\nb"), ["a", "b"]);
	assert.deepEqual(commandSegments("a | b"), ["a", "b"]);
});

test("commandSegments drops empties and trims", () => {
	assert.deepEqual(commandSegments("  a  &&   b  "), ["a", "b"]);
	assert.deepEqual(commandSegments(";;"), []);
	assert.deepEqual(commandSegments(""), []);
});

// --- actionKey -----------------------------------------------------------

test("actionKey keys on program plus subcommand, ignoring flags and operands", () => {
	assert.equal(actionKey("git commit -m 'x'"), "git commit");
	assert.equal(actionKey("git commit --no-verify -m 'x'"), "git commit");
	assert.equal(actionKey("git push origin main --force"), "git push");
	assert.equal(actionKey("jj commit -m x"), "jj commit");
});

test("actionKey makes a refused commit and a bypassed commit the same action", () => {
	// The whole mechanism depends on this pair colliding.
	assert.equal(actionKey("git commit -m 'fix'"), actionKey("git commit -m 'fix' --no-verify"));
});

test("actionKey keeps different subcommands apart", () => {
	// A refused `git push` must not arm a nudge for `git commit --no-verify`.
	assert.notEqual(actionKey("git push"), actionKey("git commit"));
	assert.notEqual(actionKey("make check-all"), actionKey("make build"));
});

test("actionKey strips env assignments and command prefixes", () => {
	assert.equal(actionKey("FOO=bar git commit"), "git commit");
	assert.equal(actionKey("sudo make install"), "make install");
	assert.equal(actionKey("env FOO=1 sudo git push"), "git push");
});

test("actionKey handles a bare program", () => {
	assert.equal(actionKey("make"), "make");
	assert.equal(actionKey("pytest"), "pytest");
	assert.equal(actionKey("pytest -x"), "pytest"); // flags are not the action
});

test("actionKey returns null for nothing runnable", () => {
	assert.equal(actionKey(""), null);
	assert.equal(actionKey("   "), null);
	assert.equal(actionKey("FOO=bar"), null); // assignment only, no program
});

// --- bypassFlags ---------------------------------------------------------

test("bypassFlags finds the flags that disable a check", () => {
	assert.deepEqual(bypassFlags("git commit --no-verify"), ["--no-verify"]);
	assert.deepEqual(bypassFlags("git push --force"), ["--force"]);
	assert.deepEqual(bypassFlags("jj commit --ignore-immutable"), ["--ignore-immutable"]);
});

test("bypassFlags matches whole words only", () => {
	// The substring trap: -n must not match -name, and --force must not be
	// found inside --force-with-lease's own entry by accident.
	assert.deepEqual(bypassFlags("find . -name x"), []);
	assert.deepEqual(bypassFlags("grep -nv pattern file"), []);
	assert.deepEqual(bypassFlags("git push --force-with-lease"), ["--force-with-lease"]);
	assert.deepEqual(bypassFlags("echo '--no-verify'"), []); // quoted, not a flag position
});

test("bypassFlags is empty for ordinary commands", () => {
	assert.deepEqual(bypassFlags("git commit -m 'message'"), []);
	assert.deepEqual(bypassFlags("make check-all"), []);
	assert.deepEqual(bypassFlags(""), []);
});

test("bypassFlags dedupes", () => {
	assert.deepEqual(bypassFlags("git push --force --force"), ["--force"]);
});

// --- isGateOnlyFlag ------------------------------------------------------

test("isGateOnlyFlag separates skip-a-check-only from everyday flags", () => {
	// These have no purpose but getting past a check, so a bare nonzero exit is
	// enough context — which is what catches a hook that is only `exit 1` and
	// prints nothing for looksLikeGate to match.
	assert.ok(isGateOnlyFlag("--no-verify"));
	assert.ok(isGateOnlyFlag("--skip-hooks"));
	assert.ok(isGateOnlyFlag("--ignore-immutable"));

	// These have legitimate everyday uses, so they need actual gate wording in
	// the preceding failure before they mean anything.
	assert.ok(!isGateOnlyFlag("--force"));
	assert.ok(!isGateOnlyFlag("--force-with-lease"));
	assert.ok(!isGateOnlyFlag("--ignore-working-copy"));
	assert.ok(!isGateOnlyFlag("-f"));
	assert.ok(!isGateOnlyFlag("--unknown-flag"));
});

// --- looksLikeGate -------------------------------------------------------

test("looksLikeGate recognizes a gate declining", () => {
	assert.ok(looksLikeGate("pre-commit hook failed"));
	assert.ok(looksLikeGate("husky > commit-msg hook exited with 1"));
	assert.ok(looksLikeGate("Error: Commit abc123 is immutable"));
	assert.ok(looksLikeGate("remote: rejected: branch is protected"));
	assert.ok(looksLikeGate("2 files would be reformatted"));
	assert.ok(looksLikeGate("REFUSED to overwrite")); // case-insensitive
	assert.ok(looksLikeGate("abort: commit is immutable")); // jj/hg wording
});

test("looksLikeGate ignores a command that merely failed", () => {
	// This is the precision boundary: a nonzero exit is not a refusal.
	assert.ok(!looksLikeGate("No such file or directory"));
	assert.ok(!looksLikeGate("bash: frobnicate: command not found"));
	assert.ok(!looksLikeGate("ModuleNotFoundError: No module named 'foo'"));
	assert.ok(!looksLikeGate("Command exited with code 1"));
	assert.ok(!looksLikeGate(""));
});

test("looksLikeGate ignores pi's own abort and timeout statuses", () => {
	// bash.js throws these too, so they reach the handler as isError: true.
	// Neither is a gate refusing anything.
	assert.ok(!looksLikeGate("Command aborted"));
	assert.ok(!looksLikeGate("Command timed out after 30 seconds"));
});

test("looksLikeGate needs gate WORDING, not a mention of a gate", () => {
	// pi throws the tool's whole output as the error message, so a failing
	// command that merely talks about hooks must not arm a refusal.
	assert.ok(!looksLikeGate("usage: git help hooks"));
	assert.ok(!looksLikeGate(".git/hooks/pre-commit.sample"));
	assert.ok(!looksLikeGate("grep: hooks: Is a directory"));
	assert.ok(!looksLikeGate("installing hook scripts"));
	// And an ordinary failure mentioning a blocked/immutable-ish word in prose.
	assert.ok(!looksLikeGate("TypeError: cannot read property 'immutable' of undefined"));
});
