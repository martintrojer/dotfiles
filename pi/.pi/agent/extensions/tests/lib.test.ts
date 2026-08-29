/**
 * Pure-helper tests for _lib.ts.
 *
 * Run with `node --test` (see `make check-ts-tests`). Node strips the TS types
 * natively, so there is no build step, no runner dependency, and no
 * package.json — matching the rest of the repo, where the extensions are loaded
 * by a globally installed host.
 *
 * Scope: only the side-effect-free helpers. The scheduler, the model picker and
 * the fast-model state file all touch timers, the network or $HOME.
 */

import { test } from "node:test";
import assert from "node:assert/strict";

import {
	type BranchEntryLike,
	blockText,
	conversationTranscript,
	expandProbe,
	formatInterval,
	NAMED_PROBES,
	parseInterval,
} from "../_lib.ts";

// --- parseInterval -------------------------------------------------------

test("parseInterval understands s/m/h suffixes", () => {
	assert.equal(parseInterval("30s"), 30_000);
	assert.equal(parseInterval("5m"), 5 * 60_000);
	assert.equal(parseInterval("2h"), 2 * 3_600_000);
});

test("parseInterval treats a bare number as minutes", () => {
	assert.equal(parseInterval("5"), 5 * 60_000);
	assert.equal(parseInterval("0"), 0);
});

test("parseInterval accepts fractions and uppercase units", () => {
	assert.equal(parseInterval("1.5h"), 90 * 60_000);
	assert.equal(parseInterval("0.5m"), 30_000);
	assert.equal(parseInterval("30S"), 30_000);
	assert.equal(parseInterval("2H"), 2 * 3_600_000);
});

test("parseInterval allows a zero interval (the 5s floor is the caller's job)", () => {
	// /loop and /watch reject anything under MIN_INTERVAL_MS themselves, so the
	// parser deliberately does not treat 0 as invalid — null means "not an
	// interval token" and would make splitInterval swallow the word as prompt text.
	assert.equal(parseInterval("0s"), 0);
});

test("parseInterval rejects garbage", () => {
	for (const bad of [
		"",
		" ",
		"abc",
		"m",
		"5d", // unsupported unit
		"5min", // only a single-letter unit
		"5 m", // whitespace is split off before parsing
		"-5m", // no sign support
		"+5m",
		"1e3", // no exponent support
		".5m", // digits required before the dot
		"5.m", // digits required after the dot
		"5m5",
		"5m ",
		"1,5m",
		"NaN",
		"Infinity",
	]) {
		assert.equal(parseInterval(bad), null, `expected ${JSON.stringify(bad)} to be rejected`);
	}
});

// --- formatInterval ------------------------------------------------------

test("formatInterval picks the largest exact unit", () => {
	assert.equal(formatInterval(3_600_000), "1h");
	assert.equal(formatInterval(7_200_000), "2h");
	assert.equal(formatInterval(5 * 60_000), "5m");
	assert.equal(formatInterval(90 * 60_000), "90m"); // 1.5h is not an exact hour
	assert.equal(formatInterval(30_000), "30s");
	assert.equal(formatInterval(5_000), "5s");
});

test("formatInterval rounds sub-second remainders into seconds", () => {
	assert.equal(formatInterval(1_500), "2s");
	assert.equal(formatInterval(1_499), "1s");
	assert.equal(formatInterval(1), "0s");
});

test("formatInterval(0) reports 0h, since 0 is an exact multiple of an hour", () => {
	// Degenerate but harmless: both call sites format an interval that already
	// passed the 5s floor, so 0 never reaches here.
	assert.equal(formatInterval(0), "0h");
});

test("formatInterval round-trips through parseInterval", () => {
	for (const ms of [5_000, 30_000, 60_000, 90_000, 5 * 60_000, 90 * 60_000, 3_600_000, 6 * 3_600_000]) {
		assert.equal(parseInterval(formatInterval(ms)), ms, `round-trip failed for ${ms}ms`);
	}
});

// --- blockText -----------------------------------------------------------

test("blockText passes strings through untouched", () => {
	assert.equal(blockText("hello"), "hello");
	assert.equal(blockText(""), "");
});

test("blockText joins text blocks with newlines", () => {
	assert.equal(
		blockText([
			{ type: "text", text: "one" },
			{ type: "text", text: "two" },
		]),
		"one\ntwo",
	);
});

test("blockText renders toolCall blocks with their arguments", () => {
	assert.equal(
		blockText([{ type: "toolCall", name: "bash", arguments: { command: "ls" } }]),
		'[tool bash {"command":"ls"}]',
	);
	assert.equal(blockText([{ type: "toolCall", name: "read" }]), "[tool read {}]");
});

test("blockText prefixes toolResult blocks", () => {
	assert.equal(blockText([{ type: "toolResult", text: "out" }]), "[tool result] out");
});

test("blockText returns empty for non-string, non-array input", () => {
	for (const junk of [undefined, null, 0, 42, true, {}, { type: "text", text: "not in an array" }]) {
		assert.equal(blockText(junk), "");
	}
});

test("blockText skips malformed and unknown blocks instead of throwing", () => {
	assert.equal(
		blockText([
			null,
			undefined,
			"a bare string inside the array",
			42,
			{}, // no type
			{ type: "text" }, // no text
			{ type: "text", text: 5 }, // text is not a string
			{ type: "toolCall" }, // no name
			{ type: "toolResult" }, // no text
			{ type: "thinking", text: "hidden" }, // unknown type
			{ type: "text", text: "kept" },
		]),
		"kept",
	);
});

test("blockText keeps empty text blocks as blank lines", () => {
	assert.equal(
		blockText([
			{ type: "text", text: "" },
			{ type: "text", text: "b" },
		]),
		"\nb",
	);
});

// --- conversationTranscript ----------------------------------------------

const msg = (role: string, content: unknown): BranchEntryLike => ({ type: "message", message: { role, content } });

test("conversationTranscript labels roles and blank-line separates turns", () => {
	assert.equal(
		conversationTranscript([msg("user", "hi"), msg("assistant", "hello")], 1000),
		"User: hi\n\nAssistant: hello",
	);
});

test("conversationTranscript skips non-message entries and other roles", () => {
	assert.equal(
		conversationTranscript(
			[
				{ type: "checkpoint" },
				{ type: "summary", message: { role: "user", content: "not a message entry" } },
				{ type: "message" }, // no message
				{ type: "message", message: {} }, // no role
				msg("system", "system prompt"),
				msg("tool", "tool chatter"),
				msg("user", "kept"),
			],
			1000,
		),
		"User: kept",
	);
});

test("conversationTranscript drops whitespace-only turns", () => {
	assert.equal(conversationTranscript([msg("user", "   \n "), msg("assistant", " ok ")], 1000), "Assistant: ok");
});

test("conversationTranscript renders structured content via blockText", () => {
	assert.equal(
		conversationTranscript(
			[
				msg("assistant", [
					{ type: "text", text: "a" },
					{ type: "toolResult", text: "b" },
				]),
			],
			1000,
		),
		"Assistant: a\n[tool result] b",
	);
});

test("conversationTranscript truncation keeps the TAIL", () => {
	const branch = [msg("user", "A".repeat(100)), msg("assistant", "B".repeat(100))];
	const out = conversationTranscript(branch, 50);
	assert.equal(out.length, 50);
	assert.equal(out, "B".repeat(50));
	assert.ok(!out.includes("A"), "oldest turn must be the part that is dropped");
});

test("conversationTranscript leaves output under budget untouched", () => {
	const full = conversationTranscript([msg("user", "hi")], 1000);
	assert.equal(full, "User: hi");
	assert.equal(conversationTranscript([msg("user", "hi")], full.length), full); // boundary: length == budget
});

test("conversationTranscript returns empty for an empty or all-skipped branch", () => {
	assert.equal(conversationTranscript([], 1000), "");
	assert.equal(conversationTranscript([{ type: "checkpoint" }], 1000), "");
});

test("conversationTranscript with budget 0 returns everything (slice(-0) is slice(0))", () => {
	// Documented quirk, not a behavior anyone depends on: every caller passes a
	// positive budget. Recorded so a future guard against it is a deliberate change.
	assert.equal(conversationTranscript([msg("user", "hi")], 0), "User: hi");
});

// --- expandProbe ---------------------------------------------------------

test("expandProbe expands every named probe", () => {
	for (const [name, { command }] of Object.entries(NAMED_PROBES)) {
		assert.equal(expandProbe(name), command);
	}
});

test("expandProbe appends trailing args verbatim", () => {
	assert.equal(expandProbe("gh-pr 1234"), "gh pr checks 1234");
	assert.equal(expandProbe("gh-pr   1234   --watch"), "gh pr checks 1234 --watch");
	assert.equal(expandProbe("git-log HEAD~1"), "git log -1 --format=%H origin/HEAD HEAD~1");
});

test("expandProbe passes unknown commands through unchanged", () => {
	assert.equal(expandProbe("gh pr checks"), "gh pr checks");
	assert.equal(expandProbe("curl -s https://example.com | tail -1"), "curl -s https://example.com | tail -1");
	assert.equal(expandProbe(""), "");
	assert.equal(expandProbe("GH-PR"), "GH-PR"); // names are case-sensitive
	assert.equal(expandProbe("gh-prx 1"), "gh-prx 1");
});

test("expandProbe does not expand inherited Object properties", () => {
	// NAMED_PROBES is a plain object literal, so a bare lookup would find
	// Object.prototype members and "expand" `constructor` to undefined, breaking
	// the `: string` return type. Guarded with Object.hasOwn in _lib.ts.
	for (const name of ["constructor", "toString", "hasOwnProperty", "__proto__"]) {
		assert.equal(expandProbe(name), name);
		assert.equal(expandProbe(`${name} arg`), `${name} arg`);
	}
});
