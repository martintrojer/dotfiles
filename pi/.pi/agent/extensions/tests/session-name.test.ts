/**
 * Tests for _session-name.ts. Run with `node --test`.
 *
 * The digest bug these pin failed silently: keeping only the transcript tail
 * named long sessions after their last few turns (commit, release, teardown).
 */

import { test } from "node:test";
import assert from "node:assert/strict";

import type { BranchEntryLike } from "../_lib.ts";
import {
	autoNameDue,
	extractName,
	ownedNameRecord,
	NAME_RECORD_TYPE,
	sessionDigest,
	turnTexts,
} from "../_session-name.ts";

const msg = (role: string, text: string): BranchEntryLike => ({
	type: "message",
	message: { role, content: [{ type: "text", text }] },
});

function longSession(prompts: number): BranchEntryLike[] {
	const branch = [msg("user", "GOAL: add a search index to museum")];
	for (let i = 1; i < prompts; i++) {
		branch.push(msg("assistant", `reply ${i} ${"x".repeat(2000)}`));
		branch.push(msg("user", `prompt ${i}`));
	}
	branch.push(msg("assistant", "LAST REPLY"));
	branch.push(msg("user", "squash and push"));
	return branch;
}

test("sessionDigest returns the full transcript when it fits", () => {
	const branch = [msg("user", "hi"), msg("assistant", "hello")];
	assert.equal(sessionDigest(branch, 1000), "User: hi\n\nAssistant: hello");
});

test("sessionDigest keeps the first prompt and the last turns of a long session", () => {
	const out = sessionDigest(longSession(30), 12000);
	assert.ok(out.length <= 12000, `digest is ${out.length} chars`);
	assert.match(out, /^First user prompt:\nGOAL: add a search index to museum/);
	assert.match(out, /- squash and push/);
	assert.match(out, /Last assistant reply:\nLAST REPLY$/);
	assert.doesNotMatch(out, /x{500}/); // assistant prose is clipped, not carried whole
});

test("sessionDigest drops middle prompts, not the head or tail, when they overflow", () => {
	const out = sessionDigest(longSession(400), 3000);
	assert.ok(out.length <= 3000, `digest is ${out.length} chars`);
	assert.match(out, /GOAL: add a search index/);
	assert.match(out, /- prompt 1\n/);
	assert.match(out, /- squash and push/);
	assert.match(out, /- … \d+ prompts omitted …/);
});

test("extractName reads the JSON reply", () => {
	assert.equal(extractName('Sure: {"name":"Museum Search Index"}'), "Museum Search Index");
});

test("extractName keeps apostrophes inside a name but strips wrapping quotes", () => {
	assert.equal(extractName(`{"name":"Don't Strip Apostrophes"}`), "Don't Strip Apostrophes");
	assert.equal(extractName("'Wrapped Name'"), "Wrapped Name");
});

test("extractName soft-truncates at a word boundary", () => {
	const name = extractName(`{"name":"${"Word ".repeat(20)}"}`);
	assert.ok(name.length <= 60);
	assert.ok(name.endsWith("Word"));
});

// --- auto-naming policy ----------------------------------------------------

const rec = (name: string, prompts: number) => ({
	type: "custom",
	customType: NAME_RECORD_TYPE,
	data: { name, prompts },
});

test("autoNameDue names an unnamed session once it has a prompt and a reply", () => {
	assert.equal(autoNameDue(undefined, undefined, 1, 0), false);
	assert.equal(autoNameDue(undefined, undefined, 1, 1), true);
});

test("autoNameDue never touches a name set by /name or --name", () => {
	assert.equal(autoNameDue("Mine", undefined, 500, 500), false); // no record at all
	assert.equal(autoNameDue("Mine", { name: "Auto", prompts: 1 }, 500, 500), false); // renamed after we named it
});

test("autoNameDue refreshes its own name at 10, then 3x the count it was named at", () => {
	const due = (named: number, now: number) => autoNameDue("Auto", { name: "Auto", prompts: named }, now, now);
	assert.equal(due(1, 9), false);
	assert.equal(due(1, 10), true);
	assert.equal(due(10, 29), false);
	assert.equal(due(10, 30), true);
	assert.equal(due(30, 90), true);
	assert.equal(due(90, 269), false);
});

const info = (name: string) => ({ type: "session_info", name });

test("ownedNameRecord returns the latest record when it follows the latest name", () => {
	const entries = [info("Old"), rec("Old", 1), { type: "message" }, info("New"), rec("New", 10)];
	assert.deepEqual(ownedNameRecord(entries), { name: "New", prompts: 10 });
	assert.deepEqual(ownedNameRecord([info("Same"), rec("Same", 1), rec("Same", 10)]), { name: "Same", prompts: 10 }); // refresh kept the name
});

test("ownedNameRecord gives up ownership to a later /name, even to the same string", () => {
	assert.equal(ownedNameRecord([info("Auto"), rec("Auto", 1), info("Mine")]), undefined);
	assert.equal(ownedNameRecord([info("Auto"), rec("Auto", 1), info("Auto")]), undefined);
	assert.equal(ownedNameRecord([info("Pinned")]), undefined); // --name, no record
	assert.equal(ownedNameRecord([info("X"), { type: "custom", customType: NAME_RECORD_TYPE, data: {} }]), undefined);
});

test("turnTexts counts tool-call-only assistant turns as no reply", () => {
	const toolOnly: BranchEntryLike = {
		type: "message",
		message: { role: "assistant", content: [{ type: "toolCall", name: "bash" }] },
	};
	assert.deepEqual(turnTexts([msg("user", "go"), toolOnly]), { prompts: ["go"], replies: [] });
});
