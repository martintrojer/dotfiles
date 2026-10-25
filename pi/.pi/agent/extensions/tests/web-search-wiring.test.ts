/**
 * Wiring tests for web-search.ts: adapter -> VendorHttpError -> classify ->
 * writeMarker -> readState, through the real tool with fetch mocked.
 *
 * Bug-driven: the review panel (2026-10-25) found that a failed state write
 * could throw from the cleanup path and abort failover, and showed that losing
 * the HTTP status or breaking the marker read/write passed every pure-helper
 * test. All of these are silent: another vendor still answers.
 *
 * A separate file because it mutates process-wide globals (HOME, fetch,
 * Math.random); node --test runs each file in its own process.
 */

import { after, beforeEach, test } from "node:test";
import assert from "node:assert/strict";
import { chmodSync, mkdirSync, mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import register from "../web-search.ts";

const home = mkdtempSync(join(tmpdir(), "web-search-test-"));
const stateDir = join(home, ".pi", "agent", "extension-state");
const statePath = join(stateDir, "web-search.json");
process.env.HOME = home;
process.env.BRAVE_SEARCH_API_KEY = "test-brave";
process.env.TAVILY_API_KEY = "test-tavily";
delete process.env.EXA_API_KEY;

const realFetch = globalThis.fetch;
const realRandom = Math.random;
let calls: string[] = [];
globalThis.fetch = (async (url: string | URL) => {
	const host = new URL(String(url)).host;
	calls.push(host);
	if (host.includes("brave")) return new Response('{"error":"USAGE_LIMIT_EXCEEDED"}', { status: 402 });
	return new Response(JSON.stringify({ results: [{ title: "t", url: "https://example.com/" }] }), { status: 200 });
}) as typeof fetch;
// Keyed order is brave, tavily; rng ~1 keeps input order, so brave is tried first.
Math.random = () => 0.99;

type Tool = { execute: (...args: unknown[]) => Promise<{ details: { vendor?: string }; isError?: boolean }> };
let tool: Tool | undefined;
register({ registerTool: (t: Tool) => (tool = t) } as never);
const search = () => (tool as Tool).execute("id", { query: "q" }, undefined, undefined, undefined);

beforeEach(() => {
	calls = [];
	chmodSync(home, 0o700);
	rmSync(join(home, ".pi"), { recursive: true, force: true });
});

after(() => {
	globalThis.fetch = realFetch;
	Math.random = realRandom;
	rmSync(home, { recursive: true, force: true });
});

test("a quota error persists a marker, and the next call skips that vendor", async () => {
	const first = await search();
	assert.equal(first.details.vendor, "tavily");
	assert.deepEqual(calls, ["api.search.brave.com", "api.tavily.com"]);
	const state = JSON.parse(readFileSync(statePath, "utf8"));
	assert.equal(state.brave?.reason, "quota");

	calls = [];
	const second = await search();
	assert.equal(second.details.vendor, "tavily");
	assert.deepEqual(calls, ["api.tavily.com"]);
});

test("an unwritable state directory never aborts failover", async () => {
	mkdirSync(stateDir, { recursive: true });
	chmodSync(stateDir, 0o000);
	try {
		const result = await search();
		assert.equal(result.details.vendor, "tavily");
		assert.deepEqual(calls, ["api.search.brave.com", "api.tavily.com"]);
	} finally {
		chmodSync(stateDir, 0o700);
	}
});
