/**
 * Tests for osc7577.ts. Run with `node --test`.
 *
 * Pins the wire format against a re-implementation of vtmux's D35 verifier
 * (crates/vtmux-server/src/program_resume.rs): a bad MAC or malformed body is
 * ignored by vtmux with no visible error, so the emitter fails silently.
 */

import { test } from "node:test";
import assert from "node:assert/strict";
import { createHash } from "node:crypto";

import { buildOsc7577, writeAll } from "../osc7577.ts";

const TOKEN = "AbC_def-0123456789abcdefghijklmn";
const NOW = 1795543200;

/** Mirror of vtmux's screen()+validate(); returns the parsed report or null. */
function verify(seq: string, token: string, now: number) {
	const m = /^\x1b\]7577;(.*)\x1b\\$/s.exec(seq);
	if (!m) return null;
	const body = m[1];
	if (Buffer.byteLength(body) > 4096) return null;
	const at = body.lastIndexOf(":mac=");
	if (at < 0) return null;
	const prefix = body.slice(0, at);
	const mac = body.slice(at + 5);
	if (!/^[0-9a-f]{64}$/.test(mac)) return null;
	if (createHash("sha256").update(`${prefix}:${token}`).digest("hex") !== mac) return null;
	const kv = new Map<string, string>();
	for (const pair of prefix.split(":")) {
		const i = pair.indexOf("=");
		const k = pair.slice(0, i);
		const v = pair.slice(i + 1);
		if (i < 1 || !/^[a-z]+$/.test(k) || !/^[A-Za-z0-9_.,+/=-]*$/.test(v) || kv.has(k) || k === "mac") return null;
		kv.set(k, v);
	}
	const uint = (v: string | undefined) => (v && /^[1-9][0-9]*$/.test(v) ? Number(v) : null);
	const pid = uint(kv.get("pid"));
	const time = uint(kv.get("time"));
	if (pid === null || time === null || Math.abs(time - now) > 60) return null;
	const text = (v: string) => {
		const s = Buffer.from(v, "base64").toString("utf8");
		return /[\x00-\x1f\x7f-\x9f]/.test(s) ? null : s;
	};
	let cwd: string | undefined;
	if (kv.has("cwd")) {
		const c = text(kv.get("cwd")!);
		if (!c || !c.startsWith("/")) return null;
		cwd = c;
	}
	const hasArgv = kv.has("argv");
	const hasClear = kv.get("clear") === "1";
	if (hasArgv === hasClear) return null;
	if (hasClear) return { pid, time, clear: true as const };
	const argv = kv
		.get("argv")!
		.split(",")
		.map((e) => text(e));
	if (argv.length > 128 || argv.some((a) => a === null)) return null;
	const first = argv[0]!;
	if (!first || !(first.startsWith("/") || (!first.includes("/") && !first.startsWith("-")))) return null;
	return { pid, time, argv: argv as string[], cwd };
}

test("argv record verifies and round-trips", () => {
	const file = "/home/u/.pi/agent/sessions/--tmp--/2026_abc.jsonl";
	const seq = buildOsc7577({
		token: TOKEN,
		pid: 4242,
		time: NOW,
		argv: ["pi", "--session", file],
		cwd: "/tmp/my proj",
	});
	assert.deepEqual(verify(seq, TOKEN, NOW), {
		pid: 4242,
		time: NOW,
		argv: ["pi", "--session", file],
		cwd: "/tmp/my proj",
	});
});

test("clear record verifies", () => {
	const seq = buildOsc7577({ token: TOKEN, pid: 4242, time: NOW });
	assert.deepEqual(verify(seq, TOKEN, NOW), { pid: 4242, time: NOW, clear: true });
	assert.ok(!seq.includes("argv="));
});

test("spec MAC example (vtmux spec_mac_example)", () => {
	const seq = buildOsc7577({ token: "tok", pid: 74877, time: NOW });
	// Same body vtmux signs with its test token; only the token differs, so
	// recompute the expected MAC independently.
	const mac = createHash("sha256").update("pid=74877:time=1795543200:clear=1:tok").digest("hex");
	assert.equal(seq, `\x1b]7577;pid=74877:time=1795543200:clear=1:mac=${mac}\x1b\\`);
});

test("terminator is ST and starts with OSC 7577", () => {
	const seq = buildOsc7577({ token: TOKEN, pid: 1, time: NOW, argv: ["pi"] });
	assert.ok(seq.startsWith("\x1b]7577;"));
	assert.ok(seq.endsWith("\x1b\\"));
	assert.ok(seq.indexOf(":mac=") === seq.lastIndexOf(":mac="));
});

test("wrong token, stale time, tampered body are rejected by the verifier", () => {
	const seq = buildOsc7577({ token: TOKEN, pid: 7, time: NOW, argv: ["pi", "--session", "/x"], cwd: "/x" });
	assert.equal(verify(seq, "other-token", NOW), null);
	assert.equal(verify(seq, TOKEN, NOW + 61), null);
	assert.equal(verify(seq.replace("pid=7", "pid=8"), TOKEN, NOW), null);
});

test("non-ASCII paths survive base64", () => {
	const seq = buildOsc7577({ token: TOKEN, pid: 9, time: NOW, argv: ["pi", "--session", "/h/ü/ß.jsonl"], cwd: "/h/ü" });
	const r = verify(seq, TOKEN, NOW);
	assert.deepEqual(r && "argv" in r && r.argv, ["pi", "--session", "/h/ü/ß.jsonl"]);
});

test("control characters in argv or cwd yield null", () => {
	for (const bad of ["a\x00b", "a\nb", "a\x1bb", "a\x7fb", "a\x85b"]) {
		assert.equal(buildOsc7577({ token: TOKEN, pid: 1, time: NOW, argv: ["pi", bad] }), null);
		assert.equal(buildOsc7577({ token: TOKEN, pid: 1, time: NOW, argv: ["pi"], cwd: `/${bad}` }), null);
	}
});

test("body over 4096 bytes yields null; under it verifies", () => {
	assert.equal(buildOsc7577({ token: TOKEN, pid: 1, time: NOW, argv: ["pi", "a".repeat(4000)] }), null);
	const ok = buildOsc7577({ token: TOKEN, pid: 1, time: NOW, argv: ["pi", "a".repeat(2000)] });
	assert.ok(ok && verify(ok, TOKEN, NOW));
	// Exact boundary: largest filler that fits is <= 4096, one more group is not.
	let n = 2900;
	const size = (k: number) => {
		const s = buildOsc7577({ token: TOKEN, pid: 1, time: NOW, argv: ["pi", "a".repeat(k)] });
		return s && Buffer.byteLength(s) - "\x1b]7577;\x1b\\".length;
	};
	while (size(n + 1) !== null) n++;
	assert.ok(size(n)! <= 4096);
});

test("relative cwd is omitted", () => {
	const seq = buildOsc7577({ token: TOKEN, pid: 1, time: NOW, argv: ["pi"], cwd: "rel/dir" })!;
	assert.deepEqual(verify(seq, TOKEN, NOW), { pid: 1, time: NOW, argv: ["pi"], cwd: undefined });
});

test("writeAll loops over short writes", () => {
	const got: Buffer[] = [];
	const ok = writeAll(3, Buffer.from("abcdefgh"), (_fd, b, off, len) => {
		const n = Math.min(3, len);
		got.push(b.subarray(off, off + n));
		return n;
	});
	assert.ok(ok);
	assert.equal(Buffer.concat(got).toString(), "abcdefgh");
});

test("writeAll retries EAGAIN then succeeds; persistent EAGAIN gives up", () => {
	let i = 0;
	assert.ok(
		writeAll(3, Buffer.from("abcd"), (_fd, _b, _o, len) => {
			if (i++ % 2 === 0) throw Object.assign(new Error("again"), { code: "EAGAIN" });
			return len;
		}),
	);
	assert.equal(
		writeAll(
			3,
			Buffer.from("abcd"),
			() => {
				throw Object.assign(new Error("again"), { code: "EWOULDBLOCK" });
			},
			2,
		),
		false,
	);
	let first = true;
	assert.equal(
		writeAll(
			3,
			Buffer.from("abcdef"),
			() => {
				if (first) {
					first = false;
					return 2;
				}
				throw Object.assign(new Error("again"), { code: "EAGAIN" });
			},
			2,
		),
		false,
	);
});
