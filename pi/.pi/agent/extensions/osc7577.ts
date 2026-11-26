/**
 * OSC 7577 (Program Resume Protocol, rev 0.5) emitter.
 *
 * Tells vtmux how to relaunch this pi: `#{pane_resume_command}` becomes
 * `pi --session <file>` and `#{pane_resume_cwd}` the session cwd, so mural and
 * pane resume find the exact live session. Wire format (vtmux D35,
 * docs/PROGRAM-RESUME.md):
 *
 *   ESC ] 7577 ; pid=<pid>:time=<unix s>:argv=<b64>,<b64>:cwd=<b64>:mac=<hex> ESC \
 *
 * mac = lowercase hex SHA-256(body-before-":mac=" + ":" + $OSC7577_TOKEN).
 * Shutdown sends `clear=1` in place of argv/cwd. No-op without a token, with an
 * ephemeral session, or without a writable /dev/tty. Never throws or blocks.
 */

import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { createHash } from "node:crypto";
import { closeSync, constants, openSync, writeSync } from "node:fs";

const b64 = (s: string): string => Buffer.from(s, "utf8").toString("base64");

const CONTROL_RE = /[\x00-\x1f\x7f-\x9f]/;
const MAX_BODY_BYTES = 4096;

/**
 * Build one signed OSC 7577 report. `argv` omitted means `clear=1`.
 * Returns null (emit nothing) for control characters in argv/cwd or a body over
 * 4096 bytes: vtmux would drop such a report and, worse, a malformed one must
 * never replace a valid record. A non-absolute cwd is omitted.
 */
export function buildOsc7577(opts: {
	token: string;
	pid: number;
	time: number;
	argv?: string[];
	cwd?: string;
}): string | null {
	const pairs = [`pid=${opts.pid}`, `time=${opts.time}`];
	if (opts.argv) {
		if (opts.argv.length === 0 || opts.argv.some((a) => CONTROL_RE.test(a))) return null;
		pairs.push(`argv=${opts.argv.map(b64).join(",")}`);
		if (opts.cwd && opts.cwd.startsWith("/")) {
			if (CONTROL_RE.test(opts.cwd)) return null;
			pairs.push(`cwd=${b64(opts.cwd)}`);
		} else if (opts.cwd && CONTROL_RE.test(opts.cwd)) {
			return null;
		}
	} else {
		pairs.push("clear=1");
	}
	const prefix = pairs.join(":");
	const mac = createHash("sha256").update(`${prefix}:${opts.token}`).digest("hex");
	const body = `${prefix}:mac=${mac}`;
	if (Buffer.byteLength(body) > MAX_BODY_BYTES) return null;
	return `\x1b]7577;${body}\x1b\\`;
}

/**
 * Write all of `data`, looping over short writes. EAGAIN/EWOULDBLOCK is retried
 * a bounded number of times; returns false (gives up) otherwise, never true on
 * a partial write.
 */
export function writeAll(
	fd: number,
	data: Buffer,
	write: (fd: number, buf: Buffer, off: number, len: number) => number = (f, b, o, l) => writeSync(f, b, o, l),
	maxRetries = 20,
): boolean {
	let off = 0;
	let retries = 0;
	while (off < data.length) {
		try {
			const n = write(fd, data, off, data.length - off);
			if (n > 0) {
				off += n;
				retries = 0;
				continue;
			}
		} catch (e) {
			const code = (e as NodeJS.ErrnoException).code;
			if (code !== "EAGAIN" && code !== "EWOULDBLOCK") return false;
		}
		if (++retries > maxRetries) return false;
		Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 5);
	}
	return true;
}

/** Non-blocking, fail-silent write to the controlling terminal. */
export function writeTty(seq: string): boolean {
	try {
		const fd = openSync("/dev/tty", constants.O_WRONLY | constants.O_NOCTTY | constants.O_NONBLOCK);
		try {
			return writeAll(fd, Buffer.from(seq, "utf8"));
		} finally {
			closeSync(fd);
		}
	} catch {
		return false;
	}
}

function emit(ctx: ExtensionContext | null): void {
	const token = process.env.OSC7577_TOKEN;
	if (!token) return;
	try {
		let argv: string[] | undefined;
		if (ctx) {
			const file = ctx.sessionManager.getSessionFile();
			if (!file) return;
			argv = ["pi", "--session", file];
		}
		const seq = buildOsc7577({
			token,
			pid: process.pid,
			time: Math.floor(Date.now() / 1000),
			argv,
			cwd: ctx?.cwd,
		});
		if (seq) writeTty(seq);
	} catch {
		// fail-silent
	}
}

export default function (pi: ExtensionAPI) {
	// session_start fires for startup, reload, new, resume and fork alike.
	pi.on("session_start", async (_event, ctx) => emit(ctx));
	pi.on("session_shutdown", async (event) => {
		// A reload re-announces from session_start; keep the record meanwhile.
		if (event.reason !== "reload") emit(null);
	});
}
