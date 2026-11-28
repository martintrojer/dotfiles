/**
 * `/star` + session recorder for `pi-restore` (local-bin), the picker that
 * relaunches pi sessions after a reboot, crash, or mux teardown (vtmux or Rex).
 *
 * Records every human-driven pi session in ~/.pi/agent/extension-state/restore.json
 * (override: env PI_RESTORE_STATE=<path>), a map keyed by session id:
 *   { file, cwd, session, mux, pid, startedAt, lastActiveAt, starred, state }
 * state is "live" | "lost" | "closed"; mux is "tmux" | "rex" | null.
 * Skipped: MU_MANAGED_AGENT=1 and ephemeral sessions.
 *
 * Lost vs closed mechanism shipped: PRIMARY, a process.on SIGHUP/SIGTERM listener
 * that synchronously (writeFileSync + renameSync) writes state "lost". pi reports
 * those exits as session_shutdown reason "quit", the same as /quit, so the listener
 * is the only discriminator. session_shutdown without a prior signal marks the entry
 * "closed" (/quit, /new, /resume, /fork), and unstarred closed entries are deleted.
 * A crash or SIGKILL leaves "live" with a dead pid; the picker treats that as lost.
 *
 * Measured (isolated tmux servers, private sockets; 99 runs, 0 closed or deleted):
 * - Direct SIGHUP/SIGTERM to the pi process: the listener's write lands, state "lost".
 * - tmux teardown (kill-server, kill-window, kill-pane): pi usually exits on stdin end
 *   before tmux's SIGHUP arrives, so the listener never runs and no session_shutdown
 *   fires. The entry stays "live" with a dead pid (60/60 runs). The picker's
 *   live-with-dead-pid => lost rule is therefore load-bearing, not just a crash fallback.
 * - No teardown path marks "closed" or deletes: stdin end and terminal errors exit
 *   without session_shutdown, and signals set `signalled` before it.
 * So the listener covers direct signals only; tmux teardown relies on the picker rule.
 *
 * Usage:  /star    toggle the star on the current session (starred stays listed after close)
 */

import { randomBytes } from "node:crypto";
import { execFileSync } from "node:child_process";
import { mkdirSync, readFileSync, renameSync, rmSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

type RestoreState = "live" | "lost" | "closed";
type MuxKind = "tmux" | "rex" | null;

interface Entry {
	file: string;
	cwd: string;
	session: string | null;
	mux: MuxKind;
	pid: number;
	startedAt: number;
	lastActiveAt: number;
	starred: boolean;
	state: RestoreState;
}

type Store = Record<string, Entry>;

// Resolved per call so tests can set the env after import.
const statePath = () =>
	process.env.PI_RESTORE_STATE || join(homedir(), ".pi", "agent", "extension-state", "restore.json");

function readStore(): Store {
	try {
		const parsed = JSON.parse(readFileSync(statePath(), "utf8"));
		return parsed && typeof parsed === "object" && !Array.isArray(parsed) ? (parsed as Store) : {};
	} catch {
		return {};
	}
}

/** Synchronous read-modify-write through temp + rename: never a torn file. */
function update(fn: (store: Store) => void): void {
	const path = statePath();
	const tmp = `${path}.${process.pid}.${randomBytes(4).toString("hex")}.tmp`;
	try {
		const store = readStore();
		fn(store);
		mkdirSync(dirname(path), { recursive: true });
		writeFileSync(tmp, `${JSON.stringify(store, null, "\t")}\n`);
		renameSync(tmp, path);
	} catch {
		// Best-effort: recording must never break pi.
		try {
			rmSync(tmp, { force: true });
		} catch {}
	}
}

/** Current mux and session label (inner mux wins: vtmux over Rex). */
function muxContext(): { mux: MuxKind; session: string | null } {
	const pane = process.env.TMUX_PANE;
	if (process.env.TMUX && pane) {
		try {
			const name = execFileSync("tmux", ["display", "-p", "-t", pane, "#S"], {
				encoding: "utf8",
				timeout: 2000,
				stdio: ["ignore", "pipe", "ignore"],
			}).trim();
			return { mux: "tmux", session: name || null };
		} catch {
			return { mux: "tmux", session: null };
		}
	}
	if (process.env.REX_BLOCK) {
		try {
			const out = execFileSync("rex", ["session", "inspect", "--json"], {
				encoding: "utf8",
				timeout: 2000,
				stdio: ["ignore", "pipe", "ignore"],
			});
			const label = JSON.parse(out)?.label;
			return { mux: "rex", session: typeof label === "string" && label ? label : null };
		} catch {
			return { mux: "rex", session: null };
		}
	}
	return { mux: null, session: null };
}

export default function (pi: ExtensionAPI) {
	if (process.env.MU_MANAGED_AGENT === "1") return;

	let sessionId: string | null = null;
	let signalled = false;
	let listening = false;

	const onSignal = () => {
		signalled = true;
		const id = sessionId;
		if (id) {
			update((store) => {
				if (store[id]) store[id].state = "lost";
			});
		}
		// Not exiting here: pi's own handler runs and shuts down as usual.
	};

	const unlisten = () => {
		if (!listening) return;
		process.off("SIGHUP", onSignal);
		process.off("SIGTERM", onSignal);
		listening = false;
	};

	pi.on("session_start", async (_event, ctx: ExtensionContext) => {
		const file = ctx.sessionManager.getSessionFile();
		if (!file) return; // ephemeral
		const id = ctx.sessionManager.getSessionId();
		sessionId = id;
		signalled = false;
		const now = Date.now();
		const { mux, session } = muxContext();
		update((store) => {
			const prev = store[id];
			store[id] = {
				file,
				cwd: ctx.cwd,
				session,
				mux,
				pid: process.pid,
				startedAt: prev?.startedAt ?? now,
				lastActiveAt: now,
				starred: prev?.starred ?? false,
				state: "live",
			};
		});
		if (!listening) {
			process.on("SIGHUP", onSignal);
			process.on("SIGTERM", onSignal);
			listening = true;
		}
	});

	pi.on("agent_end", async () => {
		const id = sessionId;
		if (!id) return;
		update((store) => {
			if (store[id]) store[id].lastActiveAt = Date.now();
		});
	});

	pi.on("session_shutdown", async (event) => {
		const id = sessionId;
		unlisten();
		if (!id || signalled) return;
		// A reload keeps the same session running: session_start re-marks it live.
		if (event.reason === "reload") return;
		update((store) => {
			const entry = store[id];
			if (!entry) return;
			if (entry.starred) entry.state = "closed";
			else delete store[id];
		});
		sessionId = null;
	});

	pi.registerCommand("star", {
		description: "Toggle the star on this session so pi-restore keeps it after a close",
		handler: async (_args, ctx) => {
			const id = sessionId;
			if (!id) return ctx.ui.notify("Ephemeral session: nothing to star.", "warning");
			let starred = false;
			update((store) => {
				const entry = store[id];
				if (entry) starred = entry.starred = !entry.starred;
			});
			ctx.ui.notify(starred ? "★ Starred: pi-restore keeps this session." : "Unstarred.", "info");
		},
	});
}
