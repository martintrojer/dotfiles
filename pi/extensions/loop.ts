/**
 * `/loop` — run a prompt on a recurring interval within the current session.
 *
 * A clone of Claude Code's `/loop`. You give it an interval and a prompt, and
 * pi re-sends that prompt at the specified cadence as if you had typed it. The
 * loop lives only as long as the session — it stops on `/loop stop`, on session
 * switch (`/new`, `/resume`, `/fork`), and on quit.
 *
 * Usage:
 *   /loop 5m check if the deploy on staging finished and report status
 *   /loop 30s /some-command          # the prompt can be another slash command
 *   /loop summarize new errors        # no interval -> default 10m
 *   /loop list                        # show active loops
 *   /loop stop                        # cancel all loops
 *   /loop stop 2                      # cancel loop #2
 *
 * Each tick only fires when the agent is idle with nothing queued, so ticks
 * never stack up behind a slow response — a missed tick is simply skipped until
 * the next interval.
 */

import type { ExtensionAPI, ExtensionCommandContext } from "@earendil-works/pi-coding-agent";

const DEFAULT_INTERVAL_MS = 10 * 60 * 1000; // 10 minutes
const MIN_INTERVAL_MS = 5 * 1000; // guard against runaway loops

interface Loop {
	id: number;
	intervalMs: number;
	prompt: string;
	timer: ReturnType<typeof setInterval>;
	ticks: number;
}

const loops = new Map<number, Loop>();
let nextId = 1;

/** Parse a leading interval token like `30s`, `5m`, `2h`, or bare `5` (minutes). */
function parseInterval(token: string): number | null {
	const m = /^(\d+(?:\.\d+)?)(s|m|h)?$/i.exec(token);
	if (!m) return null;
	const value = parseFloat(m[1]);
	const unit = (m[2] ?? "m").toLowerCase();
	const factor = unit === "s" ? 1000 : unit === "h" ? 3600_000 : 60_000;
	return value * factor;
}

function formatInterval(ms: number): string {
	if (ms % 3600_000 === 0) return `${ms / 3600_000}h`;
	if (ms % 60_000 === 0) return `${ms / 60_000}m`;
	return `${Math.round(ms / 1000)}s`;
}

function clearLoop(loop: Loop): void {
	clearInterval(loop.timer);
	loops.delete(loop.id);
}

function clearAll(): void {
	for (const loop of loops.values()) clearInterval(loop.timer);
	loops.clear();
}

function updateStatus(ctx: ExtensionCommandContext): void {
	if (loops.size === 0) {
		ctx.ui.setStatus("loop", "");
		return;
	}
	const summary = [...loops.values()].map((l) => `#${l.id} ${formatInterval(l.intervalMs)}`).join(", ");
	ctx.ui.setStatus("loop", `⟳ loop: ${summary}`);
}

export default function (pi: ExtensionAPI) {
	pi.registerCommand("loop", {
		description: "Run a prompt on a recurring interval (e.g. /loop 5m check the deploy)",
		getArgumentCompletions: (prefix: string) => {
			const items = [
				{ value: "stop", label: "stop — cancel all loops" },
				{ value: "list", label: "list — show active loops" },
			];
			const filtered = items.filter((i) => i.value.startsWith(prefix));
			return filtered.length > 0 ? filtered : null;
		},
		handler: async (args, ctx) => {
			const trimmed = (args ?? "").trim();

			// --- subcommands -------------------------------------------------
			if (trimmed === "" || trimmed === "list" || trimmed === "status") {
				if (loops.size === 0) {
					ctx.ui.notify("No active loops. Usage: /loop <interval> <prompt>", "info");
					return;
				}
				const lines = [...loops.values()].map(
					(l) => `#${l.id}  every ${formatInterval(l.intervalMs)}  (${l.ticks} ticks)  ${l.prompt}`,
				);
				ctx.ui.notify(`Active loops:\n${lines.join("\n")}`, "info");
				return;
			}

			if (/^(stop|cancel|off)\b/i.test(trimmed)) {
				const rest = trimmed.replace(/^(stop|cancel|off)\s*/i, "").trim();
				if (rest === "") {
					const n = loops.size;
					clearAll();
					updateStatus(ctx);
					ctx.ui.notify(n > 0 ? `Stopped ${n} loop(s).` : "No active loops.", "info");
					return;
				}
				const id = parseInt(rest, 10);
				const loop = loops.get(id);
				if (!loop) {
					ctx.ui.notify(`No loop #${rest}.`, "warning");
					return;
				}
				clearLoop(loop);
				updateStatus(ctx);
				ctx.ui.notify(`Stopped loop #${id}.`, "info");
				return;
			}

			// --- schedule a new loop ----------------------------------------
			const parts = trimmed.split(/\s+/);
			let intervalMs = DEFAULT_INTERVAL_MS;
			let prompt = trimmed;
			const maybeInterval = parseInterval(parts[0]);
			if (maybeInterval !== null && parts.length > 1) {
				intervalMs = maybeInterval;
				prompt = parts.slice(1).join(" ");
			}

			if (prompt.trim() === "") {
				ctx.ui.notify("Nothing to loop. Usage: /loop <interval> <prompt>", "warning");
				return;
			}
			if (intervalMs < MIN_INTERVAL_MS) {
				ctx.ui.notify(`Interval too short; minimum is ${formatInterval(MIN_INTERVAL_MS)}.`, "warning");
				return;
			}

			const id = nextId++;
			const timer = setInterval(() => {
				const loop = loops.get(id);
				if (!loop) return;
				// Skip this tick if the agent is busy or has queued work, so
				// loops never pile up behind a slow turn.
				if (!ctx.isIdle() || ctx.hasPendingMessages()) return;
				loop.ticks++;
				pi.sendUserMessage(loop.prompt);
			}, intervalMs);
			// Don't let the loop keep the process alive on its own.
			if (typeof timer.unref === "function") timer.unref();

			loops.set(id, { id, intervalMs, prompt, timer, ticks: 0 });
			updateStatus(ctx);
			ctx.ui.notify(`Loop #${id} scheduled every ${formatInterval(intervalMs)}. Stop with /loop stop ${id}.`, "info");
		},
	});

	// Loops are session-scoped: tear them down on shutdown/switch.
	pi.on("session_shutdown", async () => {
		clearAll();
	});
}
