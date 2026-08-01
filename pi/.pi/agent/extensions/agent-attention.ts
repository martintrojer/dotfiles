import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { execFile, execFileSync } from "child_process";
import { join } from "path";
import { promisify } from "util";

const execFileAsync = promisify(execFile);

const MU_MANAGED = process.env.MU_MANAGED_AGENT === "1";
const SCRIPT = join(process.env.HOME!, ".config", "tmux", "scripts", "agent-attention");

// Best-effort tmux command (~34ms), off the event loop. Resolves to stdout or
// null on failure. Arg arrays, so no shell and no quoting exposure around
// interpolated values.
async function tmux(args: string[]): Promise<string | null> {
	try {
		const { stdout } = await execFileAsync("tmux", args, {
			encoding: "utf8",
			timeout: 3000,
		});
		return stdout.trim();
	} catch {
		return null;
	}
}

async function script(args: string[]): Promise<boolean> {
	try {
		await execFileAsync("python3", [SCRIPT, ...args], { timeout: 5000 });
		return true;
	} catch {
		return false;
	}
}

// Resolve pane/window ids once at load time. Sync is fine here: it is paid once
// per process rather than once per turn.
function tmuxSync(args: string[]): string | null {
	try {
		return execFileSync("tmux", args, {
			encoding: "utf8",
			timeout: 3000,
			stdio: ["ignore", "pipe", "ignore"],
		}).trim();
	} catch {
		return null;
	}
}

const PANE_ID = process.env.TMUX_PANE || tmuxSync(["display-message", "-p", "#{pane_id}"]);
const WINDOW_ID = PANE_ID && tmuxSync(["display-message", "-t", PANE_ID, "-p", "#{window_id}"]);

export default function (pi: ExtensionAPI) {
	if (!WINDOW_ID) return;

	// Status decoration must never stall a turn, but the states it writes form a
	// machine (working -> blocked/cleared) that has to stay monotonic. Serialize
	// the handlers through one chain so ordering survives without blocking.
	let queue: Promise<unknown> = Promise.resolve();
	const enqueue = (work: () => Promise<unknown>): Promise<unknown> => {
		queue = queue.then(work, work);
		return queue;
	};

	pi.on("agent_start", async () => {
		enqueue(async () => {
			// Full script path: DB records the pid for crash reaper (~300ms).
			const ok = await script(["event", "--window", WINDOW_ID, "--state", "working", "--pid", String(process.pid)]);
			if (ok) return;
			// Fallback: at least set the tmux option.
			await tmux(["set-window-option", "-q", "-t", WINDOW_ID, "@agent_state", "working"]);
			await tmux(["set-window-option", "-q", "-t", WINDOW_ID, "@pane_agent", "1"]);
			await tmux(["refresh-client", "-S"]);
		});
	});

	pi.on("agent_end", async () => {
		enqueue(async () => {
			// Direct tmux calls only (~34ms each). Survives container
			// reaping where the full Python script path gets killed.
			// DB insert is skipped — the reaper only needs working pids.
			if (MU_MANAGED) {
				await tmux(["set-window-option", "-qu", "-t", WINDOW_ID, "@agent_state"]);
				await tmux(["refresh-client", "-S"]);
				return;
			}
			const focused = await tmux([
				"display-message",
				"-t",
				PANE_ID,
				"-p",
				"#{&&:#{pane_active},#{&&:#{window_active},#{session_attached}}}",
			]);
			if (focused === "1") {
				await tmux(["set-window-option", "-qu", "-t", WINDOW_ID, "@agent_state"]);
			} else {
				await tmux(["set-window-option", "-q", "-t", WINDOW_ID, "@agent_state", "blocked"]);
			}
			await tmux(["set-window-option", "-q", "-t", WINDOW_ID, "@pane_agent", "1"]);
			await tmux(["refresh-client", "-S"]);
		});
	});

	pi.on("session_shutdown", async () => {
		// Awaited, unlike the turn-boundary handlers: this is the last chance to
		// write the terminal event, and pi is exiting anyway. Without it the
		// newest DB row stays the `working` row carrying a pid that is about to
		// die, and the reaper reports a clean exit as a crash on the next 5s
		// status tick.
		await enqueue(async () => {
			if (await script(["event", "--window", WINDOW_ID, "--state", "cleared"])) return;
			// Fallback: at least clear the tmux option.
			await tmux(["set-window-option", "-qu", "-t", WINDOW_ID, "@agent_state"]);
		});
	});
}
