import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { execSync } from "child_process";
import { join } from "path";

const MU_MANAGED = process.env.MU_MANAGED_AGENT === "1";
const SCRIPT = join(process.env.HOME!, ".config", "tmux", "scripts", "agent-attention");

// Best-effort tmux command (~34ms). Returns stdout or null on failure.
function tmux(cmd: string): string | null {
	try {
		return execSync(`tmux ${cmd}`, {
			encoding: "utf8",
			timeout: 3000,
			stdio: ["ignore", "pipe", "ignore"],
		}).trim();
	} catch {
		return null;
	}
}

// Resolve pane/window ids once at load time.
const PANE_ID = process.env.TMUX_PANE || tmux('display-message -p "#{pane_id}"');
const WINDOW_ID = PANE_ID && tmux(`display-message -t ${PANE_ID} -p "#{window_id}"`);

export default function (pi: ExtensionAPI) {
	if (!WINDOW_ID) return;

	pi.on("agent_start", async () => {
		// Full script path: DB records the pid for crash reaper (~300ms).
		// Safe because pi stays alive long after agent_start.
		try {
			execSync(`python3 ${SCRIPT} event --window ${WINDOW_ID} --state working --pid ${process.pid}`, {
				timeout: 5000,
				stdio: "ignore",
			});
		} catch {
			// Fallback: at least set the tmux option.
			tmux(`set-window-option -q -t ${WINDOW_ID} @agent_state working`);
			tmux(`set-window-option -q -t ${WINDOW_ID} @pane_agent 1`);
			tmux("refresh-client -S");
		}
	});

	pi.on("agent_end", async () => {
		// Direct tmux calls only (~34ms each). Survives container
		// reaping where the full Python script path gets killed.
		// DB insert is skipped — the reaper only needs working pids.
		if (MU_MANAGED) {
			tmux(`set-window-option -qu -t ${WINDOW_ID} @agent_state`);
			tmux("refresh-client -S");
			return;
		}
		const focused = tmux(
			`display-message -t ${PANE_ID} -p "#{&&:#{pane_active},#{&&:#{window_active},#{session_attached}}}"`,
		);
		if (focused === "1") {
			tmux(`set-window-option -qu -t ${WINDOW_ID} @agent_state`);
		} else {
			tmux(`set-window-option -q -t ${WINDOW_ID} @agent_state blocked`);
		}
		tmux(`set-window-option -q -t ${WINDOW_ID} @pane_agent 1`);
		tmux("refresh-client -S");
	});

	pi.on("session_shutdown", async () => {
		tmux(`set-window-option -qu -t ${WINDOW_ID} @agent_state`);
	});
}
