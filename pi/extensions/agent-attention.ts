import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { spawn } from "child_process";

const SCRIPT = `${process.env.HOME}/.config/tmux/scripts/agent-attention`;

// Fire-and-forget JSON-on-stdin to agent-attention. No shell, no quoting,
// no blocking. Mirrors the OpenCode plugin's invocation shape so both
// sources hit the same code path.
function notify(eventType: string, message: string) {
	const child = spawn("python3", [SCRIPT, "notify"], {
		stdio: ["pipe", "ignore", "ignore"],
	});
	child.on("error", () => {});
	// stdio[0]="pipe" guarantees a writable stdin at runtime, but the
	// child_process types still narrow to Writable | null. Belt-and-braces.
	child.stdin?.end(
		JSON.stringify({
			source: "pi",
			type: eventType,
			title: "Pi",
			message: message.slice(0, 200),
		}),
	);
}

export default function (pi: ExtensionAPI) {
	pi.on("agent_end", async () => {
		notify("agent_end", "Waiting for input");
	});
}
