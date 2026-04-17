import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { execSync } from "child_process";

const SCRIPT = `${process.env.HOME}/.config/tmux/scripts/agent-attention`;

function notify(eventType: string, message: string) {
  try {
    execSync(
      `python3 "${SCRIPT}" notify --source pi --event-type ${eventType} --title Pi --message "${message.replace(/"/g, '\\"').substring(0, 200)}"`,
      { stdio: "ignore", timeout: 5000 },
    );
  } catch {
    // best-effort
  }
}

export default function (pi: ExtensionAPI) {
  pi.on("agent_end", async () => {
    notify("agent_end", "Waiting for input");
  });
}
