import type { Plugin } from "@opencode-ai/plugin";

const NOTIFY_SCRIPT = `${process.env.HOME}/.config/tmux/scripts/agent-attention`;

const plugin: Plugin = async ({ client, $ }) => {
  const notify = (type: string, message: string) => {
    const payload = JSON.stringify({ source: "opencode", type, title: "OpenCode", message });
    $`echo ${payload} | python3 ${NOTIFY_SCRIPT} notify`.quiet().catch(() => {});
  };

  return {
    event: async ({ event }) => {
      if (event.type === "session.idle") {
        const { sessionID } = event.properties;
        const sessions = await client.session.list({ limit: 50 });
        const session = sessions.data?.find((s: { id: string }) => s.id === sessionID);
        if (!session || session.parentID) return;

        notify("session.idle", session.title || "Task completed");
      }

      if (event.type === "permission.asked") {
        const { permission, patterns } = event.properties;
        const detail = patterns.length ? `: ${patterns.join(", ")}` : "";
        notify("permission.asked", `${permission}${detail}`);
      }
    },
  };
};

export default plugin;
