import type { Plugin } from "@opencode-ai/plugin";

const NOTIFY_SCRIPT = `${process.env.HOME}/.config/tmux/scripts/agent-attention`;

/**
 * The running opencode emits the v2 `permission.asked` event, but the `Event`
 * union re-exported by @opencode-ai/plugin is still the v1 one, which only
 * knows `permission.updated`/`permission.replied`. Match on the raw type
 * string and narrow by hand until the plugin types catch up.
 */
type PermissionAsked = { type: "permission.asked"; properties: { permission: string; patterns: string[] } };

const plugin: Plugin = async ({ client, $ }) => {
	const notify = (type: string, message: string) => {
		const payload = JSON.stringify({ source: "opencode", type, title: "OpenCode", message });
		$`echo ${payload} | python3 ${NOTIFY_SCRIPT} notify`.quiet().catch(() => {});
	};

	return {
		event: async ({ event }) => {
			if (event.type === "session.idle") {
				const { sessionID } = event.properties;
				// No `limit`: the v1 /session query takes only `directory`. The old
				// `{ limit: 50 }` was spread at the top level of the request options,
				// never into `query`, so it was already a no-op at runtime.
				const sessions = await client.session.list();
				const session = sessions.data?.find((s: { id: string }) => s.id === sessionID);
				if (!session || session.parentID) return;

				notify("session.idle", session.title || "Task completed");
			}

			if ((event as { type: string }).type === "permission.asked") {
				const { permission, patterns } = (event as unknown as PermissionAsked).properties;
				const detail = patterns.length ? `: ${patterns.join(", ")}` : "";
				notify("permission.asked", `${permission}${detail}`);
			}
		},
	};
};

export default plugin;
