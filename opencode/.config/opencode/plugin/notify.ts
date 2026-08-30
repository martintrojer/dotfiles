import type { Plugin } from "@opencode-ai/plugin";

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
		$`echo ${payload} | murmur notify`.quiet().catch(() => {});
	};

	return {
		event: async ({ event }) => {
			if (event.type === "session.idle") {
				const { sessionID } = event.properties;
				// Fetch the one session directly. Listing and scanning the result
				// meant a long-running session that had aged out of the returned
				// window was silently treated the same as a subagent, and no idle
				// notification fired -- exactly the session this plugin exists for.
				const session = await client.session
					.get({ path: { id: sessionID } })
					.then((r) => r.data)
					.catch(() => undefined);
				// Only a known subagent is skipped. An unknown id still notifies:
				// a notification with a generic title beats a missed one here.
				if (session?.parentID) return;

				notify("session.idle", session?.title || "Task completed");
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
