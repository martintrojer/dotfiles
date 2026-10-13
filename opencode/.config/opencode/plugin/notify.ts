import { execFile } from "node:child_process";
import { Plugin } from "@opencode/plugin";

export default Plugin.define({
	id: "murmur.notify",
	setup(ctx) {
		const notify = (type: string, message: string) => {
			const payload = JSON.stringify({ source: "opencode", type, title: "OpenCode", message });
			const child = execFile("murmur", ["notify"], () => {});
			child.stdin?.on("error", () => {}).end(payload);
		};
		const controller = new AbortController();
		const subscription = (async () => {
			for await (const event of ctx.event.subscribe({ signal: controller.signal })) {
				// Each loaded location has its own plugin instance, but the stream is global.
				if (
					event.location?.directory !== ctx.location.directory ||
					(event.location && "workspaceID" in event.location ? event.location.workspaceID : undefined) !==
						ctx.location.workspaceID
				)
					continue;

				if (event.type === "session.idle") {
					const session = await ctx.session.get({ sessionID: event.data.sessionID }).catch(() => undefined);
					// Only a known subagent is skipped; unknown sessions still notify.
					if (session?.parentID) continue;
					notify("session.idle", session?.title || "Task completed");
				}

				if (event.type === "permission.asked") {
					const { action, resources } = event.data;
					const detail = resources.length ? `: ${resources.join(", ")}` : "";
					notify("permission.asked", `${action}${detail}`);
				}
			}
		})().catch((error) => {
			if (!controller.signal.aborted) console.error("murmur.notify: event subscription failed", error);
		});

		return async () => {
			controller.abort();
			await subscription;
		};
	},
});
