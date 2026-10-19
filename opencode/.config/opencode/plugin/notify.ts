import { execFile } from "node:child_process";
import { Plugin } from "@opencode/plugin";

type Location = { directory?: string; workspaceID?: string } | null | undefined;

export default Plugin.define({
	id: "murmur.notify",
	setup(ctx) {
		const notify = (type: string, message: string) => {
			const payload = JSON.stringify({ source: "opencode", type, title: "OpenCode", message });
			const child = execFile("murmur", ["notify"], () => {});
			child.stdin?.on("error", () => {}).end(payload);
		};
		// Each loaded location has its own plugin instance, but the stream is global.
		const isMine = (location: Location) =>
			location?.directory === ctx.location.directory &&
			(location && "workspaceID" in location ? location.workspaceID : undefined) === ctx.location.workspaceID;

		const controller = new AbortController();
		const subscription = (async () => {
			for await (const event of ctx.event.subscribe({ signal: controller.signal })) {
				// A finished turn arrives as session.execution.succeeded (2.0.32 no
				// longer emits the deprecated session.idle). Durable session events
				// carry no location, so match on the session's own. murmur keys on
				// "session.idle", hence the type below.
				if (event.type === "session.execution.succeeded") {
					const session = await ctx.session.get({ sessionID: event.data.sessionID }).catch(() => undefined);
					if (session && !isMine(session.location)) continue;
					// Only a known subagent is skipped; unknown sessions still notify.
					if (session?.parentID) continue;
					notify("session.idle", session?.title || "Task completed");
				}

				if (event.type === "permission.asked" && isMine(event.location)) {
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
