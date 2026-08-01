/**
 * `/namesession` — generate a friendly session name with a cheap/fast model.
 *
 * Pi's built-in `/name` command persists a session display name that `/resume`
 * shows instead of the first prompt. This command automates that primitive: it
 * asks a small/fast fallback model for a short title from the current transcript,
 * then calls `pi.setSessionName()`.
 *
 * The extension also auto-names unnamed sessions right before they are closed
 * by `/new`, `/resume`, `/fork`, or quit. It intentionally skips `/reload`.
 *
 * Usage:
 *   /namesession              # generate from current conversation
 *   /namesession auth cleanup # optional hint to steer the generated name
 */

import { complete } from "@earendil-works/pi-ai/compat";
import type { ExtensionAPI, ExtensionCommandContext, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { BorderedLoader } from "@earendil-works/pi-coding-agent";
import { conversationTranscript, FastModelCancelled, textContent, withFastModelFallback } from "./_lib.ts";

const TRANSCRIPT_BUDGET = 12000;
const MAX_NAME_CHARS = 60;
const AUTO_NAME_TIMEOUT_MS = 15000;
const AUTO_NAME_SHUTDOWN_REASONS = new Set(["quit", "new", "resume", "fork"]);

let namingInFlight = false;

// Trust the model: extract the name from its JSON reply, do minimal cleanup,
// and soft-truncate for display. No content gating — the prompt does the steering.
function extractName(raw: string): string {
	let name = raw.trim();

	const jsonMatch = /\{[\s\S]*\}/.exec(name);
	if (jsonMatch) {
		try {
			const parsed = JSON.parse(jsonMatch[0]) as { name?: unknown; title?: unknown };
			if (typeof parsed.name === "string") name = parsed.name;
			else if (typeof parsed.title === "string") name = parsed.title;
		} catch {
			// Fall back to plain-text cleanup below.
		}
	}

	name = name
		.replace(/[\r\n\t]+/g, " ")
		.replace(/["'`]/g, "")
		.replace(/\s+/g, " ")
		.trim();

	if (name.length > MAX_NAME_CHARS) {
		name = name
			.slice(0, MAX_NAME_CHARS)
			.replace(/\s+\S*$/, "")
			.trim();
	}

	if (!name) throw new Error("Model returned an empty name.");
	return name;
}

async function generateSessionName(
	ctx: ExtensionContext,
	hint: string,
	signal?: AbortSignal,
): Promise<{ name: string; model: string } | { error: string }> {
	const transcript = conversationTranscript(ctx.sessionManager.getBranch() as never[], TRANSCRIPT_BUDGET).trim();
	if (!transcript) return { error: "No user/assistant conversation found to name yet." };

	const current = ctx.sessionManager.getSessionName();
	const prompt = [
		"Write a short title for this coding-agent work item.",
		"A human will later scan and search a long list of these to find this exact one, so make it specific and unique.",
		"Lead with the most distinctive searchable keyword (the concrete project, file, command, symbol, bug, or feature).",
		"Rules:",
		"- 3 to 6 words, Title Case or a concise verb phrase",
		"- Never use the words Session, Chat, Conversation, or Thread (even when the work is about them; name the specific thing instead)",
		"- Specific over generic; avoid vague filler like Extension, Feature, Task, Work, Update, Changes, Improvements",
		"- No quotes, emojis, trailing period, or model names",
		"",
		"Good examples (specific, easy to find again):",
		"- Auto-Name Pi History",
		"- Fix Tmux Status Bar Tests",
		"- Summarize Skill URL Fallbacks",
		"- Stow Ignore Rules Cleanup",
		"",
		"Bad examples (too generic):",
		"- Coding Session",
		"- User Request Implementation",
		"- Extension Improvements",
		"- Config Update",
		"",
		"Reply with exactly one JSON object and nothing else:",
		'{"name":"Short Name"}',
		current ? `\nCurrent name, if useful context: ${current}` : "",
		hint ? `\nUser hint: ${hint}` : "",
		`\n<transcript>\n${transcript}\n</transcript>`,
	]
		.filter(Boolean)
		.join("\n");

	const outcome = await withFastModelFallback(ctx, "namesession.title", async (picked) => {
		if (signal?.aborted || ctx.signal?.aborted) throw new FastModelCancelled();
		const response = await complete(
			picked.model,
			{ messages: [{ role: "user", content: [{ type: "text", text: prompt }], timestamp: Date.now() }] },
			{ apiKey: picked.apiKey, headers: picked.headers, signal: signal ?? ctx.signal },
		);
		const name = extractName(textContent(response.content));
		return name;
	});

	if (outcome.kind === "ok") return { name: outcome.result, model: outcome.model };
	return { error: outcome.kind === "cancelled" ? "Cancelled." : outcome.message };
}

async function nameCurrentSession(
	pi: ExtensionAPI,
	ctx: ExtensionContext,
	options: {
		hint?: string;
		notify?: boolean;
		onlyIfUnnamed?: boolean;
		timeoutMs?: number;
		loader?: boolean;
		loaderTitle?: string;
	} = {},
): Promise<boolean> {
	if (namingInFlight) {
		if (options.notify) ctx.ui.notify("Session naming already in progress.", "info");
		return false;
	}
	// `isPersisted()` exists on SessionManager at runtime but is not part of the
	// exported ReadonlySessionManager Pick, so it is invisible to tsc. Narrow the
	// cast to just that method rather than widening ctx.sessionManager.
	if (!(ctx.sessionManager as unknown as { isPersisted(): boolean }).isPersisted()) return false;
	if (options.onlyIfUnnamed !== false && ctx.sessionManager.getSessionName()) return false;

	namingInFlight = true;
	try {
		if (options.loader && ctx.mode === "tui") {
			return await ctx.ui.custom<boolean>((tui, theme, _kb, done) => {
				const loader = new BorderedLoader(tui, theme, options.loaderTitle ?? "Naming session…", { cancellable: true });
				const signal = AbortSignal.any(
					[loader.signal, options.timeoutMs ? AbortSignal.timeout(options.timeoutMs) : undefined].filter(
						(s) => s !== undefined,
					),
				);
				let settled = false;
				const finish = (value: boolean): void => {
					if (settled) return;
					settled = true;
					done(value);
				};
				loader.onAbort = () => finish(false);
				void (async () => {
					try {
						const result = await generateSessionName(ctx, options.hint ?? "", signal);
						if (signal.aborted) return finish(false);
						if ("error" in result) {
							if (options.notify) ctx.ui.notify(`Could not name session: ${result.error}`, "warning");
							return finish(false);
						}

						pi.setSessionName(result.name);
						if (options.notify) ctx.ui.notify(`Session named: ${result.name}\nGenerated by ${result.model}`, "info");
						finish(true);
					} catch (err) {
						if (options.notify) {
							const message = err instanceof Error ? err.message : String(err);
							ctx.ui.notify(`Could not name session: ${message}`, "warning");
						}
						finish(false);
					}
				})();
				return loader;
			});
		}

		const signal = options.timeoutMs ? AbortSignal.timeout(options.timeoutMs) : undefined;
		const result = await generateSessionName(ctx, options.hint ?? "", signal);
		if ("error" in result) {
			if (options.notify) ctx.ui.notify(`Could not name session: ${result.error}`, "warning");
			return false;
		}

		pi.setSessionName(result.name);
		if (options.notify) ctx.ui.notify(`Session named: ${result.name}\nGenerated by ${result.model}`, "info");
		return true;
	} finally {
		namingInFlight = false;
	}
}

function registerNameSessionCommand(pi: ExtensionAPI, name: string): void {
	pi.registerCommand(name, {
		description: "Generate a friendly /resume session name with a small/fast model",
		getArgumentCompletions: (prefix: string) => {
			if (prefix.trim() !== "") return null;
			return [
				{
					value: "",
					label: `/${name} [optional hint]`,
					description: "name the current session from the transcript",
				},
			];
		},
		handler: async (args, ctx) => {
			await nameCurrentSession(pi, ctx, {
				hint: (args ?? "").trim(),
				notify: true,
				onlyIfUnnamed: false,
				loader: true,
			});
		},
	});
}

export default function (pi: ExtensionAPI) {
	registerNameSessionCommand(pi, "namesession");

	pi.on("session_shutdown", async (event, ctx) => {
		if (!AUTO_NAME_SHUTDOWN_REASONS.has(event.reason)) return;
		await nameCurrentSession(pi, ctx, {
			onlyIfUnnamed: true,
			timeoutMs: AUTO_NAME_TIMEOUT_MS,
			loader: ctx.mode === "tui",
			loaderTitle: "Naming session before closing…",
		});
	});
}
