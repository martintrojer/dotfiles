/**
 * `/namesession` — generate a friendly session name with a cheap/fast model.
 *
 * Pi's built-in `/name` command persists a session display name that `/resume`
 * shows instead of the first prompt. This command automates that primitive: it
 * asks a small/fast fallback model for a short title from a digest of the whole
 * transcript (see `_session-name.ts`), then calls `pi.setSessionName()`.
 *
 * Auto-naming, in the background after a turn settles (never blocking it):
 * an unnamed session is named after its first turn with a reply, and a name
 * this extension set is refreshed as the session grows (`autoNameDue`). Each
 * name it sets is recorded in a `namesession` custom entry; a name that does
 * not match the record came from `/name` or `--name` and is never touched.
 * Naming on the first turn is what keeps sessions killed by SIGHUP or a crash
 * from coming back nameless. The shutdown path (`/new`, `/resume`, `/fork`,
 * quit; not `/reload`) remains a fallback for sessions still unnamed.
 *
 * mu-managed agents (workers, delegates) are never named; orchestrators are.
 *
 * Usage:
 *   /namesession              # generate from current conversation
 *   /namesession auth cleanup # optional hint to steer the generated name
 */

import { complete } from "@earendil-works/pi-ai/compat";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { BorderedLoader } from "@earendil-works/pi-coding-agent";
import { FastModelCancelled, textContent, withFastModelFallback } from "./_lib.ts";
import {
	autoNameDue,
	buildNamePrompt,
	extractName,
	NAME_RECORD_TYPE,
	type NameRecord,
	ownedNameRecord,
	sessionDigest,
	turnTexts,
} from "./_session-name.ts";

const TRANSCRIPT_BUDGET = 12000;
const AUTO_NAME_TIMEOUT_MS = 15000;
const SHUTDOWN_REASONS = new Set(["quit", "new", "resume", "fork"]);
const MU_WORKER = process.env.MU_MANAGED_AGENT === "1" && process.env.MU_ORCHESTRATOR !== "1";

/** Resolve to `job`'s result, or false once `signal` aborts, whichever comes first. */
function bounded(job: Promise<boolean>, signal: AbortSignal | undefined): Promise<boolean> {
	if (!signal) return job;
	if (signal.aborted) return Promise.resolve(false);
	return Promise.race([
		job,
		new Promise<boolean>((resolve) => signal.addEventListener("abort", () => resolve(false), { once: true })),
	]);
}

// One naming at a time, process-wide. Shutdown awaits it so a background
// refresh lands in its own session before that session is replaced.
let inFlight: Promise<boolean> | undefined;

async function exclusive(run: () => Promise<boolean>): Promise<boolean> {
	if (inFlight) return false;
	inFlight = run();
	try {
		return await inFlight;
	} finally {
		inFlight = undefined;
	}
}

function namingState(ctx: ExtensionContext) {
	const branch = ctx.sessionManager.getBranch() as never[];
	const { prompts, replies } = turnTexts(branch);
	return {
		branch,
		prompts: prompts.length,
		replies: replies.length,
		current: ctx.sessionManager.getSessionName(),
		record: ownedNameRecord(ctx.sessionManager.getEntries()),
	};
}

// `isPersisted()` exists on SessionManager at runtime but is not part of the
// exported ReadonlySessionManager Pick, so it is invisible to tsc. Narrow the
// cast to just that method rather than widening ctx.sessionManager.
function isPersisted(ctx: ExtensionContext): boolean {
	return (ctx.sessionManager as unknown as { isPersisted(): boolean }).isPersisted();
}

function autoDue(ctx: ExtensionContext): boolean {
	const s = namingState(ctx);
	return autoNameDue(s.current, s.record, s.prompts, s.replies);
}

/** Generate a name and apply it. Returns whether a name was recorded. */
async function nameSession(
	pi: ExtensionAPI,
	ctx: ExtensionContext,
	opts: { hint?: string; keepCurrent: boolean; notify: boolean; signal?: AbortSignal },
): Promise<boolean> {
	const { branch, prompts, current, record: owned } = namingState(ctx);
	const sessionId = ctx.sessionManager.getSessionId();
	const fail = (message: string): false => {
		if (opts.notify) ctx.ui.notify(`Could not name session: ${message}`, "warning");
		return false;
	};

	const transcript = sessionDigest(branch, TRANSCRIPT_BUDGET).trim();
	if (!transcript) return fail("No user/assistant conversation found to name yet.");
	const prompt = buildNamePrompt(transcript, { current, hint: opts.hint, keepCurrent: opts.keepCurrent });

	const signal = opts.signal ?? ctx.signal;
	const outcome = await withFastModelFallback(ctx, "namesession.title", async (picked) => {
		if (signal?.aborted) throw new FastModelCancelled();
		try {
			const response = await complete(
				picked.model,
				{ messages: [{ role: "user", content: [{ type: "text", text: prompt }], timestamp: Date.now() }] },
				{ apiKey: picked.apiKey, headers: picked.headers, signal },
			);
			return extractName(textContent(response.content));
		} catch (err) {
			// A timeout or Esc is not the model's fault: stop, don't mark it failed and try the next.
			if (signal?.aborted) throw new FastModelCancelled();
			throw err;
		}
	});
	if (signal?.aborted || outcome.kind === "cancelled") return false;
	if (outcome.kind !== "ok") return fail(outcome.message);

	// The call ran in the background: drop the result if the session was
	// switched, or the user renamed it, while it was in flight.
	const now = namingState(ctx);
	if (ctx.sessionManager.getSessionId() !== sessionId || now.current !== current || (owned && !now.record))
		return false;

	const name = outcome.result;
	if (name !== current) pi.setSessionName(name);
	const record: NameRecord = { name, prompts, ...(opts.hint ? { hint: opts.hint } : {}) };
	pi.appendEntry(NAME_RECORD_TYPE, record);
	if (opts.notify) ctx.ui.notify(`Session named: ${name}\nGenerated by ${outcome.model}`, "info");
	return true;
}

/** Run `job` under a cancellable loader in the TUI, bare elsewhere. */
async function withLoader(
	ctx: ExtensionContext,
	title: string,
	timeoutMs: number | undefined,
	job: (signal: AbortSignal | undefined) => Promise<boolean>,
): Promise<boolean> {
	const timeout = timeoutMs ? AbortSignal.timeout(timeoutMs) : undefined;
	// The signal also bounds the wait itself: model and credential lookup in
	// withFastModelFallback runs before any abort check and may hang.
	if (ctx.mode !== "tui") return bounded(job(timeout), timeout);
	return ctx.ui.custom<boolean>((tui, theme, _kb, done) => {
		const loader = new BorderedLoader(tui, theme, title, { cancellable: true });
		const signal = timeout ? AbortSignal.any([loader.signal, timeout]) : loader.signal;
		let settled = false;
		const finish = (value: boolean): void => {
			if (settled) return;
			settled = true;
			done(value);
		};
		loader.onAbort = () => finish(false);
		void bounded(job(signal), signal).then(finish, () => finish(false));
		return loader;
	});
}

export default function (pi: ExtensionAPI) {
	pi.registerCommand("namesession", {
		description: "Generate a friendly /resume session name with a small/fast model",
		handler: async (args, ctx) => {
			if (!isPersisted(ctx)) return;
			const hint = (args ?? "").trim();
			const ran = await exclusive(() =>
				withLoader(ctx, "Naming session…", undefined, (signal) =>
					nameSession(pi, ctx, { hint, keepCurrent: false, notify: true, signal }),
				),
			);
			if (!ran && inFlight) ctx.ui.notify("Session naming already in progress.", "info");
		},
	});

	if (MU_WORKER) return;

	// Fire and forget: the handler returns at once, failures are swallowed.
	pi.on("agent_settled", (_event, ctx) => {
		if (inFlight || !isPersisted(ctx) || !autoDue(ctx)) return;
		const hint = ownedNameRecord(ctx.sessionManager.getEntries())?.hint;
		const signal = AbortSignal.timeout(AUTO_NAME_TIMEOUT_MS);
		void exclusive(() =>
			bounded(nameSession(pi, ctx, { hint, keepCurrent: true, notify: false, signal }), signal),
		).catch(() => {});
	});

	pi.on("session_shutdown", async (event, ctx) => {
		if (!SHUTDOWN_REASONS.has(event.reason)) return;
		// Let a background naming land in this session before it is replaced, but bounded.
		if (inFlight)
			await bounded(
				inFlight.catch(() => false),
				AbortSignal.timeout(AUTO_NAME_TIMEOUT_MS),
			);
		if (!isPersisted(ctx) || ctx.sessionManager.getSessionName()) return;
		if (!autoDue(ctx)) return;
		await exclusive(() =>
			withLoader(ctx, "Naming session before closing…", AUTO_NAME_TIMEOUT_MS, (signal) =>
				nameSession(pi, ctx, { keepCurrent: false, notify: false, signal }),
			),
		);
	});
}
