/**
 * `/goal` — keep working toward a verifiable completion condition.
 *
 * A clone of Claude Code's `/goal`. You state an end condition; pi keeps taking
 * turns toward it without you prompting each step. After every agent turn, the
 * session model checks the recent transcript and answers a single yes/no: is
 * yes/no: is the condition met? "no" injects the checker's reason as the next
 * instruction and the agent keeps going; "yes" clears the goal and hands
 * control back to you.
 *
 * This differs from `/loop` (sibling extension): `/loop` is timer-driven and
 * re-sends a fixed prompt on an interval; `/goal` is turn-driven and continues
 * until an evaluator confirms a condition. Keep them separate, like Claude Code.
 *
 * Usage:
 *   /goal until `npm test` exits 0 and tsc --noEmit is clean, max 20 turns
 *   /goal                 # show the active goal, turns spent, last reason
 *   /goal clear           # stop the goal (aliases: stop, off, reset, cancel)
 *
 * Notes:
 *   - The checker uses the current session model and has no tools; it can only
 *     judge what the agent surfaced in the conversation. Make conditions verifiable and have the agent print
 *     the evidence (test output, file counts, grep results).
 *   - A turn limit is a safety net against impossible conditions.
 *   - Goals are session-scoped: cleared on session switch and on quit.
 */

import { complete } from "@earendil-works/pi-ai";
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { conversationTranscript, pickSessionModel, textContent } from "./_lib.ts";

const DEFAULT_MAX_TURNS = 25;
// How many trailing transcript chars to show the checker.
const TRANSCRIPT_BUDGET = 16000;

interface GoalState {
	condition: string;
	maxTurns: number;
	turns: number;
	lastReason: string;
	startedAt: number;
	checking: boolean;
}

let goal: GoalState | null = null;

/** Parse an optional trailing "max N turns" directive out of the condition. */
function parseCondition(raw: string): { condition: string; maxTurns: number } {
	let maxTurns = DEFAULT_MAX_TURNS;
	let condition = raw.trim();
	const m = /(?:,?\s*(?:or\s+)?(?:stop\s+after|max(?:imum)?)\s+(\d+)\s*turns?\.?)\s*$/i.exec(condition);
	if (m) {
		maxTurns = parseInt(m[1], 10);
		condition = condition
			.slice(0, m.index)
			.trim()
			.replace(/[,;]+$/, "");
	}
	return { condition, maxTurns };
}

/** Build a trailing slice of the conversation for the checker. */
function recentTranscript(ctx: ExtensionContext): string {
	return conversationTranscript(ctx.sessionManager.getBranch() as never[], TRANSCRIPT_BUDGET);
}

interface CheckResult {
	met: boolean;
	reason: string;
}

/** Ask the session model whether the condition holds. */
async function runChecker(
	ctx: ExtensionContext,
	condition: string,
	transcript: string,
): Promise<CheckResult | { error: string }> {
	const picked = await pickSessionModel(ctx);
	if (!picked) {
		return { error: "No checker model available (no model has configured auth)." };
	}
	const { model, apiKey, headers } = picked;

	const prompt = [
		"You are a strict completion checker for an autonomous coding agent.",
		"Decide whether the COMPLETION CONDITION is verifiably satisfied by the",
		"evidence in the TRANSCRIPT. You have no tools; judge only what is shown.",
		"Do not give the benefit of the doubt. If the agent claims success without",
		"showing evidence (e.g. test output), the condition is NOT met.",
		"",
		"Reply with exactly one line of JSON and nothing else:",
		'{"met": true|false, "reason": "<one concise sentence>"}',
		"",
		`COMPLETION CONDITION:\n${condition}`,
		"",
		`<transcript>\n${transcript}\n</transcript>`,
	].join("\n");

	// Pass ctx.signal on purpose (unlike btw.ts): the checker IS part of the main
	// goal loop, so if the user aborts the agent the checker should abort too.
	const response = await complete(
		model,
		{ messages: [{ role: "user", content: [{ type: "text", text: prompt }], timestamp: Date.now() }] },
		{ apiKey, headers, signal: ctx.signal },
	);

	const text = textContent(response.content);

	const jsonMatch = /\{[\s\S]*\}/.exec(text);
	if (jsonMatch) {
		try {
			const parsed = JSON.parse(jsonMatch[0]) as { met?: unknown; reason?: unknown };
			return {
				met: parsed.met === true,
				reason: typeof parsed.reason === "string" ? parsed.reason : text,
			};
		} catch {
			// fall through to heuristic
		}
	}
	// Heuristic fallback if the model didn't return clean JSON.
	return { met: /\bmet\b|\byes\b|\btrue\b/i.test(text) && !/not met|\bno\b/i.test(text), reason: text };
}

function updateStatus(ctx: ExtensionContext): void {
	if (!goal) {
		ctx.ui.setStatus("goal", "");
		return;
	}
	const mins = Math.round((Date.now() - goal.startedAt) / 60000);
	ctx.ui.setStatus("goal", `◎ goal active · turn ${goal.turns}/${goal.maxTurns} · ${mins}m`);
}

export default function (pi: ExtensionAPI) {
	pi.registerCommand("goal", {
		description: "Work autonomously toward a verifiable condition (clone of Claude Code /goal)",
		getArgumentCompletions: (prefix: string) => {
			const items = [{ value: "clear", label: "clear — stop the active goal" }];
			const filtered = items.filter((i) => i.value.startsWith(prefix));
			return filtered.length > 0 ? filtered : null;
		},
		handler: async (args, ctx) => {
			const trimmed = (args ?? "").trim();

			if (/^(clear|stop|off|reset|none|cancel)$/i.test(trimmed)) {
				if (!goal) {
					ctx.ui.notify("No active goal.", "info");
					return;
				}
				goal = null;
				updateStatus(ctx);
				ctx.ui.notify("Goal cleared.", "info");
				return;
			}

			if (trimmed === "") {
				if (!goal) {
					ctx.ui.notify("No active goal. Usage: /goal <verifiable condition>", "info");
					return;
				}
				const mins = Math.round((Date.now() - goal.startedAt) / 60000);
				ctx.ui.notify(
					`Goal: ${goal.condition}\n` +
						`Turns: ${goal.turns}/${goal.maxTurns} · ${mins}m\n` +
						`Last check: ${goal.lastReason || "(pending first evaluation)"}`,
					"info",
				);
				return;
			}

			const { condition, maxTurns } = parseCondition(trimmed);
			goal = {
				condition,
				maxTurns,
				turns: 0,
				lastReason: "",
				startedAt: Date.now(),
				checking: false,
			};
			updateStatus(ctx);
			ctx.ui.notify(`Goal set (max ${maxTurns} turns). Working toward:\n${condition}\nStop with /goal clear.`, "info");
			// Setting the goal is the first directive; kick off work now.
			pi.sendUserMessage(
				`Work autonomously toward this goal until it is verifiably met. ` +
					`Show concrete evidence (command output, file state) as you go.\n\nGoal: ${condition}`,
			);
		},
	});

	// After each agent turn, evaluate the condition and continue or stop.
	pi.on("agent_end", async (_event, ctx) => {
		if (!goal || goal.checking) return;
		// Don't fight queued user input.
		if (ctx.hasPendingMessages()) return;

		const active = goal;
		active.checking = true;
		try {
			active.turns++;
			updateStatus(ctx);

			if (active.turns > active.maxTurns) {
				if (goal !== active) return;
				const reason = active.lastReason || "(no evaluation yet)";
				ctx.ui.notify(`Goal stopped: reached turn limit (${active.maxTurns}). Last status: ${reason}`, "warning");
				goal = null;
				updateStatus(ctx);
				return;
			}

			const transcript = recentTranscript(ctx);
			const result = await runChecker(ctx, active.condition, transcript);
			if (goal !== active) return; // goal was cleared/replaced while checker awaited

			if ("error" in result) {
				ctx.ui.notify(`Goal checker error: ${result.error} Clearing goal.`, "warning");
				goal = null;
				updateStatus(ctx);
				return;
			}

			active.lastReason = result.reason;

			if (result.met) {
				ctx.ui.notify(`✓ Goal met: ${result.reason}`, "info");
				goal = null;
				updateStatus(ctx);
				return;
			}

			// Not met: feed the reason back as the next instruction.
			updateStatus(ctx);
			pi.sendUserMessage(
				`The goal is not yet met. Checker feedback: ${result.reason}\n` +
					`Keep working toward the goal and show evidence of progress.`,
				{ deliverAs: "followUp" },
			);
		} finally {
			if (goal === active) active.checking = false;
		}
	});

	pi.on("session_shutdown", async () => {
		goal = null;
	});
}
