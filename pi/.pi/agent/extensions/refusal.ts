/**
 * `refusal` — a check that refused is a result to report, not an obstacle.
 *
 * When a gate (pre-commit hook, immutable-commit guard, a `check-*` target)
 * rejects a command and the very next attempt at the SAME action carries a flag
 * whose whole purpose is to get past that rejection — `--no-verify`, `--force`,
 * `--skip` — the bypass removes the verification rather than satisfying it.
 * This nudges once, with a fixed note. It never blocks.
 *
 * Why this is an extension and not a line in a skill: the rule has to fire
 * whether or not the agent cooperates, and the agent about to bypass the check
 * is the one who would have to remember the rule. See
 * `skills/.agents/skills/verification-before-completion/SKILL.md`.
 *
 * Why there is no model in the path: the trigger is two observable facts (a
 * gate refused; the retry disabled it), not a judgment, so `_lib.ts`'s
 * fast-model ladder is deliberately unused. Muse Code routes the same rule
 * through an LLM judge and it silently failed open — a `git push` recorded
 * `allow:policy` with no judge invocation at all. A deterministic hook has no
 * fast path to slip through.
 *
 * Precision is the whole design. A bare `--force` is normal (force-pushing your
 * own branch after a rebase); only a bypass flag that appears AFTER the same
 * action was refused, and was absent from the refused invocation, is flagged.
 * False positives here train you to ignore the nudge, which is worse than
 * silence.
 */

import { isToolCallEventType, type ExtensionAPI, type ExtensionContext } from "@earendil-works/pi-coding-agent";

/**
 * Flags that exist ONLY to skip a check. Nobody passes `--no-verify` for any
 * reason other than getting past a hook, so any prior failure of the same
 * action is enough context — the gate need not have said anything. This is
 * what catches a hook that is just `exit 1` with no output, where pi throws a
 * bare "Command exited with code 1" and there is no wording to match.
 */
const GATE_ONLY_FLAGS = [
	"--no-verify",
	"--no-validate",
	"--skip-checks",
	"--skip-hooks",
	"--skip-tests",
	"--no-pre-commit",
	"--ignore-immutable",
] as const;

/**
 * Flags that disable a check but have legitimate everyday uses — force-pushing
 * your own branch after a rebase, `jj --ignore-working-copy` for speed. These
 * fire only when the failure they follow actually read like a gate declining,
 * so ordinary use stays silent.
 */
const AMBIGUOUS_BYPASS_FLAGS = [
	"--force",
	"--force-with-lease",
	"--ignore-working-copy",
	"--allow-empty",
	"--allow-dirty",
	"--no-gpg-sign",
	"-n",
	"-f",
] as const;

const BYPASS_FLAGS = [...GATE_ONLY_FLAGS, ...AMBIGUOUS_BYPASS_FLAGS] as const;

/** Is this flag one that has no purpose other than skipping a check? */
export function isGateOnlyFlag(flag: string): boolean {
	return (GATE_ONLY_FLAGS as readonly string[]).includes(flag);
}

/**
 * Output that reads like a *gate* declining, rather than a command merely
 * failing. A typo'd path exits nonzero too; that is not a refusal.
 *
 * Every marker here must be a phrase a gate EMITS, never a word a gate is
 * named by. pi throws the tool's whole stdout+stderr as the error message
 * (`bash.js` `appendStatus`), so a bare "hook" would match `git help hooks` or
 * a grep across hook files and arm state for a command nothing refused.
 * "aborting" is git/hg/jj's own refusal wording; pi's separate "Command
 * aborted" (user pressed Esc) and "Command timed out" are deliberately absent.
 */
const GATE_MARKERS = [
	"pre-commit hook",
	"pre-push hook",
	"commit-msg hook",
	"hook failed",
	"hook exited",
	"husky",
	"rejected",
	"refus", // refused / refusing / refusal
	"is immutable",
	"immutable commit",
	"not allowed",
	"forbidden",
	"operation blocked",
	"aborting",
	"failed to pass",
	"check failed",
	"checks failed",
	"verification failed",
	"lint failed",
	"would be reformatted",
] as const;

/** Split a shell command into separately-analysable segments. */
export function commandSegments(command: string): string[] {
	return command
		.split(/&&|\|\||;|\n|\|/)
		.map((s) => s.trim())
		.filter((s) => s.length > 0);
}

/**
 * A stable key for "the same action", ignoring flags and operands: the program
 * plus its first non-flag word (`git commit -m x --no-verify` -> `git commit`).
 * Subcommand-less programs key on the program alone (`make` -> `make`), except
 * where the operand IS the action (`make check-all`).
 */
export function actionKey(segment: string): string | null {
	const words = segment.split(/\s+/).filter((w) => w.length > 0);
	// Drop leading env assignments (FOO=bar cmd) and `sudo`/`command`/`time`.
	let i = 0;
	while (
		i < words.length &&
		(/^[A-Z_][A-Z0-9_]*=/.test(words[i]) || ["sudo", "command", "time", "env"].includes(words[i]))
	)
		i++;
	const program = words[i];
	if (!program) return null;
	const rest = words.slice(i + 1).filter((w) => !w.startsWith("-"));
	// `make`/`just`/`npm run` style: the target is the action.
	const sub = rest[0];
	return sub ? `${program} ${sub}` : program;
}

/** Bypass flags present in a segment, normalized and deduped. */
export function bypassFlags(segment: string): string[] {
	const words = segment.split(/\s+/);
	const found = new Set<string>();
	for (const word of words) {
		// Exact match only: never substring. `--force-with-lease` is its own
		// entry, and `-n` must not match `-name` or a bundled `-nv`.
		if ((BYPASS_FLAGS as readonly string[]).includes(word)) found.add(word);
	}
	return [...found];
}

/** Does this output read like a gate declining, rather than a plain failure? */
export function looksLikeGate(output: string): boolean {
	const haystack = output.toLowerCase();
	return (GATE_MARKERS as readonly string[]).some((m) => haystack.includes(m));
}

interface Refusal {
	command: string;
	flags: string[];
	excerpt: string;
	/** Did the output read like a gate declining, or was it a bare failure? */
	gateWorded: boolean;
}

const NUDGE =
	"A check refused this action, and it was retried with the check disabled. " +
	"Skipping a gate removes the verification rather than satisfying it, even when the underlying action was asked for. " +
	"Report the refusal and its message, then either fix the cause or ask — do not claim this step verified.";

function excerptOf(text: string): string {
	const line = text
		.split("\n")
		.map((l) => l.trim())
		.find((l) => l.length > 0 && looksLikeGate(l));
	return (line ?? text.trim().split("\n")[0] ?? "").slice(0, 160);
}

function resultText(event: { content?: unknown }): string {
	const content = event.content;
	if (typeof content === "string") return content;
	if (!Array.isArray(content)) return "";
	return content
		.map((part) => (part && typeof part === "object" && "text" in part ? String((part as { text: unknown }).text) : ""))
		.join("\n");
}

export default function refusalExtension(pi: ExtensionAPI): void {
	// Session-scoped: a refusal in a previous session says nothing about this one.
	let refusals = new Map<string, Refusal>();

	pi.on("tool_result", async (event) => {
		if (event.toolName !== "bash" || !event.isError) return;
		const command = typeof event.input?.command === "string" ? event.input.command : "";
		if (!command) return;
		const text = resultText(event);
		// pi throws "Command aborted" / "Command timed out" as errors too. Neither
		// is a refusal, and neither should arm a gate-only nudge either.
		if (/^Command (aborted|timed out)/m.test(text.trim())) return;

		// Every failure is recorded, but `gateWorded` decides which flags can act
		// on it later: a bare failure only arms the flags that exist solely to
		// skip checks.
		const gateWorded = looksLikeGate(text);
		for (const segment of commandSegments(command)) {
			const key = actionKey(segment);
			if (!key) continue;
			refusals.set(key, {
				command: segment,
				flags: bypassFlags(segment),
				excerpt: excerptOf(text),
				gateWorded,
			});
		}
	});

	pi.on("tool_call", async (event, ctx: ExtensionContext) => {
		if (!isToolCallEventType("bash", event)) return;

		for (const segment of commandSegments(event.input.command)) {
			const key = actionKey(segment);
			if (!key) continue;
			const refused = refusals.get(key);
			if (!refused) continue;

			// Only flags NEW since the refusal count. A command refused while
			// already carrying `--force` and retried with it is not a bypass.
			const added = bypassFlags(segment).filter((f) => !refused.flags.includes(f));
			// A bare failure (no gate wording) is only actionable for flags that
			// have no purpose besides skipping a check.
			const actionable = refused.gateWorded ? added : added.filter(isGateOnlyFlag);
			if (actionable.length === 0) continue;

			// One nudge per refusal: repeats stack and train the agent to ignore it.
			refusals.delete(key);
			const detail = refused.excerpt ? `\n${refused.excerpt}` : "";
			ctx.ui.notify(`Bypassing a refused check: ${key} ${actionable.join(" ")}`, "warning");
			pi.sendMessage(
				{
					customType: "refusal",
					content: `${NUDGE}\n\nRefused: \`${refused.command}\`${detail}\nRetried with: ${actionable.join(" ")}`,
					display: true,
				},
				{ deliverAs: "steer" },
			);
			return; // one nudge per call, even if several segments match
		}
	});

	pi.on("session_shutdown", async () => {
		refusals = new Map();
	});
}
