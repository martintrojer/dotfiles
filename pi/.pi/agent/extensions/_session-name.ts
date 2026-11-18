/**
 * Pure helpers for `/namesession` (name-session.ts): the transcript digest the
 * title is generated from, the prompt, and reply parsing. Kept free of pi
 * imports so `node --test` and offline dry runs can load them directly.
 */

import { type BranchEntryLike, blockText, conversationTranscript } from "./_lib.ts";

const FIRST_PROMPT_CHARS = 1500;
const PROMPT_CHARS = 200;
const REPLY_CHARS = 400;
const MAX_NAME_CHARS = 60;

function clip(text: string, max: number): string {
	const flat = text.replace(/\s+/g, " ").trim();
	return flat.length > max ? `${flat.slice(0, max - 1)}…` : flat;
}

/** User prompts and assistant prose (tool calls dropped: they say how, not what). */
export function turnTexts(branch: BranchEntryLike[]): { prompts: string[]; replies: string[] } {
	const prompts: string[] = [];
	const replies: string[] = [];
	for (const entry of branch) {
		if (entry.type !== "message") continue;
		const role = entry.message?.role;
		if (role !== "user" && role !== "assistant") continue;
		const content = entry.message?.content;
		const text = blockText(
			role === "assistant" && Array.isArray(content)
				? content.filter((b) => (b as { type?: unknown } | null)?.type === "text")
				: content,
		).trim();
		if (text) (role === "user" ? prompts : replies).push(text);
	}
	return { prompts, replies };
}

// --- when to (re)name ------------------------------------------------------

/**
 * Refresh an extension-set name once the prompt count reaches REFRESH_GROWTH
 * times the count it was named at, but never before REFRESH_MIN_PROMPTS. From
 * a first-turn name that gives 10, 30, 90, 270...: the thin first title ("pull
 * repos") is fixed early, long sessions that change direction are still
 * caught, and calls stay logarithmic in session length so names settle.
 */
export const REFRESH_MIN_PROMPTS = 10;
export const REFRESH_GROWTH = 3;

/** customType of the entry recording each name this extension set. */
export const NAME_RECORD_TYPE = "namesession";

export interface NameRecord {
	name: string;
	prompts: number;
	hint?: string;
}

/**
 * The extension's last record, if it still owns the name: the record comes
 * after the latest `session_info` entry and carries its name. Order, not the
 * name string, decides: a later `/name` to the same string still wins.
 */
export function ownedNameRecord(entries: readonly unknown[]): NameRecord | undefined {
	let record: NameRecord | undefined;
	for (let i = entries.length - 1; i >= 0; i--) {
		const e = entries[i] as { type?: unknown; customType?: unknown; name?: unknown; data?: Partial<NameRecord> } | null;
		if (e?.type === "session_info") return record?.name === e.name ? record : undefined;
		if (record || e?.type !== "custom" || e.customType !== NAME_RECORD_TYPE) continue;
		const d = e.data;
		if (typeof d?.name === "string" && typeof d.prompts === "number") record = d as NameRecord;
	}
	return undefined;
}

/**
 * Whether to auto-name now. An unnamed session is named once it has a prompt
 * and a reply. A name this extension still owns (`ownedNameRecord`) is
 * refreshed as the session grows (see REFRESH_GROWTH). Any other name came
 * from `/name` or `--name` and is never touched.
 */
export function autoNameDue(
	current: string | undefined,
	owned: NameRecord | undefined,
	prompts: number,
	replies: number,
): boolean {
	if (prompts === 0 || replies === 0) return false;
	if (!current) return true;
	if (owned?.name !== current) return false;
	return prompts >= Math.max(REFRESH_MIN_PROMPTS, REFRESH_GROWTH * owned.prompts);
}

// --- digest, prompt, reply parsing -------------------------------------------

/**
 * What the namer sees. Short sessions get the whole transcript. Long ones get a
 * digest built around the user's prompts, which carry the intent: the first in
 * full, every later one clipped, and only the first and last assistant replies.
 * When the prompts alone overflow, the middle ones are dropped, never the head:
 * keeping the tail only (the old behaviour) named sessions after their wrap-up.
 */
export function sessionDigest(branch: BranchEntryLike[], budget: number): string {
	const full = conversationTranscript(branch, Number.POSITIVE_INFINITY);
	if (full.length <= budget) return full;

	const { prompts, replies } = turnTexts(branch);

	const head = [`First user prompt:\n${clip(prompts[0] ?? "", FIRST_PROMPT_CHARS)}`];
	if (replies.length > 0) head.push(`First assistant reply:\n${clip(replies[0], REPLY_CHARS)}`);
	const tail = replies.length > 1 ? [`Last assistant reply:\n${clip(replies[replies.length - 1], REPLY_CHARS)}`] : [];

	const later = prompts.slice(1).map((p) => `- ${clip(p, PROMPT_CHARS)}`);
	const title = `Later user prompts, in order (${later.length}):`;
	let room = budget - [...head, ...tail, title].join("\n\n").length - 60; // 60: separators + omission marker
	const front: string[] = [];
	const back: string[] = [];
	let i = 0;
	let j = later.length - 1;
	while (i <= j) {
		const takeFront = front.length <= back.length;
		const line = takeFront ? later[i] : later[j];
		if (line.length + 1 > room) break;
		room -= line.length + 1;
		if (takeFront) front.push(later[i++]);
		else back.unshift(later[j--]);
	}
	const omitted = j - i + 1;
	const middle = omitted > 0 ? [...front, `- … ${omitted} prompts omitted …`, ...back] : [...front, ...back];

	return [...head, later.length ? `${title}\n${middle.join("\n")}` : "", ...tail].filter(Boolean).join("\n\n");
}

/**
 * `keepCurrent` (auto refresh) asks the model to keep the current name unless
 * the goal has clearly moved, so names settle instead of churning; a manual
 * `/namesession` asks for a fresh one.
 */
export function buildNamePrompt(
	transcript: string,
	opts: { current?: string; hint?: string; keepCurrent?: boolean } = {},
): string {
	const { current, hint, keepCurrent } = opts;
	const currentLine = !current
		? ""
		: keepCurrent
			? `\nCurrent name: ${current}\nIf it still fits the main goal, reply with it unchanged. Replace it only when the session's main goal has clearly changed or the current name is too vague to find it again.`
			: `\nCurrent name, if useful context: ${current}`;
	return [
		"Write a short title for this coding-agent work item.",
		"A human will later scan and search a long list of these to find this exact one, so make it specific and unique.",
		"Name the main goal of the whole session, read from start to finish. The first user prompt usually states it.",
		"The final turns are often wrap-up (commit, push, release, cleanup, teardown, rebase); do not name the session after them unless they were the main work.",
		"Lead with the most distinctive searchable keyword for that goal (the concrete project, file, command, symbol, bug, or feature).",
		"Rules:",
		"- 3 to 6 words, Title Case or a concise verb phrase",
		"- Never use the words Session, Chat, Conversation, or Thread (even when the work is about them; name the specific thing instead)",
		"- Specific over generic; avoid vague filler like Extension, Feature, Task, Work, Update, Changes, Improvements",
		"- No quotes, emojis, trailing period, or model names",
		"",
		"Good examples (specific, easy to find again):",
		"- Auto-Name Pi History",
		"- Fix Tmux Status Bar Tests",
		"- Fix Article URL Fallbacks",
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
		currentLine,
		hint ? `\nUser hint: ${hint}` : "",
		`\n<transcript>\n${transcript}\n</transcript>`,
	]
		.filter(Boolean)
		.join("\n");
}

// Trust the model: extract the name from its JSON reply, do minimal cleanup,
// and soft-truncate for display. No content gating — the prompt does the steering.
export function extractName(raw: string): string {
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
		.replace(/[`"]/g, "")
		.replace(/\s+/g, " ")
		.trim()
		.replace(/^'+|'+$/g, "") // wrapping quotes only: keep "Don't"
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

// Safe no-op extension entrypoint: pi may auto-load every top-level *.ts file.
export default function () {}
