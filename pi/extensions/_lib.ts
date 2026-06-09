/**
 * Shared helpers for local Pi extensions.
 *
 * This file is intentionally a top-level `*.ts` next to the real extensions:
 * `dotfiles-sync --apply` symlinks top-level files only, and pi/pi-meta may
 * auto-load every such file. Keep the no-op default export at the bottom so it
 * is also safe to load as an extension.
 */

import type { Api, AssistantMessage, Model } from "@earendil-works/pi-ai";

export interface ContentBlock {
	type?: string;
	text?: string;
	name?: string;
	arguments?: Record<string, unknown>;
}

export interface MessageLike {
	role?: string;
	content?: unknown;
	stopReason?: string;
}

export interface BranchEntryLike {
	type: string;
	message?: MessageLike;
}

export function blockText(content: unknown): string {
	if (typeof content === "string") return content;
	if (!Array.isArray(content)) return "";
	const parts: string[] = [];
	for (const part of content) {
		if (!part || typeof part !== "object") continue;
		const b = part as ContentBlock;
		if (b.type === "text" && typeof b.text === "string") parts.push(b.text);
		else if (b.type === "toolCall" && typeof b.name === "string")
			parts.push(`[tool ${b.name} ${JSON.stringify(b.arguments ?? {})}]`);
		else if (b.type === "toolResult" && typeof b.text === "string") parts.push(`[tool result] ${b.text}`);
	}
	return parts.join("\n");
}

export function conversationTranscript(branch: BranchEntryLike[], budget: number): string {
	const lines: string[] = [];
	for (const entry of branch) {
		if (entry.type !== "message" || !entry.message?.role) continue;
		const role = entry.message.role;
		if (role !== "user" && role !== "assistant") continue;
		const text = blockText(entry.message.content).trim();
		if (text) lines.push(`${role === "user" ? "User" : "Assistant"}: ${text}`);
	}
	const joined = lines.join("\n\n");
	return joined.length > budget ? joined.slice(-budget) : joined;
}

export type LastAssistantTextResult =
	| { kind: "ok"; text: string }
	| { kind: "incomplete"; stopReason: string }
	| { kind: "none" };

export function lastCompletedAssistantText(branch: BranchEntryLike[]): LastAssistantTextResult {
	for (let i = branch.length - 1; i >= 0; i--) {
		const entry = branch[i];
		if (entry.type !== "message" || entry.message?.role !== "assistant") continue;
		const stopReason = entry.message.stopReason;
		if (stopReason && stopReason !== "stop") return { kind: "incomplete", stopReason };
		const text = blockText(entry.message.content).trim();
		if (text) return { kind: "ok", text };
	}
	return { kind: "none" };
}

export function textContent(content: AssistantMessage["content"]): string {
	return content
		.filter((c): c is { type: "text"; text: string } => c.type === "text")
		.map((c) => c.text)
		.join("")
		.trim();
}

export interface ModelPickerContext {
	model?: Model<Api>;
	modelRegistry: {
		getAvailable(): Model<Api>[];
		getApiKeyAndHeaders(
			model: Model<Api>,
		): Promise<{ ok: true; apiKey?: string; headers?: Record<string, string> } | { ok: false; error: string }>;
	};
}

export async function pickSessionModel(
	ctx: ModelPickerContext,
): Promise<{ model: Model<Api>; apiKey: string; headers?: Record<string, string> } | null> {
	const candidates: Model<Api>[] = [];
	if (ctx.model) candidates.push(ctx.model);
	candidates.push(...ctx.modelRegistry.getAvailable());
	for (const m of candidates) {
		const a = await ctx.modelRegistry.getApiKeyAndHeaders(m);
		if (a?.ok && a.apiKey) return { model: m, apiKey: a.apiKey, headers: a.headers };
	}
	return null;
}

// Safe no-op extension entrypoint: pi/pi-meta may auto-load every top-level
// *.ts file, so helper modules must also be valid extensions.
export default function () {}
