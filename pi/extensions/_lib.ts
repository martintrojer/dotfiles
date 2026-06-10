/**
 * Shared helpers for local Pi extensions.
 *
 * This file is intentionally a top-level `*.ts` next to the real extensions:
 * `dotfiles-sync --apply` symlinks top-level files only, and pi/pi-meta may
 * auto-load every such file. Keep the no-op default export at the bottom so it
 * is also safe to load as an extension.
 */

import type { Api, AssistantMessage, Model } from "@earendil-works/pi-ai";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";

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

// Fast/reliable model tiers for lightweight meta-work (question extraction,
// goal checks). Avoid size-only hints like "4b": those tend to select local
// Ollama models that are cheap but less reliable for strict JSON/judgment.
// Note: "-mini" is anchored so it matches gpt-*-mini / o*-mini but NOT gemini
// (which contains the substring "mini").
const FAST_MODEL_TIERS = [["-mini"], ["sonnet"], ["haiku"], ["flash"], ["small", "lite", "nano"]] as const;

function modelTier(m: Model<Api>): number {
	const id = modelLabel(m).toLowerCase();
	const tier = FAST_MODEL_TIERS.findIndex((hints) => hints.some((h) => id.includes(h)));
	return tier === -1 ? FAST_MODEL_TIERS.length : tier;
}

function isLocalModel(m: Model<Api>): boolean {
	const id = modelLabel(m).toLowerCase();
	return id.includes("ollama/") || id.includes("localhost") || id.includes("lmstudio");
}

function modelCost(m: Model<Api>): number {
	return (m.cost?.input ?? 0) + (m.cost?.output ?? 0);
}

// Best-effort "version" of a model id: every numeric run becomes a tuple
// component (gpt-5-mini -> [5], claude-sonnet-4-5 -> [4,5],
// gemini-2.0-flash -> [2,0]). Compared most-significant-first so the major
// version dominates. Only meaningful as a within-tier tiebreak, where models
// are usually the same family.
function modelVersion(m: Model<Api>): number[] {
	return (modelLabel(m).match(/\d+/g) ?? []).map(Number);
}

// Negative when `a` is newer than `b` (so newest sorts first).
function compareVersionDesc(a: Model<Api>, b: Model<Api>): number {
	const va = modelVersion(a);
	const vb = modelVersion(b);
	const len = Math.max(va.length, vb.length);
	for (let i = 0; i < len; i++) {
		const diff = (vb[i] ?? 0) - (va[i] ?? 0);
		if (diff !== 0) return diff;
	}
	return 0;
}

export function modelLabel(model: Model<Api>): string {
	return `${model.provider}/${model.id}`;
}

const MODEL_FAILURE_COOLDOWN_MS = 5 * 60 * 1000;
const STATE_PATH = join(homedir(), ".pi", "agent", "extension-state", "fast-models.json");

type FastModelRecord = {
	lastSeen: number;
	lastSuccess?: number;
	lastFailure?: number;
	failureCount?: number;
	lastError?: string;
};

type FastModelState = {
	version: 1;
	updatedAt: number;
	tasks: Record<string, Record<string, FastModelRecord>>;
};

function emptyState(): FastModelState {
	return { version: 1, updatedAt: Date.now(), tasks: {} };
}

function readFastModelState(): FastModelState {
	try {
		if (!existsSync(STATE_PATH)) return emptyState();
		const parsed = JSON.parse(readFileSync(STATE_PATH, "utf8")) as Partial<FastModelState>;
		if (parsed.version !== 1 || !parsed.tasks || typeof parsed.tasks !== "object") return emptyState();
		return { version: 1, updatedAt: parsed.updatedAt ?? 0, tasks: parsed.tasks };
	} catch {
		return emptyState();
	}
}

function writeFastModelState(state: FastModelState): void {
	try {
		mkdirSync(dirname(STATE_PATH), { recursive: true });
		state.updatedAt = Date.now();
		writeFileSync(STATE_PATH, JSON.stringify(state, null, 2) + "\n");
	} catch {
		// Best-effort cache; never break extension behavior on state IO errors.
	}
}

function taskRecords(state: FastModelState, task: string): Record<string, FastModelRecord> {
	state.tasks[task] ??= {};
	return state.tasks[task];
}

function recordFor(state: FastModelState, task: string, model: Model<Api>): FastModelRecord | undefined {
	return state.tasks[task]?.[modelLabel(model)];
}

function markModelsSeen(task: string, models: Model<Api>[]): FastModelState {
	const state = readFastModelState();
	const records = taskRecords(state, task);
	const now = Date.now();
	for (const model of models) {
		const label = modelLabel(model);
		records[label] = { ...records[label], lastSeen: now };
	}
	writeFastModelState(state);
	return state;
}

function modelFailedRecently(state: FastModelState, task: string, m: Model<Api>): boolean {
	const failedAt = recordFor(state, task, m)?.lastFailure;
	return failedAt !== undefined && Date.now() - failedAt < MODEL_FAILURE_COOLDOWN_MS;
}

function lastSuccess(state: FastModelState, task: string, m: Model<Api>): number {
	return recordFor(state, task, m)?.lastSuccess ?? 0;
}

export function markModelFailure(task: string, model: Model<Api>, error?: unknown): void {
	const state = readFastModelState();
	const records = taskRecords(state, task);
	const label = modelLabel(model);
	const prev = records[label];
	records[label] = {
		...prev,
		lastSeen: prev?.lastSeen ?? Date.now(),
		lastFailure: Date.now(),
		failureCount: (prev?.failureCount ?? 0) + 1,
		lastError: error instanceof Error ? error.message.slice(0, 500) : error ? String(error).slice(0, 500) : undefined,
	};
	writeFastModelState(state);
}

export function markModelSuccess(task: string, model: Model<Api>): void {
	const state = readFastModelState();
	const records = taskRecords(state, task);
	const label = modelLabel(model);
	const prev = records[label];
	records[label] = {
		...prev,
		lastSeen: prev?.lastSeen ?? Date.now(),
		lastSuccess: Date.now(),
		failureCount: 0,
		lastError: undefined,
	};
	writeFastModelState(state);
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

export type PickedModel = { model: Model<Api>; apiKey: string; headers?: Record<string, string> };

export async function pickFastModels(ctx: ModelPickerContext, task = "default"): Promise<PickedModel[]> {
	const available = ctx.modelRegistry.getAvailable();
	const state = markModelsSeen(task, available);
	const seen = new Set<string>();
	const candidates = [...available].sort((a, b) => {
		const fa = modelFailedRecently(state, task, a);
		const fb = modelFailedRecently(state, task, b);
		if (fa !== fb) return fa ? 1 : -1;
		const ta = modelTier(a);
		const tb = modelTier(b);
		if (ta !== tb) return ta - tb;
		const la = isLocalModel(a);
		const lb = isLocalModel(b);
		if (la !== lb) return la ? 1 : -1;
		const byVersion = compareVersionDesc(a, b); // within a tier, always prefer the newest
		if (byVersion !== 0) return byVersion;
		const sa = lastSuccess(state, task, a);
		const sb = lastSuccess(state, task, b);
		if (sa > 0 !== sb > 0) return sa > 0 ? -1 : 1;
		if (sa !== sb) return sb - sa;
		return modelCost(a) - modelCost(b);
	});
	if (ctx.model) candidates.push(ctx.model); // last-resort fallback to the active session model

	const picked: PickedModel[] = [];
	for (const m of candidates) {
		const k = modelLabel(m);
		if (seen.has(k)) continue;
		seen.add(k);
		const a = await ctx.modelRegistry.getApiKeyAndHeaders(m);
		if (a?.ok && a.apiKey) picked.push({ model: m, apiKey: a.apiKey, headers: a.headers });
	}
	return picked;
}

export async function pickFastModel(ctx: ModelPickerContext, task = "default"): Promise<PickedModel | null> {
	return (await pickFastModels(ctx, task))[0] ?? null;
}

export class FastModelCancelled extends Error {
	constructor() {
		super("Cancelled.");
		this.name = "FastModelCancelled";
	}
}

export type FastModelFallbackResult<T> =
	| { kind: "ok"; model: string; result: T }
	| { kind: "cancelled" }
	| { kind: "error"; message: string };

export async function withFastModelFallback<T>(
	ctx: ModelPickerContext,
	task: string,
	run: (picked: PickedModel) => Promise<T>,
): Promise<FastModelFallbackResult<T>> {
	const candidates = await pickFastModels(ctx, task);
	if (candidates.length === 0) return { kind: "error", message: "No model with configured auth is available." };

	let lastError: unknown;
	for (const picked of candidates) {
		try {
			const result = await run(picked);
			markModelSuccess(task, picked.model);
			return { kind: "ok", model: modelLabel(picked.model), result };
		} catch (err) {
			if (err instanceof FastModelCancelled) return { kind: "cancelled" };
			lastError = err;
			markModelFailure(task, picked.model, err);
		}
	}
	return { kind: "error", message: lastError instanceof Error ? lastError.message : String(lastError) };
}

// Safe no-op extension entrypoint: pi/pi-meta may auto-load every top-level
// *.ts file, so helper modules must also be valid extensions.
export default function () {}
