/**
 * Shared helpers for local Pi extensions.
 *
 * This file is intentionally a top-level `*.ts` next to the real extensions:
 * `dotfiles-sync --apply` symlinks top-level files only, and pi/pi-meta may
 * auto-load every such file. Keep the no-op default export at the bottom so it
 * is also safe to load as an extension.
 */

import type { ExtensionAPI, ExtensionCommandContext, ExtensionContext } from "@earendil-works/pi-coding-agent";
import type { Api, AssistantMessage, Model } from "@earendil-works/pi-ai";
import { exec } from "node:child_process";
import { createHash } from "node:crypto";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, isAbsolute, join, relative, resolve, sep } from "node:path";
import { promisify } from "node:util";

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

// --- interval parsing (shared by /loop and /watch) ----------------------

/** Parse a leading interval token like `30s`, `5m`, `2h`, or bare `5` (minutes). */
export function parseInterval(token: string): number | null {
	const m = /^(\d+(?:\.\d+)?)(s|m|h)?$/i.exec(token);
	if (!m) return null;
	const value = parseFloat(m[1]);
	const unit = (m[2] ?? "m").toLowerCase();
	const factor = unit === "s" ? 1000 : unit === "h" ? 3600_000 : 60_000;
	return value * factor;
}

export function formatInterval(ms: number): string {
	if (ms % 3600_000 === 0) return `${ms / 3600_000}h`;
	if (ms % 60_000 === 0) return `${ms / 60_000}m`;
	return `${Math.round(ms / 1000)}s`;
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

// --- /loop + /watch shared scheduler -------------------------------------

const execAsync = promisify(exec);
const DEFAULT_INTERVAL_MS = 10 * 60 * 1000;
const MIN_INTERVAL_MS = 5 * 1000;
const PROBE_TIMEOUT_MS = 60 * 1000;

type WatchLoopMode = "loop" | "watch";
type WatchLoopItem = {
	id: number;
	intervalMs: number;
	prompt: string;
	timer: ReturnType<typeof setInterval>;
	count: number;
	probe?: string;
	command?: string;
	lastHash?: string | null;
	warnedError?: boolean;
	running?: boolean;
};

type WatchLoopStore = {
	mode: WatchLoopMode;
	label: string;
	glyph: string;
	items: Map<number, WatchLoopItem>;
	nextId: number;
};

const watchLoopStores: Record<WatchLoopMode, WatchLoopStore> = {
	loop: { mode: "loop", label: "loop", glyph: "⟳", items: new Map(), nextId: 1 },
	watch: { mode: "watch", label: "watch", glyph: "👁", items: new Map(), nextId: 1 },
};

function sha256(s: string): string {
	return createHash("sha256").update(s).digest("hex");
}

type ProbeResult = { kind: "output"; text: string } | { kind: "error"; message: string };

// Run a watch probe. We diff stdout for change detection regardless of the exit
// code: many useful probes (e.g. `gh pr checks`) exit non-zero precisely when
// the state we care about is present (pending/failing CI). execAsync rejects on
// non-zero exit, but the rejection still carries stdout/stderr, so we treat a
// command that *ran* (numeric exit code) as a normal output. Only genuine spawn
// failures (command not found) and timeouts are reported as errors.
async function runProbe(command: string): Promise<ProbeResult> {
	try {
		const { stdout, stderr } = await execAsync(command, { timeout: PROBE_TIMEOUT_MS, maxBuffer: 10 * 1024 * 1024 });
		const out = stdout.trim() || stderr.trim();
		return { kind: "output", text: out };
	} catch (err: unknown) {
		const e = err as {
			code?: unknown;
			killed?: boolean;
			signal?: string;
			stdout?: string;
			stderr?: string;
			message?: string;
		};
		// Timeout: node sets killed=true and a signal. Treat as a real error so the
		// user learns the probe is too slow rather than silently never firing.
		if (e.killed || e.signal) return { kind: "error", message: `timed out after ${formatInterval(PROBE_TIMEOUT_MS)}` };
		// 126/127 are the shell's reserved "not executable"/"command not found" codes;
		// real probes (gh: 0/1/8, git: 0/1/128) never use them, so treat these as a
		// misconfigured-probe error rather than stable output the user won't notice.
		if (e.code === 126 || e.code === 127) {
			return { kind: "error", message: (e.stderr ?? "").trim() || e.message || `exited ${e.code}` };
		}
		// Ran but exited non-zero (numeric code): diff its output like any other.
		if (typeof e.code === "number") {
			const out = (e.stdout ?? "").trim() || (e.stderr ?? "").trim();
			return { kind: "output", text: out };
		}
		// Spawn failure (e.g. ENOENT): a real error.
		return { kind: "error", message: e.message ?? String(err) };
	}
}

function splitInterval(input: string): { intervalMs: number; rest: string } {
	const parts = input.split(/\s+/);
	const maybe = parseInterval(parts[0] ?? "");
	if (maybe !== null && parts.length > 1) return { intervalMs: maybe, rest: parts.slice(1).join(" ") };
	return { intervalMs: DEFAULT_INTERVAL_MS, rest: input };
}

// Pre-cooked probes for `/watch`. Each maps a short name to a deterministic,
// human-readable command whose output only changes when the thing you care
// about changes (no relative timestamps, color, or width-dependent wrapping —
// those churn on their own and cause spurious wakes). Trailing args typed after
// the name are appended verbatim, e.g. `gh-pr 1234` -> `gh pr checks 1234`.
export const NAMED_PROBES: Record<string, { command: string; description: string }> = {
	"gh-pr": { command: "gh pr checks", description: "CI checks on a PR (no arg = current branch's PR)" },
	"gh-ci": { command: "gh run list -L 5", description: "recent GitHub Actions workflow runs" },
	"git-remote": { command: "git ls-remote origin HEAD", description: "remote HEAD moved (someone pushed)" },
	"git-status": { command: "git status --porcelain", description: "working tree / index changed" },
	"git-log": { command: "git log -1 --format=%H origin/HEAD", description: "upstream HEAD commit moved" },
};

/** Expand a named probe (with optional trailing args) to a shell command; pass
 * through anything that isn't a known name as a literal command. */
export function expandProbe(probe: string): string {
	const [name, ...rest] = probe.split(/\s+/);
	const named = NAMED_PROBES[name];
	if (!named) return probe;
	return rest.length > 0 ? `${named.command} ${rest.join(" ")}` : named.command;
}

function parseProbeAndPrompt(rest: string): { probe: string; command: string; prompt: string } | null {
	const open = rest.indexOf("`");
	const close = open === -1 ? -1 : rest.indexOf("`", open + 1);
	if (open === -1 || close === -1) return null;
	const probe = rest.slice(open + 1, close).trim();
	const prompt = rest.slice(close + 1).trim();
	if (!probe || !prompt) return null;
	return { probe, command: expandProbe(probe), prompt };
}

function clearWatchLoopItem(store: WatchLoopStore, item: WatchLoopItem): void {
	clearInterval(item.timer);
	store.items.delete(item.id);
}

function clearWatchLoopStore(store: WatchLoopStore): void {
	for (const item of store.items.values()) clearInterval(item.timer);
	store.items.clear();
}

function formatWatchLoopSummary(): string {
	return (Object.values(watchLoopStores) as WatchLoopStore[])
		.flatMap((store) =>
			[...store.items.values()].map(
				(i) =>
					`${store.glyph} ${store.label} #${i.id} ${formatInterval(i.intervalMs)}${store.mode === "watch" && i.count > 0 ? ` (${i.count} hits)` : ""}`,
			),
		)
		.join(" ");
}

function formatTokens(count: number): string {
	if (count < 1000) return count.toString();
	if (count < 10000) return `${(count / 1000).toFixed(1)}k`;
	if (count < 1000000) return `${Math.round(count / 1000)}k`;
	if (count < 10000000) return `${(count / 1000000).toFixed(1)}M`;
	return `${Math.round(count / 1000000)}M`;
}

function formatCwdForWatchLoopFooter(cwd: string, home: string | undefined): string {
	if (!home) return cwd;
	const resolvedCwd = resolve(cwd);
	const resolvedHome = resolve(home);
	const rel = relative(resolvedHome, resolvedCwd);
	const insideHome = rel === "" || (rel !== ".." && !rel.startsWith(`..${sep}`) && !isAbsolute(rel));
	return insideHome ? (rel === "" ? "~" : `~${sep}${rel}`) : cwd;
}

function truncatePlain(s: string, width: number): string {
	if (s.length <= width) return s;
	if (width <= 3) return s.slice(0, width);
	return `${s.slice(0, width - 3)}...`;
}

let goalFooterMarker: string | null = null;
let watchLoopFooterInstalled = false;
let requestWatchLoopFooterRender: (() => void) | undefined;

function footerHasMarkers(): boolean {
	return goalFooterMarker !== null || Object.values(watchLoopStores).some((s) => s.items.size > 0);
}

export function setGoalFooterMarker(ctx: ExtensionContext, marker: string | null): void {
	goalFooterMarker = marker;
	syncWatchLoopFooter(ctx);
}

function syncWatchLoopFooter(ctx: ExtensionContext): void {
	const active = footerHasMarkers();
	if (!active) {
		if (watchLoopFooterInstalled) ctx.ui.setFooter(undefined);
		watchLoopFooterInstalled = false;
		requestWatchLoopFooterRender = undefined;
		return;
	}
	if (watchLoopFooterInstalled) {
		requestWatchLoopFooterRender?.();
		return;
	}
	watchLoopFooterInstalled = true;
	ctx.ui.setFooter((tui, theme, footerData) => {
		requestWatchLoopFooterRender = () => tui.requestRender();
		const unsub = footerData.onBranchChange(() => tui.requestRender());
		return {
			dispose: unsub,
			invalidate() {},
			render(width: number): string[] {
				let input = 0;
				let output = 0;
				let cacheRead = 0;
				let cacheWrite = 0;
				let cost = 0;
				for (const entry of ctx.sessionManager.getBranch()) {
					if (entry.type !== "message" || entry.message?.role !== "assistant") continue;
					const usage = entry.message.usage;
					if (!usage) continue;
					input += usage.input;
					output += usage.output;
					cacheRead += usage.cacheRead;
					cacheWrite += usage.cacheWrite;
					cost += usage.cost.total;
				}

				let cwd = formatCwdForWatchLoopFooter(ctx.sessionManager.getCwd(), process.env.HOME || process.env.USERPROFILE);
				const branch = footerData.getGitBranch();
				if (branch) cwd = `${cwd} (${branch})`;
				const sessionName = ctx.sessionManager.getSessionName();
				if (sessionName) cwd = `${cwd} • ${sessionName}`;
				const loopSummary = formatWatchLoopSummary();
				const summary = [goalFooterMarker, loopSummary].filter(Boolean).join(" ");
				const first = `${cwd}${summary ? ` • ${summary}` : ""}`;

				const parts: string[] = [];
				if (input) parts.push(`↑${formatTokens(input)}`);
				if (output) parts.push(`↓${formatTokens(output)}`);
				if (cacheRead) parts.push(`R${formatTokens(cacheRead)}`);
				if (cacheWrite) parts.push(`W${formatTokens(cacheWrite)}`);
				if (cost) parts.push(`$${cost.toFixed(3)}`);
				const usage = ctx.getContextUsage();
				const percent = usage?.percent === null ? "?" : (usage?.percent ?? 0).toFixed(1);
				parts.push(`${percent}%/${formatTokens(usage?.contextWindow ?? ctx.model?.contextWindow ?? 0)} (auto)`);

				const left = parts.join(" ");
				const right = ctx.model?.id ?? "no-model";
				const pad = " ".repeat(Math.max(1, width - left.length - right.length));
				return [
					theme.fg("dim", truncatePlain(first, width)),
					theme.fg("dim", truncatePlain(left + pad + right, width)),
				];
			},
		};
	});
}

function updateWatchLoopStatus(_store: WatchLoopStore, ctx: ExtensionCommandContext): void {
	syncWatchLoopFooter(ctx);
}

function watchLoopCompletions(store: WatchLoopStore, prefix: string) {
	const syntax = store.mode === "watch" ? "/watch <interval> `<probe>` <prompt>" : "/loop <interval> <prompt>";
	const trimmed = prefix.trimStart();

	// Named-probe completion (watch only): when the cursor sits inside an open
	// backtick, offer the pre-cooked probe names. An odd backtick count means the
	// last backtick is still open.
	if (store.mode === "watch" && (prefix.match(/`/g)?.length ?? 0) % 2 === 1) {
		const probePrefix = prefix.slice(prefix.lastIndexOf("`") + 1).trimStart();
		const names = Object.entries(NAMED_PROBES)
			.filter(([name]) => name.startsWith(probePrefix))
			.map(([name, { command, description }]) => ({
				value: name,
				label: name,
				description: `${command} — ${description}`,
			}));
		return names.length > 0 ? names : null;
	}

	// `stop <id>` — offer the active item ids once `stop ` has been typed, so users
	// can target a specific entry.
	const stopMatch = /^stop\s+(\S*)$/i.exec(trimmed);
	if (stopMatch) {
		const idPrefix = stopMatch[1];
		const ids = [...store.items.values()]
			.map((i) => ({
				value: `stop ${i.id}`,
				label: `stop ${i.id}`,
				description:
					store.mode === "watch"
						? `every ${formatInterval(i.intervalMs)} · \`${i.probe}\` (${i.count} hits)`
						: `every ${formatInterval(i.intervalMs)} (${i.count} ticks)`,
			}))
			.filter((i) => String(i.value.split(/\s+/)[1]).startsWith(idPrefix));
		return ids.length > 0 ? ids : null;
	}

	const items = [
		{
			value: "list",
			label: "list",
			description: `show active ${store.label}s`,
		},
		{
			value: "stop",
			label: "stop",
			description: `cancel all ${store.label}s, or "stop <id>" for one`,
		},
	];
	const filtered = items.filter((i) => i.value.startsWith(trimmed));
	// On an empty prefix, lead with the full invocation syntax so the help popup
	// also advertises the primary use (schedule a new item), not just subcommands.
	// For watch, also list the pre-cooked named probes so they're discoverable
	// without first typing a backtick. These are reference-only (value ""): the
	// arg-completion apply path would clobber the whole line, so they don't insert.
	if (trimmed === "") {
		const head = [{ value: "", label: syntax, description: `schedule a new ${store.label}` }, ...items];
		if (store.mode !== "watch") return head;
		const probes = Object.entries(NAMED_PROBES).map(([name, { command, description }]) => ({
			value: "",
			label: `\`${name}\``,
			description: `${command} — ${description}`,
		}));
		return [...head, ...probes];
	}
	return filtered.length > 0 ? filtered : null;
}

function handleWatchLoopList(store: WatchLoopStore, ctx: ExtensionCommandContext, usage: string): void {
	updateWatchLoopStatus(store, ctx);
	if (store.items.size === 0) {
		ctx.ui.notify(`No active ${store.label}s. Usage: ${usage}`, "info");
		return;
	}
	const lines = [...store.items.values()].map((i) => {
		const count = store.mode === "loop" ? `${i.count} ticks` : `${i.count} hits`;
		const detail = store.mode === "watch" ? `  \`${i.probe}\`  ${i.prompt}` : `  ${i.prompt}`;
		return `#${i.id}  every ${formatInterval(i.intervalMs)}  (${count})${detail}`;
	});
	ctx.ui.notify(`Active ${store.label}s:\n${lines.join("\n")}`, "info");
}

function handleWatchLoopStop(store: WatchLoopStore, trimmed: string, ctx: ExtensionCommandContext): boolean {
	if (!/^stop\b/i.test(trimmed)) return false;
	const rest = trimmed.replace(/^stop\s*/i, "").trim();
	if (rest === "") {
		const n = store.items.size;
		clearWatchLoopStore(store);
		updateWatchLoopStatus(store, ctx);
		ctx.ui.notify(n > 0 ? `Stopped ${n} ${store.label}(s).` : `No active ${store.label}s.`, "info");
		return true;
	}
	const id = parseInt(rest, 10);
	const item = store.items.get(id);
	if (!item) {
		ctx.ui.notify(`No ${store.label} #${rest}.`, "warning");
		return true;
	}
	clearWatchLoopItem(store, item);
	updateWatchLoopStatus(store, ctx);
	ctx.ui.notify(`Stopped ${store.label} #${id}.`, "info");
	return true;
}

function addWatchLoopItem(
	store: WatchLoopStore,
	ctx: ExtensionCommandContext,
	item: Omit<WatchLoopItem, "id" | "timer" | "count">,
	tick: (id: number) => void,
): WatchLoopItem {
	const id = store.nextId++;
	const timer = setInterval(() => tick(id), item.intervalMs);
	if (typeof timer.unref === "function") timer.unref();
	const full = { ...item, id, timer, count: 0 };
	store.items.set(id, full);
	updateWatchLoopStatus(store, ctx);
	return full;
}

export function registerLoopCommand(pi: ExtensionAPI): void {
	const store = watchLoopStores.loop;
	pi.registerCommand("loop", {
		description: "Run a prompt on a recurring interval (e.g. /loop 5m check the deploy)",
		getArgumentCompletions: (prefix) => watchLoopCompletions(store, prefix),
		handler: async (args, ctx) => {
			const trimmed = (args ?? "").trim();
			if (trimmed === "" || trimmed === "list") {
				handleWatchLoopList(store, ctx, "/loop <interval> <prompt>");
				return;
			}
			if (handleWatchLoopStop(store, trimmed, ctx)) return;
			const { intervalMs, rest: prompt } = splitInterval(trimmed);
			if (prompt.trim() === "") return ctx.ui.notify("Nothing to loop. Usage: /loop <interval> <prompt>", "warning");
			if (intervalMs < MIN_INTERVAL_MS)
				return ctx.ui.notify(`Interval too short; minimum is ${formatInterval(MIN_INTERVAL_MS)}.`, "warning");
			const added = addWatchLoopItem(store, ctx, { intervalMs, prompt }, (id) => {
				const item = store.items.get(id);
				if (!item || !ctx.isIdle() || ctx.hasPendingMessages()) return;
				item.count++;
				pi.sendUserMessage(item.prompt);
			});
			ctx.ui.notify(
				`Loop #${added.id} scheduled every ${formatInterval(intervalMs)}. Stop with /loop stop ${added.id}.`,
				"info",
			);
		},
	});
	pi.on("session_shutdown", async () => clearWatchLoopStore(store));
}

export function registerWatchCommand(pi: ExtensionAPI): void {
	const store = watchLoopStores.watch;
	pi.registerCommand("watch", {
		description:
			"Run a prompt only when a probe's output changes (e.g. /watch 3m `gh-pr 1234` address new CI failures). Named probes: gh-pr, gh-ci, git-remote, git-status, git-log.",
		getArgumentCompletions: (prefix) => watchLoopCompletions(store, prefix),
		handler: async (args, ctx) => {
			const trimmed = (args ?? "").trim();
			if (trimmed === "" || trimmed === "list") {
				handleWatchLoopList(store, ctx, "/watch <interval> `<probe>` <prompt>");
				return;
			}
			if (handleWatchLoopStop(store, trimmed, ctx)) return;
			const { intervalMs, rest } = splitInterval(trimmed);
			const parsed = parseProbeAndPrompt(rest);
			if (!parsed) return ctx.ui.notify("Usage: /watch <interval> `<probe-command>` <prompt>", "warning");
			if (intervalMs < MIN_INTERVAL_MS)
				return ctx.ui.notify(`Interval too short; minimum is ${formatInterval(MIN_INTERVAL_MS)}.`, "warning");
			const added = addWatchLoopItem(
				store,
				ctx,
				{
					intervalMs,
					prompt: parsed.prompt,
					probe: parsed.probe,
					command: parsed.command,
					lastHash: null,
					warnedError: false,
					running: false,
				},
				(id) => {
					const item = store.items.get(id);
					if (!item || !ctx.isIdle() || ctx.hasPendingMessages() || item.running) return;
					item.running = true;
					void runProbe(item.command ?? item.probe ?? "")
						.then((result) => {
							if (result.kind === "error") {
								if (item.warnedError) return;
								item.warnedError = true;
								ctx.ui.notify(`Watch #${id} probe failed (kept alive): ${result.message}`, "warning");
								return;
							}
							item.warnedError = false;
							const output = result.text;
							const nextHash = sha256(output);
							if (item.lastHash === null) {
								item.lastHash = nextHash;
								return;
							}
							if (nextHash === item.lastHash) return;
							item.lastHash = nextHash;
							item.count++;
							updateWatchLoopStatus(store, ctx);
							pi.sendUserMessage(`${item.prompt}\n\n--- watch probe output ---\n${output}`);
						})
						.finally(() => {
							item.running = false;
						});
				},
			);
			const shown = parsed.command === parsed.probe ? `\`${parsed.probe}\`` : `\`${parsed.probe}\` (${parsed.command})`;
			ctx.ui.notify(
				`Watch #${added.id} armed every ${formatInterval(intervalMs)} on ${shown}. Stop with /watch stop ${added.id}.`,
				"info",
			);
		},
	});
	pi.on("session_shutdown", async () => clearWatchLoopStore(store));
}

// Safe no-op extension entrypoint: pi/pi-meta may auto-load every top-level
// *.ts file, so helper modules must also be valid extensions.
export default function () {}
