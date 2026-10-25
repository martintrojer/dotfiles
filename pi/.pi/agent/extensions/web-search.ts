/**
 * web_search: one vendor-neutral search tool over Brave LLM Context, Tavily
 * and Exa. Each call picks a random vendor among those with a key and no live
 * out-of-quota marker, falls over to the others one at a time on failure, and
 * never fans out. Quota and rate-limit failures write a TTL'd marker to a
 * shared state file so parallel sessions skip that vendor too.
 *
 * Design and rationale: docs/specs/2026-10-25-web-search-mux.md (local).
 */

import { StringEnum } from "@earendil-works/pi-ai";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { DEFAULT_MAX_BYTES, DEFAULT_MAX_LINES, formatSize, truncateHead } from "@earendil-works/pi-coding-agent";
import { Text } from "@earendil-works/pi-tui";
import { randomBytes } from "node:crypto";
import { mkdirSync, readFileSync, renameSync, rmSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { Type } from "typebox";

// --- Pure helpers (exported for tests) -----------------------------------

export type Vendor = "brave" | "tavily" | "exa";
export type ErrorClass = "quota" | "rate" | "auth" | "transient" | "bad_request";
export type Marker = { until: number; reason: "quota" | "rate"; detail: string };
export type State = Partial<Record<Vendor, Marker>>;

const QUOTA_TTL_MS = 24 * 60 * 60 * 1000;
const RATE_TTL_MS = 60 * 1000;

const QUOTA_STATUS: Record<Vendor, number[]> = {
	brave: [402],
	tavily: [432, 433],
	exa: [402],
};

// Plain fields, not parameter properties: node's strip-only TS mode rejects those.
export class VendorHttpError extends Error {
	readonly vendor: Vendor;
	readonly status: number;
	readonly detail: string;
	constructor(vendor: Vendor, status: number, detail: string) {
		super(`${vendor} ${status}${detail ? `: ${detail}` : ""}`);
		this.name = "VendorHttpError";
		this.vendor = vendor;
		this.status = status;
		this.detail = detail;
	}
}

export function classify(vendor: Vendor, error: unknown): ErrorClass {
	if (!(error instanceof VendorHttpError)) return "transient";
	const { status } = error;
	if (QUOTA_STATUS[vendor].includes(status)) return "quota";
	if (status === 429) return "rate";
	if (status === 401 || status === 403) return "auth";
	if (status >= 500) return "transient";
	return "bad_request";
}

export function pruneState(state: State, now: number): State {
	const out: State = {};
	for (const [vendor, marker] of Object.entries(state) as [Vendor, Marker][]) {
		if (marker && marker.until > now) out[vendor] = marker;
	}
	return out;
}

export function markVendor(state: State, vendor: Vendor, cls: ErrorClass, now: number, detail: string): State {
	if (cls !== "quota" && cls !== "rate") return state;
	const ttl = cls === "quota" ? QUOTA_TTL_MS : RATE_TTL_MS;
	return { ...state, [vendor]: { until: now + ttl, reason: cls, detail } };
}

function shuffle<T>(items: T[], rng: () => number): T[] {
	const out = [...items];
	for (let i = out.length - 1; i > 0; i--) {
		const j = Math.floor(rng() * (i + 1));
		[out[i], out[j]] = [out[j], out[i]];
	}
	return out;
}

/** Keyed vendors in attempt order: unmarked first (shuffled), then live-marked (shuffled). */
export function selectVendors(keyed: Vendor[], state: State, now: number, rng: () => number): Vendor[] {
	const live = pruneState(state, now);
	const unmarked = keyed.filter((v) => !live[v]);
	const marked = keyed.filter((v) => live[v]);
	return [...shuffle(unmarked, rng), ...shuffle(marked, rng)];
}

// --- State file (best-effort IO) -----------------------------------------

// Resolved per call, not at import, so the wiring test can point HOME at a temp dir.
const statePath = () => join(homedir(), ".pi", "agent", "extension-state", "web-search.json");

function readState(): State {
	try {
		const parsed = JSON.parse(readFileSync(statePath(), "utf8"));
		return parsed && typeof parsed === "object" && !Array.isArray(parsed) ? (parsed as State) : {};
	} catch {
		return {};
	}
}

function writeMarker(vendor: Vendor, cls: ErrorClass, detail: string): void {
	if (cls !== "quota" && cls !== "rate") return;
	const now = Date.now();
	const next = pruneState(markVendor(readState(), vendor, cls, now, detail), now);
	const path = statePath();
	const tmp = `${path}.${process.pid}.${randomBytes(4).toString("hex")}.tmp`;
	try {
		mkdirSync(dirname(path), { recursive: true });
		writeFileSync(tmp, `${JSON.stringify(next, null, "\t")}\n`);
		renameSync(tmp, path);
	} catch {
		// Best-effort: a broken state file must never fail a search, cleanup included.
		try {
			rmSync(tmp, { force: true });
		} catch {}
	}
}

// --- Vendor adapters -----------------------------------------------------

type Freshness = "day" | "week" | "month" | "year";

type SearchRequest = {
	query: string;
	count: number;
	maxTokens: number;
	domains: string[];
	freshness?: Freshness;
};

type WebResult = {
	title: string;
	url: string;
	hostname?: string;
	age?: string;
	snippets: string[];
};

type VendorResponse = { results: WebResult[]; notices: string[]; warnings: string[] };

const KEY_ENV: Record<Vendor, string> = {
	brave: "BRAVE_SEARCH_API_KEY",
	tavily: "TAVILY_API_KEY",
	exa: "EXA_API_KEY",
};

const ATTEMPT_TIMEOUT_MS = 30_000;
const FRESHNESS_DAYS: Record<Freshness, number> = { day: 1, week: 7, month: 31, year: 365 };

function str(value: unknown): string | undefined {
	return typeof value === "string" && value.trim().length > 0 ? value.trim() : undefined;
}

function strList(value: unknown): string[] {
	if (!Array.isArray(value)) return [];
	return value
		.map((item) => (typeof item === "string" ? item.trim() : item == null ? "" : JSON.stringify(item)))
		.filter((item) => item.length > 0);
}

function hostOf(domain: string): string {
	return domain
		.replace(/^[a-z]+:\/\//i, "")
		.split("/")[0]
		.replace(/^\*\./, "");
}

async function postJson(
	vendor: Vendor,
	url: string,
	headers: Record<string, string>,
	body: unknown,
	signal: AbortSignal,
): Promise<unknown> {
	const response = await fetch(url, {
		method: "POST",
		headers: { Accept: "application/json", "Content-Type": "application/json", ...headers },
		body: JSON.stringify(body),
		signal,
	});
	if (!response.ok) {
		const text = await response.text().catch(() => "");
		throw new VendorHttpError(vendor, response.status, text.slice(0, 300));
	}
	return response.json();
}

type Adapter = (req: SearchRequest, key: string, signal: AbortSignal) => Promise<VendorResponse>;

const brave: Adapter = async (req, key, signal) => {
	const hosts = [...new Set(req.domains.map(hostOf).filter(Boolean))];
	// No parentheses: live-tested 2026-10-25, `(site:a)` stops filtering; bare `site:a OR site:b` works.
	const sites = hosts.length > 0 ? ` ${hosts.map((h) => `site:${h}`).join(" OR ")}` : "";
	const body: Record<string, unknown> = {
		q: `${req.query}${sites}`,
		count: req.count,
		maximum_number_of_urls: req.count,
		maximum_number_of_tokens: req.maxTokens,
		operators: true,
		operators_fallback: false,
	};
	if (req.freshness) body.freshness = `p${req.freshness[0]}`;
	const payload = (await postJson(
		"brave",
		"https://api.search.brave.com/res/v1/llm/context",
		{ "X-Subscription-Token": key, "Accept-Encoding": "gzip" },
		body,
		signal,
	)) as {
		grounding?: { generic?: Record<string, unknown>[] };
		sources?: Record<string, Record<string, unknown>>;
	};
	const sources = payload.sources ?? {};
	const results: WebResult[] = [];
	for (const item of payload.grounding?.generic ?? []) {
		const url = str(item.url);
		if (!url) continue;
		const source = sources[url] ?? {};
		const age = strList(source.age);
		results.push({
			title: str(item.title) ?? str(source.title) ?? url,
			url,
			hostname: str(source.hostname),
			age: age[1] ?? age[0],
			snippets: strList(item.snippets),
		});
	}
	return { results, notices: [], warnings: [] };
};

const tavily: Adapter = async (req, key, signal) => {
	const body: Record<string, unknown> = {
		query: req.query,
		max_results: req.count,
		search_depth: "basic",
		include_published_date: true,
	};
	if (req.domains.length > 0) body.include_domains = req.domains;
	if (req.freshness) {
		body.time_range = req.freshness;
		body.filter_by_published_date = true;
	}
	const payload = (await postJson(
		"tavily",
		"https://api.tavily.com/search",
		{ Authorization: `Bearer ${key}` },
		body,
		signal,
	)) as { results?: Record<string, unknown>[] };
	const results: WebResult[] = [];
	for (const item of payload.results ?? []) {
		const url = str(item.url);
		if (!url) continue;
		const content = str(item.content);
		results.push({
			title: str(item.title) ?? url,
			url,
			age: str(item.published_date),
			snippets: content ? [content] : [],
		});
	}
	return { results, notices: [], warnings: [] };
};

const exa: Adapter = async (req, key, signal) => {
	const body: Record<string, unknown> = {
		query: req.query,
		type: "lite",
		numResults: req.count,
		contents: { highlights: { maxCharacters: Math.max(500, Math.floor((req.maxTokens * 4) / req.count)) } },
	};
	if (req.domains.length > 0) body.includeDomains = req.domains;
	if (req.freshness) {
		body.startPublishedDate = new Date(Date.now() - FRESHNESS_DAYS[req.freshness] * 86_400_000).toISOString();
	}
	const payload = (await postJson("exa", "https://api.exa.ai/search", { "x-api-key": key }, body, signal)) as {
		results?: Record<string, unknown>[];
		notices?: unknown[];
		warnings?: unknown[];
	};
	const results: WebResult[] = [];
	for (const item of payload.results ?? []) {
		const url = str(item.url);
		if (!url) continue;
		results.push({
			title: str(item.title) ?? url,
			url,
			age: str(item.publishedDate)?.slice(0, 10),
			snippets: strList(item.highlights),
		});
	}
	const messages = (list: unknown[] | undefined) =>
		(list ?? []).map((n) =>
			typeof n === "string" ? n : (str((n as { message?: unknown })?.message) ?? JSON.stringify(n)),
		);
	return { results, notices: messages(payload.notices), warnings: messages(payload.warnings) };
};

const ADAPTERS: Record<Vendor, Adapter> = { brave, tavily, exa };

// --- Formatting ----------------------------------------------------------

type Attempt = { vendor: Vendor; class?: ErrorClass; status?: number; ms: number; detail?: string };

function formatAttempt(a: Attempt, state: State): string {
	const marker = state[a.vendor];
	const until = marker ? `, until ${new Date(marker.until).toISOString().slice(0, 16).replace("T", " ")}` : "";
	const status = a.status ? ` ${a.status}` : "";
	return `${a.vendor} (${a.class}${status}${until})`;
}

function formatResult(result: WebResult, index: number): string {
	const lines = [`${index + 1}. ${result.title}`, `URL: ${result.url}`];
	if (result.age) lines.push(`Age: ${result.age}`);
	if (result.snippets.length > 0) {
		lines.push("Snippets:");
		for (const snippet of result.snippets) lines.push(`- ${snippet}`);
	}
	return lines.join("\n");
}

function truncate(text: string, maxTokens: number): { text: string; truncated: boolean } {
	const budget = maxTokens * 4;
	let body = text;
	let truncated = false;
	if (body.length > budget) {
		body = `${body.slice(0, budget)}\n\n[Output truncated to ~${maxTokens} tokens.]`;
		truncated = true;
	}
	const t = truncateHead(body, { maxLines: DEFAULT_MAX_LINES, maxBytes: DEFAULT_MAX_BYTES });
	if (!t.truncated) return { text: body, truncated };
	return {
		text: `${t.content}\n\n[Output truncated: ${t.outputLines} of ${t.totalLines} lines (${formatSize(t.outputBytes)} of ${formatSize(t.totalBytes)}).]`,
		truncated: true,
	};
}

// --- Tool ----------------------------------------------------------------

type WebSearchDetails = {
	query: string;
	/** Absent when every vendor failed. */
	vendor?: Vendor;
	attempts: Attempt[];
	count: number;
	maxTokens: number;
	domains: string[];
	freshness?: Freshness;
	resultCount: number;
	results: WebResult[];
	warnings: string[];
	truncated: boolean;
};

const WebSearchParams = Type.Object({
	query: Type.String({ description: "Search query" }),
	count: Type.Optional(Type.Number({ description: "Maximum results (1-20, default 10)", minimum: 1, maximum: 20 })),
	maxTokens: Type.Optional(
		Type.Number({
			description: "Approximate output budget in tokens (1024-32768, default 8192)",
			minimum: 1024,
			maximum: 32768,
		}),
	),
	domains: Type.Optional(
		Type.Array(Type.String(), {
			description: "Restrict results to these hostnames or host/path prefixes (e.g. github.com/swaywm/sway)",
		}),
	),
	freshness: Type.Optional(
		StringEnum(["day", "week", "month", "year"] as const, { description: "Only results from the last period" }),
	),
});

function clamp(value: number | undefined, fallback: number, min: number, max: number): number {
	if (value === undefined || !Number.isFinite(value)) return fallback;
	return Math.max(min, Math.min(max, Math.trunc(value)));
}

export default function webSearchExtension(pi: ExtensionAPI): void {
	const keys: Partial<Record<Vendor, string>> = {};
	for (const vendor of Object.keys(KEY_ENV) as Vendor[]) {
		const key = str(process.env[KEY_ENV[vendor]]);
		if (key) keys[vendor] = key;
	}
	const keyed = Object.keys(keys) as Vendor[];
	// fetch echoes an invalid header value (a key with CR/LF/NUL) in its TypeError; keep keys out of output.
	const redact = (text: string) =>
		Object.values(keys).reduce((acc, key) => (key ? acc.split(key).join("[redacted]") : acc), text);
	if (keyed.length === 0) return; // No keys: keep the tool out of the prompt.

	pi.registerTool({
		name: "web_search",
		label: "Web Search",
		description: `Search the web (vendors: ${keyed.join(", ")}). Returns extracted page snippets with URLs. Output is truncated to ${DEFAULT_MAX_LINES} lines or ${formatSize(DEFAULT_MAX_BYTES)}.`,
		promptSnippet:
			"Search the web for current information, extracted page content, documentation, references, or examples",
		promptGuidelines: [
			"Use web_search when the user asks for current web information, external documentation, references, examples, or research that is not available in the local workspace.",
			"Use web_search domains to restrict to trusted sources; use freshness for recent events.",
			"Cite URLs from web_search results when using web information in the answer.",
		],
		parameters: WebSearchParams,

		async execute(_toolCallId, params, signal, _onUpdate, _ctx) {
			const query = params.query.trim();
			if (!query) throw new Error("query must not be empty");
			const req: SearchRequest = {
				query,
				count: clamp(params.count, 10, 1, 20),
				maxTokens: clamp(params.maxTokens, 8192, 1024, 32768),
				domains: (params.domains ?? []).map((d) => d.trim()).filter(Boolean),
				freshness: params.freshness,
			};

			const order = selectVendors(keyed, readState(), Date.now(), Math.random);
			const attempts: Attempt[] = [];

			for (const vendor of order) {
				if (signal?.aborted) throw new Error("web_search aborted");
				const started = Date.now();
				const attemptSignal = signal
					? AbortSignal.any([signal, AbortSignal.timeout(ATTEMPT_TIMEOUT_MS)])
					: AbortSignal.timeout(ATTEMPT_TIMEOUT_MS);
				try {
					const res = await ADAPTERS[vendor](req, keys[vendor] as string, attemptSignal);
					// Brave ignores count/maximum_number_of_urls (27-30 results for count=3), so cap here for all.
					res.results = res.results.slice(0, req.count);
					attempts.push({ vendor, ms: Date.now() - started });
					const state = readState();
					const failed = attempts.filter((a) => a.class);
					const header = `Web results for: ${query} (via ${vendor})`;
					const parts = [header];
					if (failed.length > 0) parts.push(`skipped: ${failed.map((a) => formatAttempt(a, state)).join(", ")}`);
					for (const n of res.notices) parts.push(`notice: ${n}`);
					parts.push(res.results.length > 0 ? res.results.map(formatResult).join("\n\n") : "No results found.");
					const out = truncate(parts.join("\n\n"), req.maxTokens);
					return {
						content: [{ type: "text", text: out.text }],
						details: {
							query,
							vendor,
							attempts,
							count: req.count,
							maxTokens: req.maxTokens,
							domains: req.domains,
							freshness: req.freshness,
							resultCount: res.results.length,
							results: res.results,
							warnings: res.warnings,
							truncated: out.truncated,
						} satisfies WebSearchDetails,
					};
				} catch (error) {
					if (signal?.aborted) throw error;
					const cls = classify(vendor, error);
					const status = error instanceof VendorHttpError ? error.status : undefined;
					const raw =
						error instanceof VendorHttpError
							? error.detail
							: error instanceof Error
								? `${error.name}: ${error.message}`
								: String(error);
					const detail = redact(raw).slice(0, 120);
					attempts.push({ vendor, class: cls, status, ms: Date.now() - started, detail });
					writeMarker(vendor, cls, `${status ?? ""} ${detail}`.trim());
				}
			}

			// Return isError instead of throwing so `details` (attempts) still reach the UI and session log.
			const state = readState();
			const summary = attempts.map((a) => `${formatAttempt(a, state)} ${a.detail ?? ""}`.trim()).join("; ");
			return {
				content: [{ type: "text", text: `web_search: every vendor failed: ${summary}` }],
				details: {
					query,
					attempts,
					count: req.count,
					maxTokens: req.maxTokens,
					domains: req.domains,
					freshness: req.freshness,
					resultCount: 0,
					results: [],
					warnings: [],
					truncated: false,
				} satisfies WebSearchDetails,
				isError: true,
			};
		},

		renderCall(args, theme, _context) {
			let text = theme.fg("toolTitle", theme.bold("web_search "));
			text += theme.fg("accent", `"${args.query ?? ""}"`);
			if (args.domains?.length) text += theme.fg("dim", ` domains=${args.domains.join(",")}`);
			if (args.freshness) text += theme.fg("dim", ` ${args.freshness}`);
			if (args.count) text += theme.fg("dim", ` count=${args.count}`);
			return new Text(text, 0, 0);
		},

		renderResult(result, { expanded, isPartial }, theme, _context) {
			if (isPartial) return new Text(theme.fg("warning", "Searching..."), 0, 0);
			const details = result.details as WebSearchDetails | undefined;
			if (!details) return new Text(theme.fg("dim", "No results"), 0, 0);
			if (!details.vendor) {
				const failed = details.attempts.map((a) => `${a.vendor}:${a.class}${a.status ? ` ${a.status}` : ""}`);
				let text = theme.fg("error", `All vendors failed (${failed.join(", ")})`);
				if (expanded)
					for (const a of details.attempts) text += `\n${theme.fg("dim", `${a.vendor}: ${a.detail ?? ""}`)}`;
				return new Text(text, 0, 0);
			}
			const n = details.resultCount;
			let text = theme.fg(n > 0 ? "success" : "dim", `${n} result${n === 1 ? "" : "s"}`);
			text += theme.fg("dim", ` via ${details.vendor}`);
			const skipped = details.attempts.filter((a) => a.class);
			if (skipped.length > 0)
				text += theme.fg("warning", ` (skipped ${skipped.map((a) => `${a.vendor}:${a.class}`).join(", ")})`);
			if (details.truncated) text += theme.fg("warning", " (truncated)");
			if (expanded) {
				for (const [i, item] of details.results.entries()) {
					text += `\n${theme.fg("accent", `${i + 1}. ${item.title}`)}`;
					text += `\n${theme.fg("dim", item.url)}`;
					for (const snippet of item.snippets.slice(0, 2)) text += `\n${snippet}`;
				}
			}
			return new Text(text, 0, 0);
		},
	});
}
