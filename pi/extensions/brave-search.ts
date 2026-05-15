import { StringEnum } from "@mariozechner/pi-ai";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { DEFAULT_MAX_BYTES, DEFAULT_MAX_LINES, formatSize, truncateHead } from "@mariozechner/pi-coding-agent";
import { Text } from "@mariozechner/pi-tui";
import { Type } from "typebox";

const BRAVE_API_BASE_URL = "https://api.search.brave.com/res/v1";
const BRAVE_LLM_CONTEXT_ENDPOINT = `${BRAVE_API_BASE_URL}/llm/context`;
const BRAVE_NEWS_ENDPOINT = `${BRAVE_API_BASE_URL}/news/search`;
const BRAVE_IMAGES_ENDPOINT = `${BRAVE_API_BASE_URL}/images/search`;
const BRAVE_VIDEOS_ENDPOINT = `${BRAVE_API_BASE_URL}/videos/search`;
const API_KEY_ENV_VAR = "BRAVE_SEARCH_API_KEY";
let cachedApiKey: string | undefined;

type BraveSearchDetails = {
	query: string;
	count: number;
	country: string;
	searchLang: string;
	maxTokens: number;
	maxUrls: number;
	maxSnippets?: number;
	maxTokensPerUrl?: number;
	maxSnippetsPerUrl?: number;
	contextThresholdMode?: string;
	goggles?: string;
	enableLocal?: boolean;
	locationHeaders: Record<string, string>;
	resultCount: number;
	results: BraveContextResult[];
	truncated: boolean;
};

type BraveMediaDetails = {
	kind: "news" | "images" | "videos";
	query: string;
	count: number;
	country: string;
	searchLang: string;
	resultCount: number;
	results: BraveMediaResult[];
	truncated: boolean;
};

type BraveGroundingItem = {
	url?: unknown;
	title?: unknown;
	name?: unknown;
	snippets?: unknown;
};

type BraveSource = {
	title?: unknown;
	hostname?: unknown;
	age?: unknown;
};

type BraveLlmContextResponse = {
	grounding?: {
		generic?: BraveGroundingItem[];
		poi?: BraveGroundingItem | null;
		map?: BraveGroundingItem[];
	};
	sources?: Record<string, BraveSource>;
};

type BraveContextResult = {
	kind: "generic" | "poi" | "map";
	title: string;
	url: string;
	hostname?: string;
	age?: string[];
	snippets: string[];
};

type BraveMediaResult = {
	title: string;
	url: string;
	description?: string;
	age?: string;
	source?: string;
	thumbnailUrl?: string;
	imageUrl?: string;
	duration?: string;
};

type BraveMediaApiResult = {
	title?: unknown;
	url?: unknown;
	description?: unknown;
	age?: unknown;
	source?: unknown;
	thumbnail?: { src?: unknown };
	properties?: {
		url?: unknown;
		duration?: unknown;
	};
	meta_url?: {
		hostname?: unknown;
	};
};

type BraveMediaResponse = {
	results?: BraveMediaApiResult[];
};

const BraveSearchParams = Type.Object({
	query: Type.String({ description: "Search query" }),
	count: Type.Optional(
		Type.Number({
			description: "Maximum search results/URLs to consider (1-50, default 10)",
			minimum: 1,
			maximum: 50,
		}),
	),
	country: Type.Optional(
		Type.String({
			description: "Two-letter country code for localized results (default us)",
		}),
	),
	searchLang: Type.Optional(
		Type.String({
			description: "Search language code (default en)",
		}),
	),
	maxTokens: Type.Optional(
		Type.Number({
			description: "Approximate maximum tokens of Brave context to request (1024-32768, default 8192)",
			minimum: 1024,
			maximum: 32768,
		}),
	),
	maxUrls: Type.Optional(
		Type.Number({
			description: "Maximum URLs in the response (1-50, default same as count)",
			minimum: 1,
			maximum: 50,
		}),
	),
	maxSnippets: Type.Optional(
		Type.Number({
			description: "Maximum snippets across all URLs (1-100, Brave default 50)",
			minimum: 1,
			maximum: 100,
		}),
	),
	maxTokensPerUrl: Type.Optional(
		Type.Number({
			description: "Maximum tokens per individual URL (512-8192, Brave default 4096)",
			minimum: 512,
			maximum: 8192,
		}),
	),
	maxSnippetsPerUrl: Type.Optional(
		Type.Number({
			description: "Maximum snippets per individual URL (1-100, Brave default 50)",
			minimum: 1,
			maximum: 100,
		}),
	),
	contextThresholdMode: Type.Optional(
		StringEnum(["strict", "balanced", "lenient", "disabled"] as const, {
			description: "Relevance threshold for including content (default balanced)",
		}),
	),
	goggles: Type.Optional(
		Type.String({
			description: "Optional Brave Goggle URL or inline definition for source ranking/filtering",
		}),
	),
	enableLocal: Type.Optional(
		Type.Boolean({
			description:
				"Force local recall for location-aware queries. Omit to let Brave auto-detect from location headers.",
		}),
	),
	latitude: Type.Optional(Type.Number({ description: "Optional location latitude for local recall" })),
	longitude: Type.Optional(Type.Number({ description: "Optional location longitude for local recall" })),
	city: Type.Optional(Type.String({ description: "Optional city for local recall" })),
	state: Type.Optional(Type.String({ description: "Optional state/region for local recall" })),
	locationCountry: Type.Optional(Type.String({ description: "Optional location country for local recall" })),
	postalCode: Type.Optional(Type.String({ description: "Optional postal code for local recall" })),
	freshness: Type.Optional(
		Type.String({
			description: "Optional freshness filter: pd, pw, pm, py, or YYYY-MM-DDtoYYYY-MM-DD",
		}),
	),
});

const BraveMediaParams = Type.Object({
	query: Type.String({ description: "Search query" }),
	count: Type.Optional(Type.Number({ description: "Number of results to return", minimum: 1, maximum: 50 })),
	country: Type.Optional(Type.String({ description: "Two-letter country code for localized results (default us)" })),
	searchLang: Type.Optional(Type.String({ description: "Search language code (default en)" })),
	freshness: Type.Optional(Type.String({ description: "Optional freshness filter: pd, pw, pm, py, or date range" })),
	safesearch: Type.Optional(
		StringEnum(["strict", "moderate", "off"] as const, {
			description: "Safe search level (default strict for images/videos; Brave default for news)",
		}),
	),
	spellcheck: Type.Optional(Type.Boolean({ description: "Enable Brave spellcheck (default true)" })),
	offset: Type.Optional(Type.Number({ description: "Pagination offset where supported", minimum: 0, maximum: 9 })),
});

function asString(value: unknown): string | undefined {
	return typeof value === "string" && value.trim().length > 0 ? value.trim() : undefined;
}

function toStringList(value: unknown): string[] | undefined {
	if (!Array.isArray(value)) return undefined;

	const values = value
		.map((item) => {
			if (typeof item === "string") return item.trim();
			if (item === undefined || item === null) return "";
			return JSON.stringify(item);
		})
		.filter((item) => item.length > 0);

	return values.length > 0 ? values : undefined;
}

async function loadApiKey(_signal: AbortSignal | undefined): Promise<string> {
	if (cachedApiKey) return cachedApiKey;

	const apiKey = asString(process.env[API_KEY_ENV_VAR]);
	if (!apiKey) throw new Error(`Missing Brave Search API key: set the ${API_KEY_ENV_VAR} environment variable.`);

	cachedApiKey = apiKey;
	return cachedApiKey;
}

function clampNumber(value: number | undefined, fallback: number, min: number, max: number): number {
	if (!Number.isFinite(value)) return fallback;
	return Math.max(min, Math.min(max, Math.trunc(value ?? fallback)));
}

function normalizeCode(value: string | undefined, fallback: string): string {
	const normalized = value?.trim();
	return normalized ? normalized : fallback;
}

function optionalNumber(value: number | undefined, min: number, max: number): number | undefined {
	return value === undefined ? undefined : clampNumber(value, value, min, max);
}

function addOptionalString(
	target: Record<string, string | number | boolean>,
	key: string,
	value: string | undefined,
): void {
	const normalized = value?.trim();
	if (normalized) target[key] = normalized;
}

function buildLocationHeaders(params: {
	latitude?: number;
	longitude?: number;
	city?: string;
	state?: string;
	locationCountry?: string;
	postalCode?: string;
}): Record<string, string> {
	const headers: Record<string, string> = {};
	if (Number.isFinite(params.latitude) && Number.isFinite(params.longitude)) {
		headers["X-Loc-Lat"] = String(params.latitude);
		headers["X-Loc-Long"] = String(params.longitude);
	}
	if (params.city?.trim()) headers["X-Loc-City"] = params.city.trim();
	if (params.state?.trim()) headers["X-Loc-State"] = params.state.trim();
	if (params.locationCountry?.trim()) headers["X-Loc-Country"] = params.locationCountry.trim();
	if (params.postalCode?.trim()) headers["X-Loc-Postal-Code"] = params.postalCode.trim();
	return headers;
}

function parseGroundingItem(
	kind: BraveContextResult["kind"],
	item: BraveGroundingItem,
	sources: Record<string, BraveSource>,
): BraveContextResult | undefined {
	const url = asString(item.url);
	if (!url) return undefined;

	const source = sources[url];
	const title = asString(item.title) ?? asString(item.name) ?? asString(source?.title) ?? url;
	const snippets = toStringList(item.snippets) ?? [];

	return {
		kind,
		title,
		url,
		hostname: asString(source?.hostname),
		age: toStringList(source?.age),
		snippets,
	};
}

function parseResults(payload: BraveLlmContextResponse): BraveContextResult[] {
	const grounding = payload.grounding ?? {};
	const sources = payload.sources ?? {};
	const results: BraveContextResult[] = [];

	for (const item of grounding.generic ?? []) {
		const result = parseGroundingItem("generic", item, sources);
		if (result) results.push(result);
	}

	if (grounding.poi) {
		const result = parseGroundingItem("poi", grounding.poi, sources);
		if (result) results.push(result);
	}

	for (const item of grounding.map ?? []) {
		const result = parseGroundingItem("map", item, sources);
		if (result) results.push(result);
	}

	return results;
}

function parseMediaResults(payload: BraveMediaResponse): BraveMediaResult[] {
	return (payload.results ?? [])
		.map((result) => {
			const title = asString(result.title);
			const url = asString(result.url);
			if (!title || !url) return undefined;
			return {
				title,
				url,
				description: asString(result.description),
				age: asString(result.age),
				source: asString(result.source) ?? asString(result.meta_url?.hostname),
				thumbnailUrl: asString(result.thumbnail?.src),
				imageUrl: asString(result.properties?.url),
				duration: asString(result.properties?.duration),
			};
		})
		.filter((result): result is BraveMediaResult => result !== undefined);
}

function formatResult(result: BraveContextResult, index: number): string {
	const label = result.kind === "generic" ? "source" : result.kind;
	const lines = [`${index + 1}. ${result.title} (${label})`, `URL: ${result.url}`];
	if (result.hostname) lines.push(`Host: ${result.hostname}`);
	if (result.age?.length) lines.push(`Age: ${result.age.join(" | ")}`);
	if (result.snippets.length > 0) {
		lines.push("Snippets:");
		for (const snippet of result.snippets) {
			lines.push(`- ${snippet}`);
		}
	}
	return lines.join("\n");
}

function formatMediaResult(result: BraveMediaResult, index: number): string {
	const lines = [`${index + 1}. ${result.title}`, `URL: ${result.url}`];
	if (result.source) lines.push(`Source: ${result.source}`);
	if (result.age) lines.push(`Age: ${result.age}`);
	if (result.duration) lines.push(`Duration: ${result.duration}`);
	if (result.description) lines.push(`Description: ${result.description}`);
	if (result.thumbnailUrl) lines.push(`Thumbnail: ${result.thumbnailUrl}`);
	if (result.imageUrl) lines.push(`Image: ${result.imageUrl}`);
	return lines.join("\n");
}

function truncateText(text: string): { text: string; truncated: boolean } {
	const truncation = truncateHead(text, {
		maxLines: DEFAULT_MAX_LINES,
		maxBytes: DEFAULT_MAX_BYTES,
	});

	let output = truncation.content;
	if (truncation.truncated) {
		output += `\n\n[Output truncated: showing ${truncation.outputLines} of ${truncation.totalLines} lines`;
		output += ` (${formatSize(truncation.outputBytes)} of ${formatSize(truncation.totalBytes)}).]`;
	}

	return { text: output, truncated: truncation.truncated };
}

async function fetchJson(endpoint: string, apiKey: string, signal: AbortSignal | undefined): Promise<unknown>;
async function fetchJson(
	endpoint: string,
	apiKey: string,
	signal: AbortSignal | undefined,
	body: Record<string, string | number | boolean>,
	locationHeaders?: Record<string, string>,
): Promise<unknown>;
async function fetchJson(
	endpoint: string,
	apiKey: string,
	signal: AbortSignal | undefined,
	body?: Record<string, string | number | boolean>,
	locationHeaders: Record<string, string> = {},
): Promise<unknown> {
	const response = await fetch(endpoint, {
		method: body ? "POST" : "GET",
		headers: {
			Accept: "application/json",
			"Accept-Encoding": "gzip",
			...(body ? { "Content-Type": "application/json" } : {}),
			"X-Subscription-Token": apiKey,
			...locationHeaders,
		},
		body: body ? JSON.stringify(body) : undefined,
		signal,
	});

	if (!response.ok) {
		const body = await response.text().catch(() => "");
		const suffix = body ? `: ${body.slice(0, 500)}` : "";
		throw new Error(`Brave request failed (${response.status} ${response.statusText})${suffix}`);
	}

	return response.json();
}

function buildMediaUrl(endpoint: string, params: Record<string, string | number | boolean>): string {
	const url = new URL(endpoint);
	for (const [key, value] of Object.entries(params)) {
		url.searchParams.set(key, String(value));
	}
	return url.toString();
}

function registerMediaTool(
	pi: ExtensionAPI,
	options: {
		kind: BraveMediaDetails["kind"];
		name: string;
		label: string;
		description: string;
		endpoint: string;
		defaultCount: number;
		maxCount: number;
	},
) {
	pi.registerTool({
		name: options.name,
		label: options.label,
		description: `${options.description}. Requires the ${API_KEY_ENV_VAR} environment variable. Output is truncated to ${DEFAULT_MAX_LINES} lines or ${formatSize(DEFAULT_MAX_BYTES)}.`,
		promptSnippet: options.description,
		promptGuidelines: [`Use ${options.name} when the user specifically asks for Brave ${options.kind} search results.`],
		parameters: BraveMediaParams,

		async execute(_toolCallId, params, signal, _onUpdate, _ctx) {
			const apiKey = await loadApiKey(signal);
			const query = params.query.trim();
			if (!query) throw new Error("query must not be empty");

			const count = clampNumber(params.count, options.defaultCount, 1, options.maxCount);
			const country = normalizeCode(params.country, "us");
			const searchLang = normalizeCode(params.searchLang, "en");
			const requestParams: Record<string, string | number | boolean> = {
				q: query,
				count,
				country,
				search_lang: searchLang,
			};
			if (params.freshness?.trim()) requestParams.freshness = params.freshness.trim();
			if (params.safesearch) requestParams.safesearch = params.safesearch;
			if (params.spellcheck !== undefined) requestParams.spellcheck = params.spellcheck;
			if (params.offset !== undefined) requestParams.offset = clampNumber(params.offset, 0, 0, 9);

			const payload = (await fetchJson(
				buildMediaUrl(options.endpoint, requestParams),
				apiKey,
				signal,
			)) as BraveMediaResponse;
			const results = parseMediaResults(payload);
			const resultText =
				results.length > 0
					? `Brave ${options.kind} search for: ${query}\n\n${results.map(formatMediaResult).join("\n\n")}`
					: `No Brave ${options.kind} results found for: ${query}`;
			const truncated = truncateText(resultText);

			return {
				content: [{ type: "text", text: truncated.text }],
				details: {
					kind: options.kind,
					query,
					count,
					country,
					searchLang,
					resultCount: results.length,
					results,
					truncated: truncated.truncated,
				} satisfies BraveMediaDetails,
			};
		},

		renderCall(args, theme, _context) {
			let text = theme.fg("toolTitle", theme.bold(`${options.name} `));
			text += theme.fg("accent", `"${args.query ?? ""}"`);
			if (args.count) text += theme.fg("dim", ` count=${args.count}`);
			return new Text(text, 0, 0);
		},

		renderResult(result, { expanded, isPartial }, theme, _context) {
			if (isPartial) return new Text(theme.fg("warning", `Searching Brave ${options.kind}...`), 0, 0);

			const details = result.details as BraveMediaDetails | undefined;
			if (!details || details.resultCount === 0) return new Text(theme.fg("dim", "No results"), 0, 0);

			let text = theme.fg(
				"success",
				`${details.resultCount} Brave ${options.kind} result${details.resultCount === 1 ? "" : "s"}`,
			);
			if (details.truncated) text += theme.fg("warning", " (truncated)");

			if (expanded) {
				for (const [index, item] of details.results.entries()) {
					text += `\n${theme.fg("accent", `${index + 1}. ${item.title}`)}`;
					text += `\n${theme.fg("dim", item.url)}`;
					if (item.description) text += `\n${item.description}`;
				}
			}

			return new Text(text, 0, 0);
		},
	});
}

export default function (pi: ExtensionAPI) {
	pi.registerTool({
		name: "brave_search",
		label: "Brave Search",
		description: `Search the web with Brave LLM Context. Requires the ${API_KEY_ENV_VAR} environment variable. Output is truncated to ${DEFAULT_MAX_LINES} lines or ${formatSize(DEFAULT_MAX_BYTES)}.`,
		promptSnippet:
			"Search the web with Brave LLM Context for current information, extracted page content, documentation, references, or examples",
		promptGuidelines: [
			"Use brave_search when the user asks for current web information, external documentation, references, examples, or research that is not available in the local workspace.",
			"Use brave_search contextThresholdMode='strict' when precision matters more than recall; use 'lenient' or 'disabled' for broad discovery.",
			"Use brave_search goggles to restrict or boost trusted sources when the user asks for source-constrained research.",
			"Use brave_news_search, brave_image_search, or brave_video_search when the user specifically asks for news, images, or videos.",
			"Cite URLs from Brave search results when using web information in the answer.",
		],
		parameters: BraveSearchParams,

		async execute(_toolCallId, params, signal, _onUpdate, _ctx) {
			const apiKey = await loadApiKey(signal);
			const query = params.query.trim();
			if (!query) throw new Error("query must not be empty");

			const count = clampNumber(params.count, 10, 1, 50);
			const country = normalizeCode(params.country, "us");
			const searchLang = normalizeCode(params.searchLang, "en");
			const maxTokens = clampNumber(params.maxTokens, 8192, 1024, 32768);
			const maxUrls = clampNumber(params.maxUrls, count, 1, 50);
			const maxSnippets = optionalNumber(params.maxSnippets, 1, 100);
			const maxTokensPerUrl = optionalNumber(params.maxTokensPerUrl, 512, 8192);
			const maxSnippetsPerUrl = optionalNumber(params.maxSnippetsPerUrl, 1, 100);
			const locationHeaders = buildLocationHeaders(params);
			const requestBody: Record<string, string | number | boolean> = {
				q: query,
				count,
				country,
				search_lang: searchLang,
				maximum_number_of_urls: maxUrls,
				maximum_number_of_tokens: maxTokens,
			};
			if (maxSnippets !== undefined) requestBody.maximum_number_of_snippets = maxSnippets;
			if (maxTokensPerUrl !== undefined) requestBody.maximum_number_of_tokens_per_url = maxTokensPerUrl;
			if (maxSnippetsPerUrl !== undefined) requestBody.maximum_number_of_snippets_per_url = maxSnippetsPerUrl;
			if (params.contextThresholdMode) requestBody.context_threshold_mode = params.contextThresholdMode;
			if (params.enableLocal !== undefined) requestBody.enable_local = params.enableLocal;
			addOptionalString(requestBody, "freshness", params.freshness);
			addOptionalString(requestBody, "goggles", params.goggles);

			const payload = (await fetchJson(
				BRAVE_LLM_CONTEXT_ENDPOINT,
				apiKey,
				signal,
				requestBody,
				locationHeaders,
			)) as BraveLlmContextResponse;
			const results = parseResults(payload);
			const resultText =
				results.length > 0
					? `Brave LLM context for: ${query}\n\n${results.map(formatResult).join("\n\n")}`
					: `No Brave LLM context found for: ${query}`;
			const truncated = truncateText(resultText);

			return {
				content: [{ type: "text", text: truncated.text }],
				details: {
					query,
					count,
					country,
					searchLang,
					maxTokens,
					maxUrls,
					maxSnippets,
					maxTokensPerUrl,
					maxSnippetsPerUrl,
					contextThresholdMode: params.contextThresholdMode,
					goggles: params.goggles?.trim() || undefined,
					enableLocal: params.enableLocal,
					locationHeaders,
					resultCount: results.length,
					results,
					truncated: truncated.truncated,
				} satisfies BraveSearchDetails,
			};
		},

		renderCall(args, theme, _context) {
			let text = theme.fg("toolTitle", theme.bold("brave_search "));
			text += theme.fg("accent", `"${args.query ?? ""}"`);
			if (args.count) text += theme.fg("dim", ` count=${args.count}`);
			if (args.maxTokens) text += theme.fg("dim", ` maxTokens=${args.maxTokens}`);
			if (args.contextThresholdMode) text += theme.fg("dim", ` ${args.contextThresholdMode}`);
			return new Text(text, 0, 0);
		},

		renderResult(result, { expanded, isPartial }, theme, _context) {
			if (isPartial) return new Text(theme.fg("warning", "Searching Brave LLM Context..."), 0, 0);

			const details = result.details as BraveSearchDetails | undefined;
			if (!details || details.resultCount === 0) return new Text(theme.fg("dim", "No results"), 0, 0);

			let text = theme.fg(
				"success",
				`${details.resultCount} Brave context source${details.resultCount === 1 ? "" : "s"}`,
			);
			if (details.truncated) text += theme.fg("warning", " (truncated)");

			if (expanded) {
				for (const [index, item] of details.results.entries()) {
					text += `\n${theme.fg("accent", `${index + 1}. ${item.title}`)}`;
					text += `\n${theme.fg("dim", item.url)}`;
					for (const snippet of item.snippets.slice(0, 2)) {
						text += `\n${snippet}`;
					}
				}
			}

			return new Text(text, 0, 0);
		},
	});

	registerMediaTool(pi, {
		kind: "news",
		name: "brave_news_search",
		label: "Brave News Search",
		description: "Search Brave News for recent articles, news events, and trending topics",
		endpoint: BRAVE_NEWS_ENDPOINT,
		defaultCount: 10,
		maxCount: 50,
	});
	registerMediaTool(pi, {
		kind: "images",
		name: "brave_image_search",
		label: "Brave Image Search",
		description: "Search Brave Images for image results, thumbnails, and source pages",
		endpoint: BRAVE_IMAGES_ENDPOINT,
		defaultCount: 10,
		maxCount: 50,
	});
	registerMediaTool(pi, {
		kind: "videos",
		name: "brave_video_search",
		label: "Brave Video Search",
		description: "Search Brave Videos for video results, tutorials, clips, and source pages",
		endpoint: BRAVE_VIDEOS_ENDPOINT,
		defaultCount: 10,
		maxCount: 50,
	});
}
