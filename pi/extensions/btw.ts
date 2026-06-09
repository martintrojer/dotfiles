/**
 * `/btw` — ask quick side questions without polluting the main conversation.
 *
 * A clone of Claude Code's `/btw` ("by the way"). It opens an interactive,
 * multi-turn, no-tools side thread in a modal UI that sees the current session
 * context: ask a follow-up, get another answer, repeat. Nothing is written to
 * the main session history unless you explicitly paste the last answer into the
 * main editor. The inverse of a subagent: full context, no tools.
 *
 * Use it for quick clarifications about what the agent already knows from this
 * session ("what does this function return?", "why did you pick that lib?")
 * without derailing a long-running task or burning main-thread context.
 *
 * Usage:
 *   /btw what does calculate_metrics return here?
 *   /btw    # open an empty side-thread and type the first question in-modal
 *
 * Answers stream in token-by-token; an animated "Answering…" indicator shows
 * while a turn is in flight (cancellable with Esc).
 *
 * Key handling:
 *   - Enter with text   → submit a follow-up question
 *   - Enter when empty   → paste the LAST answer into the main editor and close
 *   - Esc                → close without pasting (cancels any in-flight turn)
 *
 * Notes:
 *   - Read-only: the side agent has no tools.
 *   - Ephemeral: nothing is written to the main session history; only an
 *     explicit empty-Enter paste drops the last answer into the main editor.
 *   - The side thread keeps its own follow-up history for continuity, with the
 *     main session supplied as structured messages and project context supplied
 *     via the system prompt.
 *   - Uses the current session model (full context awareness), falling back to
 *     any model with configured auth.
 *   - Runs independently of the main task: its abort signal is its own, so
 *     stopping the main turn won't cancel /btw and vice versa.
 */

import { stream } from "@earendil-works/pi-ai";
import type { Message } from "@earendil-works/pi-ai";
import type { BuildSystemPromptOptions, ExtensionAPI, ExtensionCommandContext } from "@earendil-works/pi-coding-agent";
import { buildSessionContext, getMarkdownTheme } from "@earendil-works/pi-coding-agent";
import type { Theme } from "@earendil-works/pi-coding-agent";
import { Box, Input, Markdown, Text, truncateToWidth } from "@earendil-works/pi-tui";
import type { Focusable, TUI } from "@earendil-works/pi-tui";
import { conversationTranscript, pickSessionModel, textContent } from "./_lib.ts";

// Fallback flattened transcript budget if structured session-context building fails.
const CONTEXT_BUDGET = 24000;
// Separate budget for project context (cwd, system prompt, context files).
const PROJECT_BUDGET = 16000;

function buildContext(ctx: ExtensionCommandContext): string {
	return conversationTranscript(ctx.sessionManager.getBranch() as never[], CONTEXT_BUDGET);
}

/**
 * Project/session context the main agent sees but that isn't in the
 * conversation transcript: cwd, custom/appended system prompt, and loaded
 * context files (AGENTS.md / CLAUDE.md, etc.). This makes /btw aware of
 * project guidelines the way Claude Code's /btw is.
 */
function buildProjectContext(ctx: ExtensionCommandContext): string {
	let opts: BuildSystemPromptOptions | undefined;
	try {
		opts = ctx.getSystemPromptOptions();
	} catch {
		return "";
	}
	if (!opts) return "";

	const sections: string[] = [];
	if (opts.cwd) sections.push(`Working directory: ${opts.cwd}`);
	if (opts.customPrompt) sections.push(`Custom system prompt:\n${opts.customPrompt}`);
	if (opts.appendSystemPrompt) sections.push(`Appended system prompt:\n${opts.appendSystemPrompt}`);
	for (const file of opts.contextFiles ?? []) {
		if (file?.content?.trim()) sections.push(`--- ${file.path} ---\n${file.content.trim()}`);
	}

	const joined = sections.join("\n\n");
	return joined.length > PROJECT_BUDGET ? joined.slice(0, PROJECT_BUDGET) : joined;
}

/** System prompt carrying project context the side thread reasons over. */
function buildSystemPrompt(ctx: ExtensionCommandContext): string {
	const projectContext = buildProjectContext(ctx);
	return [
		"You are answering quick side questions about an ongoing coding session.",
		"You have NO tools. Answer from the provided main-session context and your own knowledge.",
		"Be concise and direct. The user may ask follow-up questions.",
		...(projectContext ? ["", "<project_context>", projectContext, "</project_context>"] : []),
	].join("\n");
}

/** Structured main-session messages, preserving roles/tool-results better than a flattened transcript. */
function buildMainMessages(ctx: ExtensionCommandContext): Message[] {
	try {
		return buildSessionContext(ctx.sessionManager.getEntries(), ctx.sessionManager.getLeafId()).messages as Message[];
	} catch {
		// Fallback to flattened text if session-context construction fails.
		return [
			{
				role: "user",
				content: [{ type: "text", text: `<conversation_context>\n${buildContext(ctx)}\n</conversation_context>` }],
				timestamp: Date.now(),
			},
		];
	}
}

type SideMessage = { role: "user" | "assistant"; text: string };

/**
 * Run one side-thread turn. `history` is the prior follow-up exchange (not the
 * main conversation — that lives in the system prompt); `question` is the new
 * user turn. Streams the answer via `onDelta` and returns the final text.
 *
 * Uses the caller's own signal, NOT ctx.signal: during a turn ctx.signal is the
 * MAIN agent's abort signal, so reusing it would make aborting (Esc) the main
 * task also kill the side thread. /btw runs independently.
 */
async function askSideQuestion(
	ctx: ExtensionCommandContext,
	systemPrompt: string,
	mainMessages: Message[],
	history: SideMessage[],
	question: string,
	signal: AbortSignal,
	onDelta: (text: string) => void,
): Promise<{ answer: string } | { error: string }> {
	const picked = await pickSessionModel(ctx);
	if (!picked) return { error: "No model with configured auth is available." };

	const sideMessages = [...history, { role: "user" as const, text: question }].map((m) => ({
		role: m.role,
		content: [{ type: "text" as const, text: m.text }],
		timestamp: Date.now(),
	}));

	const events = stream(
		picked.model,
		{ systemPrompt, messages: [...mainMessages, ...sideMessages] },
		{ apiKey: picked.apiKey, headers: picked.headers, signal },
	);

	let streamed = "";
	for await (const event of events) {
		if (event.type === "text_delta") {
			streamed += event.delta;
			onDelta(streamed);
		}
	}

	const response = await events.result();
	if (response.stopReason === "aborted") return { error: "Cancelled." };

	const answer = textContent(response.content) || streamed;

	return { answer: answer || "(no response)" };
}

const SPINNER = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];

interface Turn {
	question: string;
	answer: string;
	isError: boolean;
}

/** Result of the side-thread modal: paste the last answer, or just close. */
type SideResult = { paste: string } | { closed: true };

/**
 * Interactive multi-turn side-thread modal.
 *
 * Framed with horizontal rules: a title bar, the running Q/A transcript (with
 * one blank line between turns), a rule, then the follow-up input + hint. Shows
 * an animated "Answering…" indicator while a turn is in flight. Key handling:
 *   - Enter with text   → submit a follow-up
 *   - Enter when empty   → paste the LAST answer into the main editor and close
 *   - Esc                → close without pasting (cancels any in-flight turn)
 *
 * Owns its own AbortController so cancelling never touches the main task.
 */
class SideThread implements Focusable {
	// Focus is delegated to the inner Input. pi calls setFocus() on THIS
	// component (the one returned from custom()), so we must own focus and
	// forward both the focus flag (for the cursor) and keystrokes to the Input.
	private _focused = false;
	get focused(): boolean {
		return this._focused;
	}
	set focused(value: boolean) {
		this._focused = value;
		this.input.focused = value;
	}

	private readonly input = new Input();
	private readonly turns: Turn[] = [];
	private readonly history: SideMessage[] = [];
	private answering = false;
	private streaming = "";
	private streamingQuestion = "";
	private closed = false;
	private spinnerFrame = 0;
	private spinnerTimer: ReturnType<typeof setInterval> | undefined;
	private controller: AbortController | undefined;

	constructor(
		private readonly tui: TUI,
		private readonly theme: Theme,
		private readonly mdTheme: ReturnType<typeof getMarkdownTheme>,
		private readonly ask: (
			history: SideMessage[],
			question: string,
			signal: AbortSignal,
			onDelta: (text: string) => void,
		) => Promise<{ answer: string } | { error: string }>,
		initialQuestion: string | undefined,
		private readonly done: (result: SideResult) => void,
	) {
		this.input.onSubmit = (value) => this.onSubmit(value);
		this.input.onEscape = () => this.close();
		if (initialQuestion) void this.submitQuestion(initialQuestion);
	}

	handleInput(data: string): void {
		// Forward all keys to the Input, which owns editing + submit/escape. Submit
		// is gated by onSubmit's `answering` guard, so typing during a turn just
		// buffers in the input.
		this.input.handleInput(data);
	}

	invalidate(): void {
		this.input.invalidate();
	}

	dispose(): void {
		this.cleanup();
	}

	private onSubmit(value: string): void {
		if (this.answering) return; // ignore submits while a turn is running
		const text = value.trim();
		if (text === "") {
			// Empty Enter → paste the last good answer into the main editor.
			const last = this.turns[this.turns.length - 1];
			if (last && !last.isError) this.finish({ paste: last.answer });
			else this.finish({ closed: true });
			return;
		}
		this.input.setValue("");
		void this.submitQuestion(text);
	}

	private async submitQuestion(question: string): Promise<void> {
		this.answering = true;
		this.streaming = "";
		this.streamingQuestion = question;
		this.controller = new AbortController();
		this.startSpinner();
		this.tui.requestRender();

		let result: { answer: string } | { error: string };
		try {
			result = await this.ask([...this.history], question, this.controller.signal, (text) => {
				if (this.closed) return;
				this.streaming = text;
				this.tui.requestRender();
			});
		} catch (err) {
			result = { error: err instanceof Error ? err.message : String(err) };
		}

		if (this.closed) return; // modal was dismissed mid-answer; drop the result
		this.answering = false;
		this.streaming = "";
		this.streamingQuestion = "";
		this.stopSpinner();
		const isError = "error" in result;
		const answer = isError ? (result as { error: string }).error : (result as { answer: string }).answer;
		this.turns.push({ question, answer, isError });
		if (!isError) {
			this.history.push({ role: "user", text: question });
			this.history.push({ role: "assistant", text: answer });
		}
		this.tui.requestRender(true);
	}

	private close(): void {
		this.finish({ closed: true });
	}

	/** Resolve the modal exactly once, tearing down timers/abort first. */
	private finish(result: SideResult): void {
		if (this.closed) return;
		this.closed = true;
		this.cleanup();
		this.done(result);
	}

	private cleanup(): void {
		this.controller?.abort();
		this.stopSpinner();
	}

	private startSpinner(): void {
		if (this.spinnerTimer) return;
		this.spinnerTimer = setInterval(() => {
			this.spinnerFrame = (this.spinnerFrame + 1) % SPINNER.length;
			this.tui.requestRender();
		}, 100);
		if (typeof this.spinnerTimer.unref === "function") this.spinnerTimer.unref();
	}

	private stopSpinner(): void {
		if (this.spinnerTimer) {
			clearInterval(this.spinnerTimer);
			this.spinnerTimer = undefined;
		}
	}

	private renderQuestion(turn: Turn, width: number): string[] {
		const qBox = new Box(1, 0, (s) => this.theme.bg("userMessageBg", s));
		qBox.addChild(new Text(this.theme.fg("accent", "› ") + this.theme.fg("userMessageText", turn.question), 0, 0));
		return qBox.render(width);
	}

	private renderAnswer(turn: Turn, width: number): string[] {
		const aBox = new Box(1, 0, (s) => this.theme.bg("customMessageBg", s));
		if (turn.isError) aBox.addChild(new Text(this.theme.fg("error", turn.answer), 0, 0));
		else
			aBox.addChild(
				new Markdown(turn.answer, 0, 0, this.mdTheme, {
					color: (s) => this.theme.fg("customMessageText", s),
				}),
			);
		return aBox.render(width);
	}

	render(width: number): string[] {
		const t = this.theme;
		const lines: string[] = [];
		const add = (s: string) => lines.push(truncateToWidth(s, width));
		const rule = () => t.fg("borderMuted", "─".repeat(Math.max(1, width)));
		const count = this.turns.length;

		add(rule());
		add(
			t.fg("accent", t.bold("  /btw")) +
				t.fg("dim", " side thread") +
				(count > 1 ? t.fg("dim", `  · ${count} turns`) : ""),
		);
		lines.push("");

		if (this.turns.length === 0 && !this.answering) {
			add(t.fg("dim", "  Ask a side question below."));
		} else {
			this.turns.forEach((turn, i) => {
				if (i > 0) lines.push(""); // exactly one empty line between turns
				lines.push(...this.renderQuestion(turn, width));
				lines.push(...this.renderAnswer(turn, width));
			});
		}

		if (this.answering) {
			if (this.turns.length > 0) lines.push("");
			// Render the in-flight question, then either the streamed-so-far answer
			// (token by token) or, before the first delta, the spinner.
			if (this.streamingQuestion) {
				lines.push(...this.renderQuestion({ question: this.streamingQuestion, answer: "", isError: false }, width));
			}
			if (this.streaming) {
				lines.push(...this.renderAnswer({ question: "", answer: this.streaming, isError: false }, width));
			}
			const frame = SPINNER[this.spinnerFrame];
			add(t.fg("accent", `  ${frame} `) + t.fg("dim", "Answering…"));
		}

		add(rule());
		lines.push(...this.input.render(width));
		const hint = this.answering
			? "  Esc to cancel"
			: "  Enter: follow-up · empty Enter: paste last answer to input · Esc: close";
		add(t.fg("dim", hint));
		add(rule());
		return lines;
	}
}

export default function (pi: ExtensionAPI) {
	pi.registerCommand("btw", {
		description: "Ask a quick side question (multi-turn) without polluting the main conversation",
		handler: async (args, ctx) => {
			const question = (args ?? "").trim();

			const ask = (
				history: SideMessage[],
				q: string,
				signal: AbortSignal,
				onDelta: (text: string) => void = () => {},
			) => askSideQuestion(ctx, buildSystemPrompt(ctx), buildMainMessages(ctx), history, q, signal, onDelta);

			// Non-TUI (print/json/rpc): single-shot, answer via notification.
			if (ctx.mode !== "tui") {
				if (!question) {
					ctx.ui.notify("Usage: /btw <question>", "info");
					return;
				}
				const controller = new AbortController();
				const result = await ask([], question, controller.signal).catch((err) => ({
					error: err instanceof Error ? err.message : String(err),
				}));
				const body = "error" in result ? result.error : result.answer;
				ctx.ui.notify(`/btw ${question}\n\n${body}`, "error" in result ? "warning" : "info");
				return;
			}

			// TUI: interactive multi-turn side thread. Modal replacement UI (captures
			// input), so only one /btw is open at a time; the main agent task keeps
			// running in the background independent of this modal.
			const mdTheme = getMarkdownTheme();
			const result = await ctx.ui.custom<SideResult>((tui, theme, _kb, done) => {
				return new SideThread(tui, theme, mdTheme, ask, question, done);
			});

			if ("paste" in result) {
				// Drop the last answer into the main editor for the user to send/edit.
				ctx.ui.setEditorText(result.paste);
			}
		},
	});
}
