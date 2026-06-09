/**
 * `/answer` — extract the questions from the last assistant message and answer
 * them in an interactive modal, then send the compiled answers back.
 *
 * A clean reimplementation: a fast model extracts questions as JSON, then a
 * modal (same look/feel as /btw — framing rules, title bar, themed colors)
 * walks you through them one at a time with a multi-line editor. On submit it
 * sends a single user message with all the Q/A pairs.
 *
 * Navigation:
 *   - Enter        → next question (or submit on the last)
 *   - Shift+Enter   → newline in the answer
 *   - Tab / ↑↓      → move between questions (↑↓ only when the answer is empty)
 *   - Esc          → cancel
 *
 * Invoke with `/answer` or Ctrl+. Extraction has no tools; submitting answers
 * sends one compiled message back to the main agent and triggers a turn.
 */

import { complete } from "@earendil-works/pi-ai";
import type { ExtensionAPI, ExtensionContext, Theme } from "@earendil-works/pi-coding-agent";
import { BorderedLoader, getMarkdownTheme } from "@earendil-works/pi-coding-agent";
import {
	Box,
	Editor,
	type EditorTheme,
	Key,
	Markdown,
	matchesKey,
	Text,
	type TUI,
	truncateToWidth,
} from "@earendil-works/pi-tui";
import { lastCompletedAssistantText, pickSessionModel, textContent } from "./_lib.ts";

interface Question {
	question: string;
	context?: string;
	/** The marker the assistant used for this question verbatim (e.g. "1", "a)", "iii."), if any. */
	label?: string;
}

const SYSTEM_PROMPT = `You extract questions a user needs to answer from assistant text.

Output ONLY a JSON object:
{"questions": [{"label": "original marker, if any", "question": "...", "context": "optional essential context"}]}

Rules:
- Include every question that requires user input, in order.
- If the assistant numbered or labeled its questions (1/2/3, a)/b)/c), bullets,
  "First.../Second...", etc.), copy that marker VERBATIM into "label" so answers
  can be matched back unambiguously. Use the assistant's own scheme — don't
  renumber, don't invent labels. Omit "label" only when there was no marker.
- Don't copy the assistant's wording verbatim: extract the real intent of each
  question and rephrase it as a clear, direct ask the user can answer quickly.
- Keep "question" scannable (ideally one line) but complete: never drop part of
  the ask to make it shorter. If the source poses an either/or or offers a
  concrete alternative, keep both options in the question ("X or Y?") so the
  user can see the choices on the table.
- Lead "context" with the crux: a single bullet naming the actual tension,
  risk, or failure mode being decided — phrased as the stakes ("Bug risk: ...",
  "Tradeoff: ...", "Risk: ..."), not a neutral setup fact. Every later bullet is
  background that supports that crux. If the crux is currently spread across
  several facts, synthesize it into one leading bullet.
- Preserve the stakes. These are often critiques: keep the argument for why the
  current choice might be wrong (the consequence, failure mode, or tradeoff the
  user must defend or rebut), not just neutral facts about the code.
- Surface the specifics the user needs to decide — names, paths, values,
  options, defaults — in "context", but only those that bear on the answer.
- Format "context" so a human can scan it fast: short lines or markdown bullets
  (one fact each). Break long explanations into short lines, not one dense block.
- Backtick EVERY code token wherever it appears — in both "question" and
  "context": function calls, file paths, config keys, flags, commands, plugin
  names, identifiers (e.g. \`get_palette()\`, \`nvim-pack-lock.json\`,
  \`PackChanged\`, \`pattern = \\"*\\"\`). Don't leave any bare.
- Split a question only when the parts need genuinely different answers; keep a
  tightly-coupled follow-up ("...and should it be made explicit?") in the same
  question rather than fragmenting one decision.
- Add "context" only when it genuinely helps answer the question.
- If there are no questions, return {"questions": []}.

Example of the intended style (label copied verbatim, crux first, backticked identifiers, options visible):
{"questions": [{
  "label": "3.",
  "question": "Is the captured-once palette in \`mini_setup.lua\` a latent bug, or a deliberate catppuccin-only choice that should be made explicit?",
  "context": "- Latent bug risk: the \`pattern = \\"*\\"\` ColorScheme autocmd re-stamps catppuccin colors over any non-catppuccin theme.\\n- Cause: \`get_palette()\` is captured once at module load, not re-read per event.\\n- ~40 highlight overrides re-applied on every ColorScheme via \`apply_theme_overrides\`."
}]}`;

/** Parse the extractor's JSON (tolerating ```json fences and surrounding prose). */
function parseQuestions(text: string): Question[] | null {
	const fenced = /```(?:json)?\s*([\s\S]*?)```/.exec(text);
	const candidate = fenced ? fenced[1] : (/\{[\s\S]*\}/.exec(text)?.[0] ?? text);
	try {
		const parsed = JSON.parse(candidate.trim()) as { questions?: unknown };
		if (!Array.isArray(parsed.questions)) return null;
		return parsed.questions
			.filter((q): q is Question => !!q && typeof (q as Question).question === "string")
			.map((q) => ({
				question: q.question,
				context: typeof q.context === "string" ? q.context : undefined,
				label: typeof q.label === "string" && q.label.trim() ? q.label.trim() : undefined,
			}));
	} catch {
		return null;
	}
}

type ExtractOutcome =
	| { kind: "ok"; questions: Question[] }
	| { kind: "cancelled" }
	| { kind: "error"; message: string };

/** Run the extractor behind a cancellable loader overlay. */
async function extractQuestions(ctx: ExtensionContext, source: string): Promise<ExtractOutcome> {
	const picked = await pickSessionModel(ctx);
	if (!picked) return { kind: "error", message: "No model with configured auth is available." };

	return ctx.ui.custom<ExtractOutcome>((tui, theme, _kb, done) => {
		const loader = new BorderedLoader(tui, theme, `Extracting questions using ${picked.model.id}…`, {
			cancellable: true,
		});
		let settled = false;
		const finish = (outcome: ExtractOutcome): void => {
			if (settled) return;
			settled = true;
			done(outcome);
		};
		loader.onAbort = () => finish({ kind: "cancelled" });

		void (async () => {
			try {
				const response = await complete(
					picked.model,
					{
						systemPrompt: SYSTEM_PROMPT,
						messages: [{ role: "user", content: [{ type: "text", text: source }], timestamp: Date.now() }],
					},
					{ apiKey: picked.apiKey, headers: picked.headers, signal: loader.signal },
				);
				if (response.stopReason === "aborted") return finish({ kind: "cancelled" });
				const text = textContent(response.content);
				const questions = parseQuestions(text);
				if (!questions) return finish({ kind: "error", message: "Extractor returned invalid JSON." });
				finish({ kind: "ok", questions });
			} catch (err) {
				finish({ kind: "error", message: err instanceof Error ? err.message : String(err) });
			}
		})();

		return loader;
	});
}

type AnswerResult = { answers: string } | { cancelled: true };

/**
 * Interactive Q&A modal. One question at a time with a multi-line editor;
 * a progress dot row shows answered/current/pending. Framed like /btw.
 *
 * Implements the focus contract: pi calls setFocus() on the returned component,
 * so we forward focus + keystrokes to the embedded Editor.
 */
function buildQnA(questions: Question[]) {
	const mdTheme = getMarkdownTheme();
	return (tui: TUI, theme: Theme, done: (r: AnswerResult) => void) => {
		const answers = questions.map(() => "");
		let index = 0;
		let confirming = false;
		let cachedLines: string[] | undefined;

		const editorTheme: EditorTheme = {
			borderColor: (s) => theme.fg("borderMuted", s),
			selectList: {
				selectedPrefix: (t) => theme.fg("accent", t),
				selectedText: (t) => theme.fg("accent", t),
				description: (t) => theme.fg("muted", t),
				scrollInfo: (t) => theme.fg("dim", t),
				noMatch: (t) => theme.fg("warning", t),
			},
		};
		const editor = new Editor(tui, editorTheme);
		editor.disableSubmit = true; // we own Enter, to preserve the text
		editor.onChange = () => refresh();

		function refresh(): void {
			cachedLines = undefined;
			tui.requestRender();
		}
		function save(): void {
			answers[index] = editor.getText();
		}
		function goTo(i: number): void {
			if (i < 0 || i >= questions.length) return;
			save();
			index = i;
			editor.setText(answers[index] ?? "");
			refresh();
		}
		function submit(): void {
			save();
			// Paste back only Q/A pairs. The context bullets were a scanning aid for
			// the human while answering; the main agent already has that context
			// (it asked the questions), so echoing it back is redundant noise.
			const parts: string[] = [];
			questions.forEach((q, i) => {
				// Prefix with the assistant's own marker (verbatim) when present so it can
				// match answers back to its original numbering/lettering unambiguously.
				const q_label = q.label ? `Q ${q.label} ` : "Q: ";
				parts.push(`${q_label}${q.question}`, `A: ${answers[i].trim() || "(no answer)"}`, "");
			});
			done({ answers: parts.join("\n").trim() });
		}

		function handleInput(data: string): void {
			if (confirming) {
				if (matchesKey(data, Key.enter) || data.toLowerCase() === "y") return submit();
				if (matchesKey(data, Key.escape) || data.toLowerCase() === "n") {
					confirming = false;
					return refresh();
				}
				return;
			}
			if (matchesKey(data, Key.escape) || matchesKey(data, Key.ctrl("c"))) return done({ cancelled: true });
			if (matchesKey(data, Key.tab)) return void goTo(index + 1);
			if (matchesKey(data, Key.shift("tab"))) return void goTo(index - 1);
			if (matchesKey(data, Key.up) && editor.getText() === "") return void goTo(index - 1);
			if (matchesKey(data, Key.down) && editor.getText() === "") return void goTo(index + 1);
			// Plain Enter advances / confirms; Shift+Enter falls through to the editor.
			if (matchesKey(data, Key.enter) && !matchesKey(data, Key.shift("enter"))) {
				save();
				if (index < questions.length - 1) goTo(index + 1);
				else {
					confirming = true;
					refresh();
				}
				return;
			}
			editor.handleInput(data);
			refresh();
		}

		function render(width: number): string[] {
			if (cachedLines) return cachedLines;
			const t = theme;
			const add = (s: string, out: string[]) => out.push(truncateToWidth(s, width));
			const lines: string[] = [];

			add(t.fg("borderMuted", "─".repeat(width)), lines);
			add("  " + t.fg("accent", t.bold("/answer")) + t.fg("dim", `  ·  ${index + 1}/${questions.length}`), lines);

			// progress dots
			const dots = questions
				.map((_, i) => {
					if (i === index) return t.fg("accent", "●");
					return (answers[i]?.trim() ?? "") ? t.fg("success", "●") : t.fg("dim", "○");
				})
				.join(" ");
			add("  " + dots, lines);
			lines.push("");

			const q = questions[index];
			const qBox = new Box(1, 0, (s) => t.bg("userMessageBg", s));
			// Show the assistant's own marker (verbatim) when present, so the modal
			// matches the numbering the user saw in the original message.
			const qPrefix = q.label ? `Q ${q.label} ` : "Q: ";
			qBox.addChild(new Text(t.fg("text", t.bold(qPrefix)) + t.fg("userMessageText", q.question), 0, 0));
			if (q.context) qBox.addChild(new Markdown(q.context, 0, 0, mdTheme, { color: (s) => t.fg("muted", s) }));
			for (const line of qBox.render(width)) add(line, lines);
			lines.push("");

			add("  " + t.fg("muted", "Your answer:"), lines);
			for (const line of editor.render(width - 4)) add("  " + line, lines);
			lines.push("");

			add(t.fg("borderMuted", "─".repeat(width)), lines);
			if (confirming) add("  " + t.fg("warning", "Submit all answers? ") + t.fg("dim", "(y/Enter · n/Esc)"), lines);
			else add("  " + t.fg("dim", "Enter next · Shift+Enter newline · Tab/↑↓ move · Esc cancel"), lines);
			add(t.fg("borderMuted", "─".repeat(width)), lines);

			cachedLines = lines;
			return lines;
		}

		return {
			render,
			invalidate: () => {
				cachedLines = undefined;
				editor.invalidate();
			},
			handleInput,
			// Focusable: forward focus to the editor for cursor/IME positioning.
			get focused() {
				return editor.focused;
			},
			set focused(v: boolean) {
				editor.focused = v;
			},
		};
	};
}

export default function (pi: ExtensionAPI) {
	const run = async (ctx: ExtensionContext): Promise<void> => {
		if (ctx.mode !== "tui") {
			ctx.ui.notify("/answer requires interactive mode", "error");
			return;
		}
		const sourceResult = lastCompletedAssistantText(ctx.sessionManager.getBranch() as never[]);
		if (sourceResult.kind === "incomplete") {
			ctx.ui.notify(`Last assistant message incomplete (${sourceResult.stopReason})`, "warning");
			return;
		}
		if (sourceResult.kind === "none") {
			ctx.ui.notify("No assistant message with questions found", "info");
			return;
		}
		const source = sourceResult.text;

		const outcome = await extractQuestions(ctx, source);
		if (outcome.kind === "cancelled") return;
		if (outcome.kind === "error") {
			ctx.ui.notify(outcome.message, "error");
			return;
		}
		if (outcome.questions.length === 0) {
			ctx.ui.notify("No questions found in the last message", "info");
			return;
		}

		const result = await ctx.ui.custom<AnswerResult>((tui, theme, _kb, done) =>
			buildQnA(outcome.questions)(tui, theme, done),
		);
		if ("cancelled" in result) return;

		pi.sendMessage(
			{
				customType: "answers",
				content: "I answered your questions:\n\n" + result.answers,
				display: true,
			},
			{ triggerTurn: true },
		);
	};

	pi.registerCommand("answer", {
		description: "Extract questions from the last assistant message and answer them interactively",
		handler: (_args, ctx) => run(ctx),
	});
	pi.registerShortcut("ctrl+.", {
		description: "Extract and answer questions",
		handler: (ctx) => run(ctx),
	});
}
