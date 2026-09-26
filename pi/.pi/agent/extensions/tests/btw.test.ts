import { test } from "node:test";
import assert from "node:assert/strict";
import type { Message } from "@earendil-works/pi-ai";

import { withoutSystemMessages } from "../_lib.ts";

const timestamp = Date.now();

test("side-thread context drops replayed system prompts and tool declarations", () => {
	const messages = [
		{
			role: "system",
			content: "You are a coding agent.",
			toolsAdded: [{ name: "read", description: "Read a file", parameters: {} }],
			timestamp,
		},
		{ role: "user", content: [{ type: "text", text: "Inspect it" }], timestamp },
	] as Message[];

	assert.deepEqual(withoutSystemMessages(messages), [messages[1]]);
});
