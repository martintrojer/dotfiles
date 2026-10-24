/**
 * Pure-helper tests for answer.ts.
 *
 * Both bugs here were silent: a cheap extractor returned no questions for a
 * brainstorm-style "❓ **Q6** …" message and /answer reported "No questions
 * found", and labels like "Q6" rendered as "Q Q6".
 */

import { test } from "node:test";
import assert from "node:assert/strict";

import { checkExtraction, questionPrefix, type Question } from "../answer.ts";

const q: Question = { question: "Hard rename or fallback reads?", label: "Q6" };

test("an empty extraction from a message with question markers is retried", () => {
	assert.throws(() => checkExtraction([], "❓ **Q6: Rename cutover**: hard rename?\n\n➡️ I recommend **1**."));
	assert.throws(() => checkExtraction([], "Q1 yes or no? Q2 which one?"));
});

test("an empty extraction from a message without markers is believed", () => {
	assert.deepEqual(checkExtraction([], "Done. All tests pass."), []);
	// "Q4" inside a word is not a marker.
	assert.deepEqual(checkExtraction([], "Shipped the FAQ4 page."), []);
});

test("found questions pass through", () => {
	assert.deepEqual(checkExtraction([q], "❓ **Q6** …"), [q]);
});

test("questionPrefix does not double a Q the label already carries", () => {
	assert.equal(questionPrefix("Q6"), "Q6 ");
	assert.equal(questionPrefix("q12"), "q12 ");
	assert.equal(questionPrefix("3."), "Q 3. ");
	assert.equal(questionPrefix("a)"), "Q a) ");
	assert.equal(questionPrefix(undefined), "Q: ");
});
