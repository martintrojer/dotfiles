"use strict";

document.querySelectorAll("form.quiz").forEach((quiz) => {
	const questions = [...quiz.querySelectorAll("fieldset.question")];
	const score = quiz.querySelector(".score");
	const checkBtn = quiz.querySelector("button.check");
	const resetBtn = quiz.querySelector("button.reset");

	function gradeOne(q) {
		const picked = q.querySelector("input:checked");
		const fb = q.querySelector(".feedback");
		q.classList.remove("correct", "incorrect", "unanswered");
		if (!picked) {
			q.classList.add("unanswered");
			fb.textContent = "Pick an answer.";
			return false;
		}
		const correct = picked.value === q.dataset.answer;
		q.classList.add(correct ? "correct" : "incorrect");
		// data-why is repo-controlled, pre-rendered HTML (code spans, links, etc.).
		fb.innerHTML = (correct ? "Correct. " : "Not quite. ") + (q.dataset.why || "");
		return correct;
	}

	function check() {
		const correct = questions.filter(gradeOne).length;
		score.textContent = `${correct} / ${questions.length}`;
	}

	function reset() {
		quiz.querySelectorAll("input").forEach((i) => (i.checked = false));
		questions.forEach((q) => {
			q.classList.remove("correct", "incorrect", "unanswered");
			q.querySelector(".feedback").innerHTML = "";
		});
		score.textContent = `0 / ${questions.length}`;
	}

	checkBtn.addEventListener("click", check);
	resetBtn.addEventListener("click", reset);
});
