/**
 * `/loop` — run a prompt on a recurring interval within the current session.
 *
 * A clone of Claude Code's `/loop`. You give it an interval and a prompt, and
 * pi re-sends that prompt at the specified cadence as if you had typed it.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { registerLoopCommand } from "./_lib.ts";

export default function (pi: ExtensionAPI) {
	registerLoopCommand(pi);
}
