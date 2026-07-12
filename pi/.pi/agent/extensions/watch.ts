/**
 * `/watch` — a change-detecting variant of `/loop`.
 *
 * Runs a probe command on each tick and only injects its prompt when the probe
 * output changes. The first successful tick establishes a baseline and does not
 * fire, so arming a watch never spuriously wakes the agent.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { registerWatchCommand } from "./_lib.ts";

export default function (pi: ExtensionAPI) {
	registerWatchCommand(pi);
}
