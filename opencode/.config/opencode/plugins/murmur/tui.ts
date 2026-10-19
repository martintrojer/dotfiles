// murmur's opencode plugin lives in mu-crew/dotfiles (pinned clone; see
// _dotfiles_sync/pins.py). opencode loads TUI plugins only from
// plugins/<name>/tui.*, so this shim stays here and re-exports it.
import { homedir } from "node:os";
import { join } from "node:path";

const plugin = join(homedir(), ".local/share/mu-crew-dotfiles/opencode/plugins/murmur/tui.ts");
export default (await import(plugin)).default;
