-- Rex key bindings. Everything else stays Rex's defaults; see rex/README.md.
-- luacheck: read globals rex

-- Ctrl+h/j/k/l move between panes, vim-tmux-navigator style. When the focused
-- pane runs nvim, debrief, ramble (or tmux, which has its own navigator) the
-- key goes to it, and it hands focus back with `rex focus DIR` at its edge.
local terminal = "com.superlogical.terminal"
local passthrough = { nvim = true, vim = true, tmux = true, debrief = true, ramble = true }

local function navigate(direction)
	return function(ctx)
		local p = rex.block.call(terminal, "process", { block_id = ctx.block_id })
		local fg = p and p.foreground and p.foreground.name
		if fg and passthrough[fg] then
			return false
		end
		rex.session.focus_direction({ direction = direction })
		return true
	end
end

rex.bind("ctrl+h", navigate("left"))
rex.bind("ctrl+j", navigate("down"))
rex.bind("ctrl+k", navigate("up"))
rex.bind("ctrl+l", navigate("right"))

-- The Rex server runs with launchd's PATH; tools like murmur, fzf, rex, and pi
-- live in ~/.local/bin, mise shims, Homebrew, or /usr/local/bin.
local home = os.getenv("HOME")
local path_dirs = {
	home .. "/.local/bin",
	home .. "/.local/share/mise/shims",
	home .. "/.tmux/plugins/tmux-session-picker/bin",
	"/opt/homebrew/bin",
	"/usr/local/bin",
}
local full_path = table.concat(path_dirs, ":") .. ":/usr/bin:/bin:/usr/sbin:/sbin"

-- Cmd+J: the mu-crew agent switcher (jump to any agent murmur sees, Rex or
-- tmux, this Mac or a peer). It lives in mu-crew/dotfiles (rex/mu-crew.lua),
-- cloned by dotfiles-sync at the pin in _dotfiles_sync/pins.py, like the
-- tmux/mu-crew.conf .tmux.conf sources. Fail-soft until that clone has rex/.
package.path = home .. "/.local/share/mu-crew-dotfiles/rex/?.lua;" .. package.path
local ok, crew = pcall(require, "mu-crew")
if ok then
	crew.setup({ key = "cmd+j", path = path_dirs })
end

-- Leader + P (`cmd+b` then `P`): pi-restore fzf picker in an 80% floating
-- layer, matching vtmux's `prefix + P` and mu-crew's `mu_crew_leader`.
-- Checks --rows first so an empty list doesn't flash a layer.
local pi_restore = home .. "/.local/bin/pi-restore"
rex.action({
	name = "pi_restore",
	title = "Restore Pi Session…",
	description = "Open the pi-restore fzf picker in a floating layer.",
	keywords = "pi restore session star picker resume",
	run = function(ctx)
		if ctx.server then
			error("pi-restore is local only", 0)
		end
		local r, err = rex.exec({ pi_restore, "--rows" }, { env = { PATH = full_path }, timeout = 5 })
		if r == nil then
			error("pi-restore: " .. tostring(err), 0)
		end
		if not r.ok then
			error("pi-restore: " .. (r.stderr or ("exit " .. tostring(r.code))), 0)
		end
		if not r.stdout:match("%S") then
			return
		end
		local layer, lerr = rex.call("session.new_layer", {
			session_id = ctx.session_id,
			bounds = { 0.1, 0.1, 0.8, 0.8 },
			focus = true,
			layout = rex.layout.block({
				flavor = "com.superlogical.terminal.shell",
				label = "pi-restore",
				options = {
					command = { "/bin/sh", "-c", 'export PATH="$0"; exec "$1"', full_path, pi_restore },
				},
			}),
		})
		if type(layer) ~= "table" then
			error("pi-restore layer: " .. tostring(lerr), 0)
		end
	end,
})
if ok then
	rex.bind("mu_crew_leader/shift+p", "pi_restore")
end
