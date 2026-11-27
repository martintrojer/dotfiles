-- Rex key bindings. Everything else stays Rex's defaults; see rex/README.md.
-- luacheck: read globals rex

-- Ctrl+h/j/k/l move between panes, vim-tmux-navigator style. When the focused
-- pane runs nvim (or tmux, which has its own navigator) the key goes to it,
-- and nvim hands focus back with `rex focus DIR` at its edge (keymaps/core.lua).
local terminal = "com.superlogical.terminal"
local passthrough = { nvim = true, vim = true, tmux = true }

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

-- Cmd+J: the mu-crew agent switcher (jump to any agent murmur sees, Rex or
-- tmux, this Mac or a peer). It lives in mu-crew/dotfiles (rex/mu-crew.lua),
-- cloned by dotfiles-sync at the pin in _dotfiles_sync/pins.py, like the
-- tmux/mu-crew.conf .tmux.conf sources. Fail-soft until that clone has rex/.
package.path = os.getenv("HOME") .. "/.local/share/mu-crew-dotfiles/rex/?.lua;" .. package.path
local ok, crew = pcall(require, "mu-crew")
if ok then
	-- The Rex server runs with launchd's PATH; murmur is a node script and
	-- node comes from mise, so its shims must be on the switcher's PATH.
	local home = os.getenv("HOME")
	crew.setup({
		key = "cmd+j",
		path = { home .. "/.local/bin", home .. "/.local/share/mise/shims", "/opt/homebrew/bin", "/usr/local/bin" },
	})
end
