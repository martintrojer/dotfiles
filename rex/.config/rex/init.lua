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
