-- Settings
hs.window.animationDuration = 0
local GAP = 4 -- pixels: gap between adjacent windows (no gap against screen edges)
local HYPER = { "shift", "cmd", "alt", "ctrl" }
local BROWSER = { "Safari" } -- or { "Google Chrome", "Chrome" }
local IDE = { "Visual Studio Code" }

-- Layout units
local UNITS = {
	full = hs.geometry.rect(0.0, 0.0, 1.0, 1.0),
	center90 = hs.geometry.rect(0.05, 0.05, 0.9, 0.9),
	tallCenter = hs.geometry.rect(0.15, 0.0, 0.7, 1.0),
}

-- Window layout cycles
local CYCLE_UNITS = {
	Q = {
		hs.geometry.rect(0.0, 0.0, 1 / 3, 1.0),
		hs.geometry.rect(0.0, 0.0, 0.5, 1.0),
		hs.geometry.rect(0.0, 0.0, 2 / 3, 1.0),
	},
	R = { UNITS.full, UNITS.center90, UNITS.tallCenter },
	W = {
		hs.geometry.rect(2 / 3, 0.0, 1 / 3, 1.0),
		hs.geometry.rect(0.5, 0.0, 0.5, 1.0),
		hs.geometry.rect(1 / 3, 0.0, 2 / 3, 1.0),
	},
	E = {
		hs.geometry.rect(0.0, 0.0, 1.0, 1 / 3),
		hs.geometry.rect(0.0, 0.0, 1.0, 0.5),
		hs.geometry.rect(0.0, 0.0, 1.0, 2 / 3),
	},
	X = {
		hs.geometry.rect(0.0, 2 / 3, 1.0, 1 / 3),
		hs.geometry.rect(0.0, 0.5, 1.0, 0.5),
		hs.geometry.rect(0.0, 1 / 3, 1.0, 2 / 3),
	},
}

-- Runtime cycle state, per key and per focused window id
local cycleStateByKey = {}
local helpAlertId = nil
local HELP_SECTION_ORDER = { "Window", "Apps" }
local HELP_SECTIONS = {
	Window = {},
	Apps = {},
}

-- Helpers

-- Apps where the AX fast path (focusedWindow / mainWindow / allWindows) is
-- unreliable or expensive. For Electron apps it's prohibitively slow; for
-- Ghostty `app:allWindows()` returns empty so the fast path always wastes
-- AX round-trips before falling through to the SkyLight slow path anyway.
local SKIP_FULL_ENUM = {
	["Code"] = true,
	["Cider"] = true,
	["Slack"] = true,
	["Discord"] = true,
	["Microsoft Teams"] = true,
	["Figma"] = true,
	["Notion"] = true,
	["Obsidian"] = true,
	["1Password"] = true,
	["Bitwarden"] = true,
	["Signal"] = true,
	["Spotify"] = true,
	["WhatsApp"] = true,
	["Ghostty"] = true,
}
for _, name in ipairs(IDE) do
	SKIP_FULL_ENUM[name] = true
end

-- Set of window IDs on the currently-focused Space, or nil if hs.spaces
-- query fails. Both window-enumeration helpers below need this prelude;
-- factored out to avoid the (previously duplicated) 8-line dance.
local function currentSpaceWindowIds()
	local space = hs.spaces.focusedSpace()
	local ok, ids = pcall(hs.spaces.windowsForSpace, space)
	if not space or not ok or not ids then
		return nil
	end
	local set = {}
	for _, wid in ipairs(ids) do
		set[wid] = true
	end
	return set
end

local function getAppWindowsOnCurrentSpace(app)
	local onSpace = currentSpaceWindowIds()
	if not onSpace then
		return {}
	end

	-- Fast path: check mainWindow/focusedWindow (single AX query each)
	local seen = {}
	local results = {}
	for _, win in ipairs({ app:focusedWindow(), app:mainWindow() }) do
		if win and not seen[win:id()] and onSpace[win:id()] then
			seen[win:id()] = true
			table.insert(results, win)
		end
	end
	if #results > 0 or SKIP_FULL_ENUM[app:name()] then
		return results
	end

	-- Slow path: enumerate via SkyLight rather than the app's AX AXWindows
	-- attribute (app:allWindows()). Some apps -- notably Ghostty -- return an
	-- empty AXWindows list even when their windows are perfectly enumerable
	-- through SkyLight. hs.window.visibleWindows() already restricts to the
	-- current Space and skips minimized windows, so it's much cheaper than
	-- hs.window.allWindows() and good enough for our "windows on current
	-- Space" use case (onSpace[] is kept as a belt-and-braces filter).
	local pid = app:pid()
	for _, win in ipairs(hs.window.visibleWindows()) do
		local wid = win:id()
		if wid and onSpace[wid] and not seen[wid] then
			local a = win:application()
			if a and a:pid() == pid then
				seen[wid] = true
				table.insert(results, win)
			end
		end
	end
	return results
end

local function withFocusedWindow(action)
	local win = hs.window.focusedWindow()
	if win then
		action(win)
	end
end

local function gappedFrame(sf, unit)
	-- Inner gaps only: screen-edge sides get 0; interior sides get half GAP
	-- so two adjacent windows produce a full GAP between them, while windows
	-- touching the screen edge sit flush against it.
	local padL = (unit.x < 0.01) and 0 or (GAP / 2)
	local padT = (unit.y < 0.01) and 0 or (GAP / 2)
	local padR = (unit.x + unit.w > 0.99) and 0 or (GAP / 2)
	local padB = (unit.y + unit.h > 0.99) and 0 or (GAP / 2)

	return hs.geometry.rect(
		sf.x + unit.x * sf.w + padL,
		sf.y + unit.y * sf.h + padT,
		unit.w * sf.w - padL - padR,
		unit.h * sf.h - padT - padB
	)
end

local function moveToUnit(win, unit)
	win:setFrame(gappedFrame(win:screen():frame(), unit))
end

local function isAtUnit(win, unit)
	local f = win:frame()
	local gf = gappedFrame(win:screen():frame(), unit)
	local tolerance = 5
	return math.abs(f.x - gf.x) <= tolerance
		and math.abs(f.y - gf.y) <= tolerance
		and math.abs(f.w - gf.w) <= tolerance
		and math.abs(f.h - gf.h) <= tolerance
end

local function cycleUnitsForKey(win, key, units)
	local keyState = cycleStateByKey[key] or {}
	local winId = tostring(win:id() or 0)
	local currentIndex = keyState[winId] or 0

	for i, unit in ipairs(units) do
		if isAtUnit(win, unit) then
			currentIndex = i
			break
		end
	end

	-- Drop entries for windows that no longer exist so the per-window state
	-- table doesn't grow unboundedly as windows are opened/closed.
	for staleId in pairs(keyState) do
		if not hs.window.get(tonumber(staleId)) then
			keyState[staleId] = nil
		end
	end

	local nextIndex = (currentIndex % #units) + 1
	keyState[winId] = nextIndex
	cycleStateByKey[key] = keyState
	moveToUnit(win, units[nextIndex])
end

-- Get all windows on the current space for the given app names, sorted by
-- ID. Goes through SkyLight (hs.window.visibleWindows()) rather than
-- app:allWindows() so apps with empty AX AXWindows (Ghostty) still work.
-- visibleWindows already restricts to the current Space, so we filter by
-- pid only and skip the per-window isStandard() AX call.
local function getWindowsOnCurrentSpace(appNames)
	local onSpace = currentSpaceWindowIds()
	if not onSpace then
		return {}
	end
	local wantedPids = {}
	for _, name in ipairs(appNames) do
		local app = hs.application.get(name)
		if app then
			wantedPids[app:pid()] = true
		end
	end
	if not next(wantedPids) then
		return {}
	end
	local wins = {}
	for _, win in ipairs(hs.window.visibleWindows()) do
		local wid = win:id()
		if wid and onSpace[wid] then
			local a = win:application()
			if a and wantedPids[a:pid()] then
				table.insert(wins, win)
			end
		end
	end
	table.sort(wins, function(a, b)
		return a:id() < b:id()
	end)
	return wins
end

local function cycleWindows(wins)
	if #wins == 0 then
		return
	end
	if #wins == 1 then
		wins[1]:focus()
		return
	end
	local focused = hs.window.focusedWindow()
	local nextIdx = 1
	for i, w in ipairs(wins) do
		if focused and w:id() == focused:id() then
			nextIdx = (i % #wins) + 1
			break
		end
	end
	wins[nextIdx]:focus()
end

local function launchOrFocusApp(appNames)
	local app = nil
	for _, appName in ipairs(appNames) do
		app = hs.application.get(appName)
		if app then
			break
		end
	end

	if app then
		-- Fast path for apps where the AX window enumeration is unreliable or
		-- prohibitively slow: just activate, or Cmd+` to cycle if frontmost.
		-- Note: Cmd+` cycles across all Spaces, which can swoosh Spaces. We
		-- accept that trade-off here to avoid an expensive app:allWindows().
		if SKIP_FULL_ENUM[app:name()] then
			if app:isFrontmost() then
				hs.eventtap.keyStroke({ "cmd" }, "`", 0)
			else
				app:activate()
			end
			return
		end

		local winsOnSpace = getAppWindowsOnCurrentSpace(app)
		if app:isFrontmost() and #winsOnSpace > 0 then
			cycleWindows(getWindowsOnCurrentSpace(appNames))
			return
		end
		if #winsOnSpace > 0 then
			winsOnSpace[1]:focus()
			return
		end

		-- App running but no windows on current space; don't switch desktops
		return
	end

	for _, appName in ipairs(appNames) do
		if hs.application.launchOrFocus(appName) then
			return
		end
	end
end

local function addHelp(section, line)
	table.insert(HELP_SECTIONS[section], line)
end

local function formatModifiers(modifiers)
	local names = {
		shift = "Shift",
		ctrl = "Ctrl",
		alt = "Alt",
		cmd = "Cmd",
	}
	local formatted = {}

	for _, modifier in ipairs(modifiers) do
		table.insert(formatted, names[modifier] or modifier)
	end

	return table.concat(formatted, " + ")
end

local SECTION_INTROS = {
	Apps = "Press app key again to cycle windows (if app has multiple)",
}

local function buildHelpText()
	local lines = { "HYPER = " .. formatModifiers(HYPER) }
	for _, section in ipairs(HELP_SECTION_ORDER) do
		table.insert(lines, "")
		table.insert(lines, section)
		if SECTION_INTROS[section] then
			table.insert(lines, SECTION_INTROS[section])
		end
		for _, line in ipairs(HELP_SECTIONS[section]) do
			table.insert(lines, line)
		end
	end
	return table.concat(lines, "\n")
end

local function toggleHelp()
	if helpAlertId then
		hs.alert.closeSpecific(helpAlertId)
		helpAlertId = nil
		return
	end

	-- hs.alert.show signature: (text, style, screen, seconds). The third arg
	-- must be a screen; passing a window silently fails on some HS versions.
	local focused = hs.window.focusedWindow()
	local screen = (focused and focused:screen()) or hs.screen.mainScreen()
	helpAlertId = hs.alert.show(buildHelpText(), {
		atScreenEdge = 1,
		fadeInDuration = 0.1,
		fadeOutDuration = 0.1,
		radius = 8,
		strokeWidth = 2,
		textSize = 20,
	}, screen, 12)
end

-- Binding helpers
local function bindCycle(key, helpText)
	hs.hotkey.bind(HYPER, key, function()
		withFocusedWindow(function(win)
			cycleUnitsForKey(win, key, CYCLE_UNITS[key])
		end)
	end)
	addHelp("Window", string.format("%s: %s", key, helpText))
end

local function bindApp(key, appNames, helpText)
	hs.hotkey.bind(HYPER, key, function()
		launchOrFocusApp(appNames)
	end)
	addHelp("Apps", string.format("%s: %s", key, helpText))
end

-- Bind an arbitrary function under HYPER, recording a help entry.
local function bindCustom(section, key, fn, helpText)
	hs.hotkey.bind(HYPER, key, fn)
	addHelp(section, string.format("%s: %s", key, helpText))
end

local function sendCmdN(app)
	-- Delay so the synthetic Cmd+N isn't merged with physical HYPER modifiers.
	hs.timer.doAfter(0.05, function()
		hs.eventtap.keyStroke({ "cmd" }, "n", 0, app)
	end)
end

-- Hyper+Return / Hyper+PadEnter: open a new Ghostty window using the
-- EXISTING Ghostty process (single Dock icon). We drive `File > New Window`
-- via AX without activating Ghostty, which avoids the Space swoosh that
-- `app:activate()` would cause when Ghostty's last window is elsewhere, and
-- avoids the per-Space proliferation of `open -na` (which spawns a fresh
-- Ghostty.app instance every time and lands a new Dock icon).
local function newGhosttyWindowHere()
	local app = hs.application.get("Ghostty")
	if not app then
		hs.application.launchOrFocus("Ghostty")
		return
	end
	if app:isFrontmost() then
		sendCmdN(app)
		return
	end
	-- Try the common menu paths Ghostty exposes for new windows.
	for _, path in ipairs({ { "File", "New Window" }, { "Ghostty", "New Window" } }) do
		if app:selectMenuItem(path) then
			return
		end
	end
	hs.alert.show("Ghostty: New Window menu item not found")
end

-- Hyper+T: focus (or cycle) a Ghostty window on the current Space.
-- No-op if Ghostty isn't running or has no window on the current Space --
-- we never launch Ghostty or jump Spaces from here. Use Hyper+Return to
-- spawn a new window.
local function focusTerminalOnCurrentSpace()
	local app = hs.application.get("Ghostty")
	if not app then
		return
	end
	local wins = getWindowsOnCurrentSpace({ "Ghostty" })
	if #wins == 0 then
		return
	end
	if app:isFrontmost() then
		cycleWindows(wins)
		return
	end
	wins[1]:focus()
end

local function openOrNewFinderWindow()
	local app = hs.application.get("Finder")
	if app then
		local winsOnSpace = getAppWindowsOnCurrentSpace(app)
		if app:isFrontmost() and #winsOnSpace > 0 then
			sendCmdN(app)
			return
		end
		if #winsOnSpace > 0 then
			winsOnSpace[1]:focus()
			return
		end
		-- No Finder window on current space; open one here
		hs.execute("/usr/bin/open " .. os.getenv("HOME"), true)
		return
	end

	launchOrFocusApp({ "Finder" })
end

-- Key bindings
bindCycle("Q", "Left cycle (1/3, 1/2, 2/3)")
bindCycle("R", "Cycle Full -> Center 90% -> Tall Center")
bindCycle("W", "Right cycle (1/3, 1/2, 2/3)")
bindCycle("E", "Top cycle (1/3, 1/2, 2/3)")
bindCycle("X", "Bottom cycle (1/3, 1/2, 2/3)")

-- App bindings (HYPER + mnemonic letter, aligned with sway where it makes sense)
bindApp("B", BROWSER, "Browser") -- B = Browser
bindApp("I", IDE, "IDE") -- I = IDE
bindApp("M", { "Music" }, "Music") -- M = Music
bindCustom("Apps", "Y", openOrNewFinderWindow, "Files/Finder (new window if frontmost)")
bindCustom("Apps", "T", focusTerminalOnCurrentSpace, "Focus Ghostty on current Space (no-op if none)")
bindCustom("Apps", "return", newGhosttyWindowHere, "New Ghostty window on current Space")
hs.hotkey.bind(HYPER, "padenter", newGhosttyWindowHere) -- duplicate of return on numpad

-- Focus window by direction (matches sway/yazi hjkl)
local function focusDir(method)
	return function()
		withFocusedWindow(function(win)
			win[method](win, hs.window.orderedWindows(), false, false)
		end)
	end
end

bindCustom("Window", "H", focusDir("focusWindowWest"), "Focus left")
bindCustom("Window", "J", focusDir("focusWindowSouth"), "Focus down")
bindCustom("Window", "K", focusDir("focusWindowNorth"), "Focus up")
bindCustom("Window", "L", focusDir("focusWindowEast"), "Focus right")

-- Help. Bound to `;` rather than `/` because macOS's built-in "Help menu
-- search" shortcut (Cmd+Shift+?) swallows the Cmd+Shift+/ event before
-- Hammerspoon sees it, even with the full HYPER modifier set.
bindCustom("Apps", ";", toggleHelp, "Show this help")
