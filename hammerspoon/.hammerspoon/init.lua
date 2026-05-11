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
local HELP_SECTIONS = {
	Window = {},
	Desktops = {},
	Apps = {},
}

-- Helpers

-- Electron apps where app:allWindows() is prohibitively slow
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

	-- Slow path: full enumeration (skipped for Electron apps)
	for _, win in ipairs(app:allWindows()) do
		if win:isStandard() and onSpace[win:id()] then
			table.insert(results, win)
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

	local nextIndex = (currentIndex % #units) + 1
	keyState[winId] = nextIndex
	cycleStateByKey[key] = keyState
	moveToUnit(win, units[nextIndex])
end

-- Get all standard windows on the current space for the given app names, sorted by ID
local function getWindowsOnCurrentSpace(appNames)
	local onSpace = currentSpaceWindowIds()
	if not onSpace then
		return {}
	end
	local wins = {}
	for _, name in ipairs(appNames) do
		local app = hs.application.get(name)
		if app then
			for _, win in ipairs(app:allWindows()) do
				if win:isStandard() and onSpace[win:id()] then
					table.insert(wins, win)
				end
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
		-- Fast path for Electron apps: skip all AX queries
		for _, name in ipairs(appNames) do
			if SKIP_FULL_ENUM[name] then
				if hs.application.frontmostApplication():pid() == app:pid() then
					hs.eventtap.keyStroke({ "cmd" }, "`", 0)
				else
					app:activate()
				end
				return
			end
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

local function buildHelpText()
	local lines = { "HYPER = " .. formatModifiers(HYPER), "", "Window" }
	for _, line in ipairs(HELP_SECTIONS.Window) do
		table.insert(lines, line)
	end

	table.insert(lines, "")
	table.insert(lines, "Desktops")
	for _, line in ipairs(HELP_SECTIONS.Desktops) do
		table.insert(lines, line)
	end

	table.insert(lines, "")
	table.insert(lines, "Apps")
	table.insert(lines, "Press app key again to cycle windows (if app has multiple)")
	for _, line in ipairs(HELP_SECTIONS.Apps) do
		table.insert(lines, line)
	end

	return table.concat(lines, "\n")
end

local function toggleHelp()
	if helpAlertId then
		hs.alert.closeSpecific(helpAlertId)
		helpAlertId = nil
		return
	end

	local target = hs.window.focusedWindow() or hs.screen.mainScreen()
	helpAlertId = hs.alert.show(buildHelpText(), {
		atScreenEdge = 1,
		fadeInDuration = 0.1,
		fadeOutDuration = 0.1,
		radius = 8,
		strokeWidth = 2,
		textSize = 20,
	}, target, 12)
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

local function sendCmdN(app)
	-- Delay so the synthetic Cmd+N isn't merged with physical HYPER modifiers.
	hs.timer.doAfter(0.05, function()
		hs.eventtap.keyStroke({ "cmd" }, "n", 0, app)
	end)
end

local function alacrittySocket()
	local app = hs.application.get("Alacritty")
	if not app then
		return nil
	end
	local tmpdir = os.getenv("TMPDIR") or "/tmp"
	return string.format("%s/Alacritty-%d.sock", tmpdir, app:pid())
end

-- Open a new Alacritty window on the CURRENT Space via IPC, without
-- activating any existing Alacritty window (which would swoosh macOS to
-- whichever Space already holds an Alacritty window).
local function newAlacrittyWindowHere()
	local sock = alacrittySocket()
	if not sock then
		hs.application.launchOrFocus("Alacritty")
		return
	end
	local cmd = string.format("alacritty msg --socket %q create-window 2>&1", sock)
	local output, ok = hs.execute(cmd, true)
	if not ok then
		hs.alert.show("alacritty msg failed: " .. (output or "?"))
		hs.application.launchOrFocus("Alacritty")
	end
end

-- Hyper+T: focus an Alacritty window on the current Space if one exists
-- (cycling between them when frontmost), otherwise spawn a new window here
-- instead of jumping Spaces to an existing window elsewhere.
local function openOrNewAlacrittyWindow()
	local app = hs.application.get("Alacritty")
	if not app then
		hs.application.launchOrFocus("Alacritty")
		return
	end
	local winsOnSpace = getAppWindowsOnCurrentSpace(app)
	if app:isFrontmost() and #winsOnSpace > 0 then
		cycleWindows(getWindowsOnCurrentSpace({ "Alacritty" }))
		return
	end
	if #winsOnSpace > 0 then
		winsOnSpace[1]:focus()
		return
	end
	newAlacrittyWindowHere()
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
hs.hotkey.bind(HYPER, "Y", openOrNewFinderWindow) -- Y = files (yazi/Finder analogue)
addHelp("Apps", "Y: Files/Finder (new window if frontmost)")
hs.hotkey.bind(HYPER, "T", openOrNewAlacrittyWindow) -- T = Terminal
addHelp("Apps", "T: Alacritty (new window if none on current Space)")
hs.hotkey.bind(HYPER, "padenter", newAlacrittyWindowHere)
hs.hotkey.bind(HYPER, "return", newAlacrittyWindowHere)
addHelp("Apps", "Return / PadEnter: New Alacritty window on current Space")

-- Focus window by direction (matches sway/yazi hjkl)
local function focusDir(method)
	return function()
		withFocusedWindow(function(win)
			win[method](win, hs.window.orderedWindows(), false, false)
		end)
	end
end

hs.hotkey.bind(HYPER, "H", focusDir("focusWindowWest"))
addHelp("Window", "H: Focus left")
hs.hotkey.bind(HYPER, "J", focusDir("focusWindowSouth"))
addHelp("Window", "J: Focus down")
hs.hotkey.bind(HYPER, "K", focusDir("focusWindowNorth"))
addHelp("Window", "K: Focus up")
hs.hotkey.bind(HYPER, "L", focusDir("focusWindowEast"))
addHelp("Window", "L: Focus right")

-- Help
hs.hotkey.bind(HYPER, "/", toggleHelp)
addHelp("Apps", "/: Show this help")
