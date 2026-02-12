-- Settings
hs.window.animationDuration = 0
local MEH = { "shift", "alt", "ctrl" }
local HYPER = { "shift", "cmd", "alt", "ctrl" }
local SMASH = MEH -- HYPER
local BROWSER = { "Safari" } -- or { "Google Chrome", "Chrome" }
local IDE = { "Visual Studio Code" }

-- Layout units
local UNITS = {
	full = hs.geometry.rect(0.0, 0.0, 1.0, 1.0),
	center90 = hs.geometry.rect(0.05, 0.05, 0.9, 0.9),
	topLeftQuarter = hs.geometry.rect(0.0, 0.0, 0.5, 0.5),
	topRightQuarter = hs.geometry.rect(0.5, 0.0, 0.5, 0.5),
	bottomLeftQuarter = hs.geometry.rect(0.0, 0.5, 0.5, 0.5),
	bottomRightQuarter = hs.geometry.rect(0.5, 0.5, 0.5, 0.5),
	topLeftTwoThirds = hs.geometry.rect(0.0, 0.0, 2 / 3, 2 / 3),
	topRightTwoThirds = hs.geometry.rect(1 / 3, 0.0, 2 / 3, 2 / 3),
	bottomLeftTwoThirds = hs.geometry.rect(0.0, 1 / 3, 2 / 3, 2 / 3),
	bottomRightTwoThirds = hs.geometry.rect(1 / 3, 1 / 3, 2 / 3, 2 / 3),
}

-- 3-state cycle units (1/3 -> 1/2 -> 2/3)
local CYCLE_UNITS = {
	E = {
		hs.geometry.rect(0.0, 0.0, 1.0, 1 / 3),
		hs.geometry.rect(0.0, 0.0, 1.0, 0.5),
		hs.geometry.rect(0.0, 0.0, 1.0, 2 / 3),
	},
	S = {
		hs.geometry.rect(0.0, 0.0, 1 / 3, 1.0),
		hs.geometry.rect(0.0, 0.0, 0.5, 1.0),
		hs.geometry.rect(0.0, 0.0, 2 / 3, 1.0),
	},
	F = {
		hs.geometry.rect(2 / 3, 0.0, 1 / 3, 1.0),
		hs.geometry.rect(0.5, 0.0, 0.5, 1.0),
		hs.geometry.rect(1 / 3, 0.0, 2 / 3, 1.0),
	},
	C = {
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
	["Codex"] = true,
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

local function getAppWindowsOnCurrentSpace(app)
	local currentSpace = hs.spaces.focusedSpace()
	local ok, spaceWinIDs = pcall(hs.spaces.windowsForSpace, currentSpace)
	if not ok or not spaceWinIDs then
		return {}
	end

	local onSpace = {}
	for _, wid in ipairs(spaceWinIDs) do
		onSpace[wid] = true
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

local function moveToUnit(win, unit)
	win:moveToUnit(unit)
end

local function isAtUnit(win, unit)
	local current = win:screen():toUnitRect(win:frame())
	local tolerance = 0.03

	return math.abs(current.x - unit.x) <= tolerance
		and math.abs(current.y - unit.y) <= tolerance
		and math.abs(current.w - unit.w) <= tolerance
		and math.abs(current.h - unit.h) <= tolerance
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

local function getRunningApp(appNames)
	for _, appName in ipairs(appNames) do
		local app = hs.application.get(appName)
		if app then
			return app
		end
	end

	return nil
end

local function launchOrFocusApp(appNames, fallback)
	local app = getRunningApp(appNames)
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
			hs.eventtap.keyStroke({ "cmd" }, "`", 0)
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

	if fallback then
		fallback()
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
	local lines = { "SMASH = " .. formatModifiers(SMASH), "", "Window" }
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

	helpAlertId = hs.alert.show(buildHelpText(), {
		atScreenEdge = 2,
		fadeInDuration = 0.1,
		fadeOutDuration = 0.1,
		radius = 8,
		strokeWidth = 2,
		textSize = 20,
	}, nil, 12)
end

-- Binding helpers
local function bindCycle(key, helpText)
	hs.hotkey.bind(SMASH, key, function()
		withFocusedWindow(function(win)
			cycleUnitsForKey(win, key, CYCLE_UNITS[key])
		end)
	end)
	addHelp("Window", string.format("%s: %s", key, helpText))
end

local function bindToggle(key, unitA, unitB, helpText)
	hs.hotkey.bind(SMASH, key, function()
		withFocusedWindow(function(win)
			if isAtUnit(win, unitA) then
				moveToUnit(win, unitB)
			elseif isAtUnit(win, unitB) then
				moveToUnit(win, unitA)
			else
				moveToUnit(win, unitA)
			end
		end)
	end)
	addHelp("Window", string.format("%s: %s", key, helpText))
end

local function bindApp(key, appNames, helpText, fallback)
	hs.hotkey.bind(SMASH, key, function()
		launchOrFocusApp(appNames, fallback)
	end)
	addHelp("Apps", string.format("%s: %s", key, helpText))
end

local function bindDesktop(key, desktopNumber)
	hs.hotkey.bind(SMASH, key, function()
		hs.eventtap.keyStroke({ "ctrl" }, tostring(desktopNumber), 0)
	end)
	addHelp("Desktops", string.format("%s: Go to Desktop %d (Ctrl+%d)", key, desktopNumber, desktopNumber))
end

local function openOrNewFinderWindow()
	local app = hs.application.get("Finder")
	if app then
		local winsOnSpace = getAppWindowsOnCurrentSpace(app)
		if app:isFrontmost() and #winsOnSpace > 0 then
			hs.eventtap.keyStroke({ "cmd" }, "n", 0, app)
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
bindToggle("W", UNITS.topLeftQuarter, UNITS.topLeftTwoThirds, "Top-left toggle (1/2 <-> 2/3)")
bindToggle("R", UNITS.topRightQuarter, UNITS.topRightTwoThirds, "Top-right toggle (1/2 <-> 2/3)")
bindToggle("X", UNITS.bottomLeftQuarter, UNITS.bottomLeftTwoThirds, "Bottom-left toggle (1/2 <-> 2/3)")
bindToggle("V", UNITS.bottomRightQuarter, UNITS.bottomRightTwoThirds, "Bottom-right toggle (1/2 <-> 2/3)")

bindCycle("E", "Top cycle (1/3, 1/2, 2/3)")
bindCycle("S", "Left cycle (1/3, 1/2, 2/3)")
bindCycle("F", "Right cycle (1/3, 1/2, 2/3)")
bindCycle("C", "Bottom cycle (1/3, 1/2, 2/3)")

-- Toggle full <-> centered 90%
hs.hotkey.bind(SMASH, "D", function()
	withFocusedWindow(function(win)
		if isAtUnit(win, UNITS.full) then
			moveToUnit(win, UNITS.center90)
		else
			moveToUnit(win, UNITS.full)
		end
	end)
end)
addHelp("Window", "D: Toggle Full <-> Center 90%")

-- Desktop bindings (requires Mission Control shortcuts for Ctrl+1..5)
bindDesktop("1", 1)
bindDesktop("2", 2)
bindDesktop("3", 3)
bindDesktop("4", 4)
bindDesktop("5", 5)

-- App bindings (SMASH + mnemonic letter)
bindApp("B", BROWSER, "Browser") -- B = Browser
bindApp("I", IDE, "IDE") -- I = IDE
bindApp("O", { "Codex" }, "Codex") -- O = Open Codex
bindApp("G", { "Google Chat" }, "Google Chat", function() -- G = Google Chat
	hs.urlevent.openURLWithBundle("https://chat.google.com", "com.google.Chrome")
end)
bindApp("M", { "Music" }, "Music") -- M = Music
hs.hotkey.bind(SMASH, "N", openOrNewFinderWindow) -- N = fiNder/new window
addHelp("Apps", "N: Finder (new window if frontmost)")
bindApp("Q", { "WhatsApp" }, "WhatsApp") -- Q = chat/quick message
bindApp("T", { "Ghostty" }, "Ghostty") -- T = Terminal
bindApp("Y", { "Activity Monitor" }, "Activity Monitor") -- Y = activitY monitor
bindApp("Z", { "zoom.us", "Zoom Workplace", "Zoom" }, "Zoom") -- Z = Zoom
bindApp(",", { "System Settings", "System Preferences" }, "System Settings") -- , = settings
hs.hotkey.bind(SMASH, "return", function() -- Return = new terminal window
	local app = hs.application.get("Ghostty")
	if app then
		hs.eventtap.keyStroke({ "cmd" }, "n", 0, app)
	else
		hs.application.launchOrFocus("Ghostty")
	end
end)
addHelp("Apps", "Return: New Ghostty window")

-- Help
hs.hotkey.bind(SMASH, "/", toggleHelp)
addHelp("Apps", "/: Show this help")
