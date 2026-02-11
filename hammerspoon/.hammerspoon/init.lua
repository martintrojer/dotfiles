-- Settings
hs.window.animationDuration = 0
local MEH = { "shift", "alt", "ctrl" }

-- Layout units
local UNITS = {
	full = hs.geometry.rect(0.0, 0.0, 1.0, 1.0),
	center80 = hs.geometry.rect(0.1, 0.1, 0.8, 0.8),
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
local HELP_TEXT = table.concat({
	"MEH = Shift + Alt + Ctrl",
	"",
	"Window",
	"W: Top-left toggle (1/2 <-> 2/3)",
	"E: Top cycle (1/3, 1/2, 2/3)",
	"R: Top-right toggle (1/2 <-> 2/3)",
	"S: Left cycle (1/3, 1/2, 2/3)",
	"D: Toggle Full <-> Center 80%",
	"F: Right cycle (1/3, 1/2, 2/3)",
	"X: Bottom-left toggle (1/2 <-> 2/3)",
	"C: Bottom cycle (1/3, 1/2, 2/3)",
	"V: Bottom-right toggle (1/2 <-> 2/3)",
	"",
	"Desktops",
	"1: Go to Desktop 1 (Ctrl+1)",
	"2: Go to Desktop 2 (Ctrl+2)",
	"3: Go to Desktop 3 (Ctrl+3)",
	"4: Go to Desktop 4 (Ctrl+4)",
	"",
	"Apps",
	"Press app key again to cycle windows (if app has multiple)",
	"A: Safari",
	"H: Chrome",
	"I: VS Code",
	"G: Ghostty",
	"M: Music",
	"Q: WhatsApp",
	"T: Google Chat",
	",: System Settings",
	"/: Show this help",
}, "\n")

-- Helpers
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
		if app:isFrontmost() then
			hs.eventtap.keyStroke({ "cmd" }, "`", 0)
			return
		end

		app:activate(true)
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

local function toggleHelp()
	if helpAlertId then
		hs.alert.closeSpecific(helpAlertId)
		helpAlertId = nil
		return
	end

	helpAlertId = hs.alert.show(HELP_TEXT, {
		atScreenEdge = 2,
		fadeInDuration = 0.1,
		fadeOutDuration = 0.1,
		radius = 8,
		strokeWidth = 2,
		textSize = 20,
	}, nil, 0)
end

-- Binding helpers
local function bindCycle(key)
	hs.hotkey.bind(MEH, key, function()
		withFocusedWindow(function(win)
			cycleUnitsForKey(win, key, CYCLE_UNITS[key])
		end)
	end)
end

local function bindToggle(key, unitA, unitB)
	hs.hotkey.bind(MEH, key, function()
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
end

local function bindApp(key, appNames, fallback)
	hs.hotkey.bind(MEH, key, function()
		launchOrFocusApp(appNames, fallback)
	end)
end

local function bindDesktop(key, desktopNumber)
	hs.hotkey.bind(MEH, key, function()
		hs.eventtap.keyStroke({ "ctrl" }, tostring(desktopNumber), 0)
	end)
end

-- Key bindings
bindToggle("W", UNITS.topLeftQuarter, UNITS.topLeftTwoThirds)
bindToggle("R", UNITS.topRightQuarter, UNITS.topRightTwoThirds)
bindToggle("X", UNITS.bottomLeftQuarter, UNITS.bottomLeftTwoThirds)
bindToggle("V", UNITS.bottomRightQuarter, UNITS.bottomRightTwoThirds)

bindCycle("E")
bindCycle("S")
bindCycle("F")
bindCycle("C")

-- Toggle full <-> centered 80%
hs.hotkey.bind(MEH, "D", function()
	withFocusedWindow(function(win)
		if isAtUnit(win, UNITS.full) then
			moveToUnit(win, UNITS.center80)
		else
			moveToUnit(win, UNITS.full)
		end
	end)
end)

-- Desktop bindings (requires Mission Control shortcuts for Ctrl+1..4)
bindDesktop("1", 1)
bindDesktop("2", 2)
bindDesktop("3", 3)
bindDesktop("4", 4)

-- App bindings (MEH + mnemonic letter)
bindApp("A", { "Safari" }) -- A = Apple Safari
bindApp("H", { "Google Chrome", "Chrome" }) -- H = cHrome
bindApp("I", { "Visual Studio Code", "Code" }) -- I = IDE
bindApp("G", { "Ghostty" }) -- G = Ghostty
bindApp("M", { "Music" }) -- M = Music
bindApp("Q", { "WhatsApp" }) -- Q = chat/quick message
bindApp("T", { "Google Chat" }, function() -- T = chaT
	hs.urlevent.openURLWithBundle("https://chat.google.com", "com.google.Chrome")
end)
bindApp(",", { "System Settings", "System Preferences" }) -- , = settings

-- Help
hs.hotkey.bind(MEH, "/", toggleHelp)
