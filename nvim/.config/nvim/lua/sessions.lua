local util = require("util")
local mini_sessions = require("mini.sessions")

local M = {}

local default_label = "default"
local session_dir = vim.fn.stdpath("state") .. "/sessions"
local session_root_markers = util.vcs_root_markers

local function normalize_path(path)
	local normalized = vim.fs.normalize(vim.fn.fnamemodify(path, ":p")):gsub("/$", "")
	return normalized
end

local function path_starts_with(path, parent)
	path = normalize_path(path)
	parent = normalize_path(parent)
	return path == parent or path:sub(1, #parent + 1) == parent .. "/"
end

local function short_path(path)
	local home = normalize_path(vim.fn.expand("~"))
	path = normalize_path(path)
	if path == home then
		return "~"
	end
	if path_starts_with(path, home) then
		return "~/" .. path:sub(#home + 2)
	end
	return path
end

local function safe_filename_component(value)
	return (value:gsub("[^%w._-]", "-")):gsub("-+", "-"):gsub("^-", ""):gsub("-$", "")
end

local function safe_label(label)
	label = label or default_label
	local safe = safe_filename_component(label)
	if safe == "" then
		return default_label
	end
	if safe ~= label then
		return string.format("%s--%s", safe, vim.fn.sha256(label):sub(1, 8))
	end
	return safe
end

local function root_id(root)
	root = normalize_path(root)
	local basename = vim.fn.fnamemodify(root, ":t")
	if basename == "" then
		basename = "root"
	end
	return string.format("%s--%s", safe_filename_component(basename), vim.fn.sha256(root):sub(1, 12))
end

local function session_name_for_root(root, label)
	return string.format("%s--%s.vim", root_id(root), safe_label(label))
end

local function current_session_name()
	local current = vim.v.this_session or ""
	if current == "" then
		return nil
	end
	return vim.fn.fnamemodify(current, ":t")
end

local function detected_session_files()
	vim.fn.mkdir(session_dir, "p")
	local files = {}
	for name in vim.fs.dir(session_dir) do
		local path = session_dir .. "/" .. name
		if vim.fn.filereadable(path) == 1 then
			files[name] = {
				modify_time = vim.fn.getftime(path),
				name = name,
				path = path,
			}
		end
	end
	return files
end

local function session_exists(name)
	return detected_session_files()[name] ~= nil
end

local function notify_missing(label)
	vim.notify(("No session '%s' for %s"):format(safe_label(label), short_path(M.root())), vim.log.levels.INFO)
end

local function label_from_name(name, root)
	local prefix = root_id(root) .. "--"
	if name:sub(1, #prefix) ~= prefix or name:sub(-4) ~= ".vim" then
		return nil
	end
	return name:sub(#prefix + 1, -5)
end

function M.root()
	local cwd = normalize_path(vim.fn.getcwd())
	local vcs_root = vim.fs.root(cwd, session_root_markers)
	if vcs_root then
		return normalize_path(vcs_root)
	end

	local home = normalize_path(vim.fn.expand("~"))
	if path_starts_with(cwd, home) then
		return home
	end
	return cwd
end

function M.current_name(label)
	return session_name_for_root(M.root(), label)
end

function M.status()
	return (vim.v.this_session or "") ~= "" and "%#MiniStatuslineSession#󰆓%#MiniStatuslineDevinfo#" or ""
end

function M.setup()
	-- Keep session files out of project directories. `file = ""` disables
	-- mini.sessions' local `Session.vim` detection/writes entirely.
	mini_sessions.setup({
		autoread = false,
		autowrite = true,
		directory = session_dir,
		file = "",
		verbose = { read = true, write = true, delete = true },
	})

	-- Avoid restoring throwaway UI buffers/terminal jobs. mini.sessions delegates
	-- serialization to :mksession, so this controls what is persisted.
	vim.o.sessionoptions = "buffers,curdir,folds,tabpages,winsize"

	vim.api.nvim_create_user_command("SessionSave", function(opts)
		M.save(opts.args ~= "" and opts.args or nil)
	end, { nargs = "?", complete = M.complete, desc = "Save current project session label" })

	vim.api.nvim_create_user_command("SessionLoad", function(opts)
		M.load(opts.args ~= "" and opts.args or nil)
	end, { nargs = "?", complete = M.complete, desc = "Load current project session label" })

	vim.api.nvim_create_user_command("SessionPick", M.pick, { desc = "Pick a current-project session" })

	vim.api.nvim_create_user_command("SessionDelete", function(opts)
		M.delete(opts.args ~= "" and opts.args or nil)
	end, { nargs = "?", complete = M.complete, desc = "Delete current project session label" })
end

function M.save(label)
	mini_sessions.write(M.current_name(label), { force = true })
end

function M.load(label)
	local name = M.current_name(label)
	if not session_exists(name) then
		notify_missing(label)
		return
	end
	mini_sessions.read(name)
end

function M.delete(label)
	if label then
		local name = M.current_name(label)
		if not session_exists(name) then
			notify_missing(label)
			return
		end
		mini_sessions.delete(name, { force = true })
		return
	end

	local active = current_session_name()
	if active and session_exists(active) then
		mini_sessions.delete(active, { force = true })
		return
	end

	local default = M.current_name()
	if not session_exists(default) then
		notify_missing(default_label)
		return
	end
	mini_sessions.delete(default, { force = true })
end

local function session_items()
	local root = M.root()
	local items = {}
	for name, data in pairs(detected_session_files()) do
		local label = label_from_name(name, root)
		if label then
			local current = name == current_session_name() and "*" or " "
			table.insert(items, {
				name = name,
				label = label,
				mtime = data.modify_time or 0,
				display = string.format("%s %-24s %s", current, label, short_path(root)),
			})
		end
	end
	table.sort(items, function(a, b)
		if a.label == b.label then
			return a.mtime > b.mtime
		end
		if a.label == default_label then
			return true
		end
		if b.label == default_label then
			return false
		end
		if a.mtime == b.mtime then
			return a.label < b.label
		end
		return a.mtime > b.mtime
	end)
	return items
end

local function picker_contents()
	return vim.tbl_map(function(item)
		return item.name .. "\t" .. item.display
	end, session_items())
end

local function selected_name(selected)
	local entry = selected and selected[1]
	if not entry then
		return nil
	end
	return entry:match("^([^\t]+)")
end

function M.pick()
	local contents = picker_contents()
	if #contents == 0 then
		vim.notify("No sessions for " .. short_path(M.root()), vim.log.levels.INFO)
		return
	end

	require("fzf-lua").fzf_exec(contents, {
		prompt = "Sessions❯ ",
		header = "ctrl-d: delete",
		previewer = false,
		fzf_opts = {
			["--delimiter"] = "\t",
			["--with-nth"] = "2..",
			["--nth"] = "2..",
		},
		actions = {
			["enter"] = function(selected)
				local name = selected_name(selected)
				if name then
					mini_sessions.read(name)
				end
			end,
			["ctrl-d"] = function(selected)
				local name = selected_name(selected)
				if name then
					mini_sessions.delete(name, { force = true })
					vim.schedule(M.pick)
				end
			end,
		},
	})
end

function M.complete(arg_lead)
	return vim.tbl_filter(
		function(item)
			return item.label:sub(1, #arg_lead) == arg_lead
		end,
		vim.tbl_map(function(item)
			return item.label
		end, session_items())
	)
end

return M
