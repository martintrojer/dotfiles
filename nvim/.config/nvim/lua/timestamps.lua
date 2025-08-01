local M = {}

function M.get_start_time()
  return vim.g.timestamp_start
end

function M.set_start_time()
  vim.g.timestamp_start = vim.fn.localtime()
end

function M.get_counter()
  return vim.g.timestamp_counter
end

function M.set_counter(val)
  vim.g.timestamp_counter = val
end

function M.reset()
  M.set_start_time()
  M.set_counter(1)
  print("timestamp and counter reset")
end

function M.disable_counter()
  M.set_counter(nil)
  print("counter disabled")
end

function M.get_ts_string()
  if M.get_start_time() == nil then
    M.set_start_time()
    return nil
  end
  local elapsed = vim.fn.localtime() - M.get_start_time()
  local counter = M.get_counter()
  local counterstr = nil
  if counter then
    counterstr = string.format("[%d]", counter)
    M.set_counter(counter + 1)
  end
  local timestr = nil
  if elapsed < 60 then
    timestr = string.format("%ds", elapsed)
  elseif elapsed < 60 * 60 then
    timestr = string.format("%dm", elapsed / 60)
  else
    timestr = string.format("%dh", elapsed / 60 / 60)
  end
  local fullstr = counterstr
  if fullstr then
    fullstr = fullstr .. " " .. timestr
  else
    fullstr = timestr
  end
  local title = vim.fn.input({ prompt = "Title: ", default = nil })
  if title then
    fullstr = fullstr .. " " .. title
  end
  return fullstr
end

return M
