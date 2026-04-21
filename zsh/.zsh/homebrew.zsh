# Ensure common Homebrew locations are on PATH before OMZ plugins load.
# Use a cheap direct PATH prepend instead of `brew shellenv` during startup.

if [[ "$OSTYPE" == darwin* ]]; then
  if [ -d "/opt/homebrew/bin" ]; then
    path=(/opt/homebrew/bin /opt/homebrew/sbin $path)
  elif [ -d "/usr/local/bin" ]; then
    path=(/usr/local/bin /usr/local/sbin $path)
  fi
fi
