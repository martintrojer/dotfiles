# Linked to ~/.Brewfile; apply with `brew bundle --global`.
# The base set every Mac gets lives in homebrew/Brewfile.base, pulled in here
# (realpath: brew evaluates this through the ~/.Brewfile symlink). Per-machine
# apps go in an untracked ~/.Brewfile.local, included when it exists.
instance_eval(File.read(File.join(File.dirname(File.realpath(__FILE__)), "Brewfile.base")))

local_brewfile = File.expand_path("~/.Brewfile.local")
instance_eval(File.read(local_brewfile)) if File.exist?(local_brewfile)
