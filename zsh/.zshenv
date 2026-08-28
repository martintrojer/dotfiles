# Read by EVERY zsh, including non-interactive ones. .zshrc is not: it is
# interactive-only, so `ssh host node -v` finds nothing even when mise has
# node installed. That breaks any tool that shells out over ssh — murmur's
# collector runs `ssh <target> murmur export` with no tty and no login shell.
#
# mise ships shims for exactly this case: a directory of thin wrappers that
# resolve the active version without needing `mise activate`. Putting them on
# PATH here is enough for non-interactive use, and harmless for interactive
# shells, where .zshrc's `mise activate` later takes precedence.
#
# Keep this file minimal. It runs for every zsh invocation, including scripts,
# so anything slow here is paid constantly. No `eval "$(mise activate)"`: that
# is what .zshrc is for.

if [ -d "$HOME/.local/share/mise/shims" ]; then
  case ":$PATH:" in
    *":$HOME/.local/share/mise/shims:"*) ;;
    *) export PATH="$HOME/.local/share/mise/shims:$PATH" ;;
  esac
fi

# Locally installed binaries (npm -g prefix, cargo, pipx, mise itself).
if [ -d "$HOME/.local/bin" ]; then
  case ":$PATH:" in
    *":$HOME/.local/bin:"*) ;;
    *) export PATH="$HOME/.local/bin:$PATH" ;;
  esac
fi
