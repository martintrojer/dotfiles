SHELL := /bin/bash
.SHELLFLAGS := -eu -o pipefail -c
.DEFAULT_GOAL := help

RG := rg --files --hidden -g '!.git' -g '!.jj'
FD := fd
RUFF_CONFIG := ruff.toml
TY_CONFIG := ty.toml
PRETTIER_CONFIG := .prettierrc.json
TSCONFIG := tsconfig.json

# Toolchain is pinned here, not inherited from whatever each machine happens to
# have installed: uv/npm are the only assumed binaries and both fetch-and-cache
# the exact pinned version, so `make check-all` behaves identically on every
# host and a toolchain bump is a reviewable one-line diff.
#
# luacheck stays a system binary: it is a Lua rock with no usable npm/PyPI
# distribution (the `luacheck` npm package is unrelated 2015 bindings).
PYTHON_VERSION := 3.14
RUFF_VERSION := 0.16.2
TY_VERSION := 0.0.70
SHELLCHECK_VERSION := 0.11.0.1
STYLUA_VERSION := 2.5.2
PRETTIER_VERSION := 3.9.6
TYPESCRIPT_VERSION := 7.0.2

PYTHON := uv run --python $(PYTHON_VERSION) --no-project --quiet python
RUFF := uvx --quiet ruff@$(RUFF_VERSION)
TY := uvx --quiet ty@$(TY_VERSION)
SHELLCHECK := uvx --quiet --from shellcheck-py==$(SHELLCHECK_VERSION) shellcheck
STYLUA := npx -y @johnnymorganz/stylua-bin@$(STYLUA_VERSION)
PRETTIER := npx -y prettier@$(PRETTIER_VERSION)
TSC := npx -y -p typescript@$(TYPESCRIPT_VERSION) tsc
PYTHON_PY_FILES_CMD := $(FD) --hidden --exclude .git --exclude .jj --exclude node_modules --type f --extension py --print0 .
PYTHON_SHEBANG_FILES_CMD := $(FD) --hidden --exclude .git --exclude .jj --exclude node_modules --type f '^[^.]+$$' . -X bash -lc 'for path in "$$@"; do IFS= read -r first < "$$path" || true; if [[ $$first =~ ^\#!.*python ]]; then printf "%s\\0" "$$path"; fi; done' bash
PYTHON_FILES_CMD := { $(PYTHON_PY_FILES_CMD); $(PYTHON_SHEBANG_FILES_CMD); }
SHELL_SH_FILES_CMD := $(FD) --hidden --exclude .git --exclude .jj --exclude node_modules --type f --extension sh --print0 .
SHELL_SHEBANG_FILES_CMD := $(FD) --hidden --exclude .git --exclude .jj --exclude node_modules --type f '^[^.]+$$' . -X bash -lc 'for path in "$$@"; do IFS= read -r first < "$$path" || true; if [[ $$first =~ ^\#!.*(ba|z|da|k)?sh([[:space:]]|$$) ]]; then printf "%s\\0" "$$path"; fi; done' bash
SHELL_FILES_CMD := { $(SHELL_SH_FILES_CMD); $(SHELL_SHEBANG_FILES_CMD); }
LUA_FILES := $(shell $(RG) -g '*.lua')
ZSH_FILES := $(shell $(RG) -g '*.zsh' -g '.zshrc')
# Vendored skills are kept byte-comparable against upstream so they stay easy
# to re-sync; reformatting their reference assets would fork them for nothing.
PRETTIER_FILES := $(shell $(RG) -g '*.ts' -g '*.json' -g '*.jsonc' -g '*.css' -g '!skills/**')
TMUX_STATUS_TEST := tmux/.config/tmux/scripts/test-status-tools
FEDORA_TEST_DIRS := fedora/tests fedora/gaming/tests
TS_TEST_GLOB := pi/.pi/agent/extensions/tests/*.test.ts
DESKTOP_TEST_DIRS := fuzzel/.config/fuzzel/scripts/tests sway/.config/sway/scripts/tests waybar/.config/waybar/scripts/tests

.PHONY: \
	help \
	check-all \
	format-all \
	check-python \
	check-shell \
	check-zsh \
	format-python \
	check-lua \
	format-lua \
	check-prettier \
	format-prettier \
	check-ts \
	check-ts-tests \
	format-ts \
	check-tmux-tests \
	check-fedora-tests \
	check-desktop-tests \
	build-guides \
	serve-guides \
	check-guides \
	clean-guides \
	theme \
	check-theme \
	tool-versions \
	push

help:
	printf '%s\n' \
	  'Targets:' \
	  '  make check-all         # python + shell + zsh + lua + prettier + ts + focused behavior tests' \
	  '  make tool-versions     # print the pinned toolchain versions' \
	  '  make format-all        # python + lua + prettier formatters' \
	  '  make check-python      # ruff check/format + ty + py_compile on all Python files/scripts' \
	  '  make format-python     # ruff format + safe autofixes' \
	  '  make check-shell       # shellcheck --severity=style on all sh/bash scripts' \
	  '  make check-zsh         # zsh -n syntax check on .zshrc and .zsh fragments' \
	  '  make check-lua         # stylua --check + luacheck' \
	  '  make format-lua        # stylua' \
	  '  make check-prettier    # prettier --check on ts/json/jsonc/css' \
	  '  make format-prettier   # prettier --write on ts/json/jsonc/css' \
	  '  make check-tmux-tests  # isolated tmux smoke tests' \
	  '  make check-fedora-tests # isolated Fedora and gaming helper behavior tests' \
	  '  make check-desktop-tests # focused fuzzel/sway/waybar script regression tests' \
	  '  make theme             # render docs/palette.toml into every THEME BEGIN..END region' \
	  '  make check-theme       # renderer behavior tests + theme regions in sync with docs/palette.toml' \
	  '  make check-ts          # tsc --noEmit on pi extensions + opencode plugin' \
	  '  make check-ts-tests    # node --test on the pi extension helper tests' \
	  '  make format-ts         # alias for format-prettier' \
	  '  make build-guides      # render guides/*.md → guides/build/*.html' \
	  '  make serve-guides      # build then http.server in guides/build' \
	  '  make check-guides      # validate guide sources without writing output' \
	  '  make clean-guides      # rm -rf guides/build' \
	  '  make push              # check-all, then jj git push (ARGS=... passed through)'

check-all: check-python check-shell check-zsh check-lua check-prettier check-ts check-ts-tests check-tmux-tests check-fedora-tests check-desktop-tests check-guides check-theme

format-all: format-python format-lua format-prettier

check-python:
	$(PYTHON_FILES_CMD) | xargs -0 $(RUFF) check --config $(RUFF_CONFIG)
	$(PYTHON_FILES_CMD) | xargs -0 $(RUFF) format --check --config $(RUFF_CONFIG)
	$(PYTHON_FILES_CMD) | xargs -0 $(TY) check --config-file $(TY_CONFIG)
	$(PYTHON_FILES_CMD) | xargs -0 $(PYTHON) -m py_compile

format-python:
	$(PYTHON_FILES_CMD) | xargs -0 $(RUFF) format --config $(RUFF_CONFIG)
	$(PYTHON_FILES_CMD) | xargs -0 $(RUFF) check --config $(RUFF_CONFIG) --fix

check-shell:
	$(SHELL_FILES_CMD) | xargs -0 $(SHELLCHECK) --severity=style

# zsh has no shellcheck equivalent; `zsh -n` still catches the parse errors that
# would break a login shell (tools.zsh is machine-edited by render_theme.py).
# Same .SHELLFLAGS -e dependency as check-fedora-tests below.
check-zsh:
	for file in $(ZSH_FILES); do zsh -n "$$file"; done

check-lua:
	$(STYLUA) --check $(LUA_FILES)
	luacheck $(LUA_FILES)

format-lua:
	$(STYLUA) $(LUA_FILES)

check-prettier:
	$(PRETTIER) --config $(PRETTIER_CONFIG) --check $(PRETTIER_FILES)

format-prettier:
	$(PRETTIER) --config $(PRETTIER_CONFIG) --write $(PRETTIER_FILES)

# No package.json here: the TS is loaded by globally installed hosts, so the
# type packages are symlinked into a scratch node_modules first (gitignored).
check-ts:
	$(PYTHON) _dotfiles_sync/link_ts_types.py
	$(TSC) -p $(TSCONFIG)

# No runner dependency: node strips the TS types itself, so `node --test` runs
# the *.test.ts files directly. The glob is quoted so node expands it (make/sh
# would fail the target if it ever matched nothing).
check-ts-tests:
	node --test '$(TS_TEST_GLOB)'

format-ts: format-prettier

check-tmux-tests:
	$(TMUX_STATUS_TEST)

# Relies on .SHELLFLAGS -e above: without it the loop would report only the
# last directory's status and a failure in an earlier suite would pass silently.
check-fedora-tests:
	for dir in $(FEDORA_TEST_DIRS); do $(PYTHON) -m unittest discover -s "$$dir" -p 'test_*.py'; done

# Same .SHELLFLAGS -e dependency as check-fedora-tests above.
check-desktop-tests:
	for dir in $(DESKTOP_TEST_DIRS); do $(PYTHON) -m unittest discover -s "$$dir" -p 'test_*.py'; done

build-guides:
	$(PYTHON) guides/build.py

serve-guides: build-guides
	$(PYTHON) -m http.server --directory guides/build 8000

check-guides:
	$(PYTHON) guides/build.py --check
	$(PYTHON) -m unittest discover -s guides -p 'test_*.py'

clean-guides:
	rm -rf guides/build

theme:
	$(PYTHON) _dotfiles_sync/render_theme.py --write

check-theme:
	$(PYTHON) _dotfiles_sync/render_theme.py --check
	$(PYTHON) -m unittest discover -s _dotfiles_sync/tests -p 'test_*.py'

tool-versions:
	printf '%s\n' \
	  'python      $(PYTHON_VERSION) (uv)' \
	  'ruff        $(RUFF_VERSION) (uvx)' \
	  'ty          $(TY_VERSION) (uvx)' \
	  'shellcheck  $(SHELLCHECK_VERSION) (uvx shellcheck-py)' \
	  'stylua      $(STYLUA_VERSION) (npx)' \
	  'prettier    $(PRETTIER_VERSION) (npx)' \
	  'typescript  $(TYPESCRIPT_VERSION) (npx)' \
	  'luacheck    system binary (no npm/PyPI distribution)'

# The repo's push gate. This is a make target rather than a pre-push hook
# because jj (0.42) has no hook point at all and `jj git push` does not run
# git's client-side hooks -- verified empirically, not read from docs -- and jj
# is the primary VCS here. A .git/hooks/pre-push would look like a gate while
# the command actually used (`jjgp`) sailed past it.
#
# check-all is a prerequisite, so make refuses to run the push at all unless it
# is green. Extra push args go through ARGS, e.g. `make push ARGS='-b main'`.
#
# TO BYPASS: run `jj git push` (or `jjgp`) directly. That is the deliberate
# escape hatch -- nothing here can intercept it, which is the point: the bypass
# is the plain command, the gate is the one you opt into.
push: check-all
	jj git push $(ARGS)
