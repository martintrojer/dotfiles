SHELL := /bin/bash
.SHELLFLAGS := -eu -o pipefail -c
.DEFAULT_GOAL := help

RG := rg --files --hidden -g '!.git' -g '!.jj'
FD := fd
RUFF_CONFIG := ruff.toml
TY_CONFIG := ty.toml
PRETTIER_CONFIG := .prettierrc.json
PYTHON_PY_FILES_CMD := $(FD) --hidden --exclude .git --exclude .jj --exclude node_modules --type f --extension py --print0 .
PYTHON_SHEBANG_FILES_CMD := $(FD) --hidden --exclude .git --exclude .jj --exclude node_modules --type f '^[^.]+$$' . -X bash -lc 'for path in "$$@"; do IFS= read -r first < "$$path" || true; if [[ $$first =~ ^\#!.*python ]]; then printf "%s\\0" "$$path"; fi; done' bash
PYTHON_FILES_CMD := { $(PYTHON_PY_FILES_CMD); $(PYTHON_SHEBANG_FILES_CMD); }
LUA_FILES := $(shell $(RG) -g '*.lua')
PRETTIER_FILES := $(shell $(RG) -g '*.ts' -g '*.json' -g '*.jsonc' -g '*.css')
TMUX_STATUS_TEST := tmux/.config/tmux/scripts/test-status-tools

.PHONY: \
	help \
	check-all \
	format-all \
	check-python \
	format-python \
	check-lua \
	format-lua \
	check-prettier \
	format-prettier \
	check-ts \
	format-ts \
	check-tmux-tests \
	build-guides \
	serve-guides \
	check-guides \
	clean-guides

help:
	printf '%s\n' \
	  'Targets:' \
	  '  make check-all         # python + lua + prettier + tmux script tests' \
	  '  make format-all        # python + lua + prettier formatters' \
	  '  make check-python      # ruff + ty + py_compile on all Python files/scripts' \
	  '  make format-python     # ruff format + safe autofixes' \
	  '  make check-lua         # stylua --check + luacheck' \
	  '  make format-lua        # stylua' \
	  '  make check-prettier    # prettier --check on ts/json/jsonc/css' \
	  '  make format-prettier   # prettier --write on ts/json/jsonc/css' \
	  '  make check-tmux-tests  # isolated tmux smoke tests' \
	  '  make check-ts          # alias for check-prettier' \
	  '  make format-ts         # alias for format-prettier' \
	  '  make build-guides      # render guides/*.md → guides/build/*.html' \
	  '  make serve-guides      # build then http.server in guides/build' \
	  '  make check-guides      # validate guide sources without writing output' \
	  '  make clean-guides      # rm -rf guides/build'

check-all: check-python check-lua check-prettier check-tmux-tests check-guides

format-all: format-python format-lua format-prettier

check-python:
	$(PYTHON_FILES_CMD) | xargs -0 ruff check --config $(RUFF_CONFIG)
	$(PYTHON_FILES_CMD) | xargs -0 ty check --config-file $(TY_CONFIG)
	$(PYTHON_FILES_CMD) | xargs -0 python3 -m py_compile

format-python:
	$(PYTHON_FILES_CMD) | xargs -0 ruff format --config $(RUFF_CONFIG)
	$(PYTHON_FILES_CMD) | xargs -0 ruff check --config $(RUFF_CONFIG) --fix

check-lua:
	stylua --check $(LUA_FILES)
	luacheck $(LUA_FILES)

format-lua:
	stylua $(LUA_FILES)

check-prettier:
	prettier --config $(PRETTIER_CONFIG) --check $(PRETTIER_FILES)

format-prettier:
	prettier --config $(PRETTIER_CONFIG) --write $(PRETTIER_FILES)

check-ts: check-prettier

format-ts: format-prettier

check-tmux-tests:
	$(TMUX_STATUS_TEST)

build-guides:
	python3 guides/build.py

serve-guides: build-guides
	python3 -m http.server --directory guides/build 8000

check-guides:
	python3 guides/build.py --check

clean-guides:
	rm -rf guides/build
