#!/bin/bash
set -euo pipefail
exec "$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd)/setup-t3code" "$@"
