# Bin

Small wrapper executables that should be available on `$PATH` even outside an
interactive shell live here under `.local/bin/`.

The toolbox-backed commands use `_toolbox_shim`, which first prefers a real
host binary and otherwise falls back to the toolbox container named by the
wrapper.
