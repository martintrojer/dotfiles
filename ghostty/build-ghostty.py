#!/usr/bin/env python3
"""Build a tagged Ghostty release from source and install it under ~/.local.

Usage::

    ghostty/build-ghostty.py 1.4.0      # one release tag
    ghostty/build-ghostty.py --latest   # newest release tag on GitHub

Fedora has no Ghostty package and its COPRs have blocked Atomic rebases, so the
Linux binary is built here instead. Only signed release tarballs are built:
the tarball's minisign signature is checked against upstream's key and the Zig
compiler's sha256 against ziglang.org's index. Git checkouts and tip are
refused.

The build runs in a Fedora toolbox matching the host release, so the -devel
headers never touch the host. The result lands in ~/.local/opt/ghostty-<ver>
with ~/.local/bin/ghostty pointing at it. gtk4-layer-shell is copied into the
prefix and found via RPATH, because Sericea ships the library only for gtk3.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import shutil
import subprocess
import sys
import tarfile
import tempfile
import urllib.request
from pathlib import Path
from typing import NoReturn

MINISIGN_KEY = "RWQlAjJC23149WL2sEpT/l0QKy7hMIFhYdQOFy0Z7z7PbneUgvlsnYcV"
RELEASES = "https://release.files.ghostty.org"
LATEST_API = "https://api.github.com/repos/ghostty-org/ghostty/releases/latest"
ZIG_INDEX = "https://ziglang.org/download/index.json"
TOOLBOX = "ghostty-build"
BUILD_PACKAGES = (
    "gtk4-devel",
    "libadwaita-devel",
    "gtk4-layer-shell-devel",
    "libglvnd-devel",
    "gettext",
    "pkgconf",
    "minisign",
    "patchelf",
)

HOME = Path.home()
CACHE = HOME / ".cache" / "ghostty-build"
OPT = HOME / ".local" / "opt"
BIN_LINK = HOME / ".local" / "bin" / "ghostty"
VERSION_RE = re.compile(r"^\d+\.\d+\.\d+$")


def die(msg: str) -> NoReturn:
    sys.exit(f"build-ghostty: {msg}")


def fetch(url: str) -> bytes:
    with urllib.request.urlopen(url, timeout=60) as resp:
        return resp.read()


def download(url: str, dest: Path) -> Path:
    if not dest.exists():
        print(f"fetch {url}")
        tmp = dest.with_suffix(dest.suffix + ".part")
        tmp.write_bytes(fetch(url))
        tmp.rename(dest)
    return dest


def in_toolbox(*cmd: str | Path, cwd: Path | None = None) -> None:
    subprocess.run(
        ["toolbox", "run", "-c", TOOLBOX, *map(str, cmd)], check=True, cwd=cwd
    )


def ensure_toolbox() -> None:
    names = subprocess.run(
        ["podman", "ps", "-a", "--format", "{{.Names}}"],
        check=True,
        capture_output=True,
        text=True,
    ).stdout.split()
    if TOOLBOX not in names:
        subprocess.run(["toolbox", "create", "-y", TOOLBOX], check=True)
    in_toolbox("sudo", "dnf", "-yq", "install", *BUILD_PACKAGES)


def latest_version() -> str:
    tag = json.loads(fetch(LATEST_API))["tag_name"]
    return tag.removeprefix("v")


def fetch_source(version: str) -> Path:
    name = f"ghostty-{version}.tar.gz"
    tarball = download(f"{RELEASES}/{version}/{name}", CACHE / name)
    sig = download(f"{RELEASES}/{version}/{name}.minisig", CACHE / f"{name}.minisig")
    in_toolbox("minisign", "-Vm", tarball, "-x", sig, "-P", MINISIGN_KEY)
    src = CACHE / f"ghostty-{version}"
    shutil.rmtree(src, ignore_errors=True)
    with tarfile.open(tarball) as tar:
        tar.extractall(CACHE, filter="data")
    return src


def fetch_zig(src: Path) -> Path:
    zon = (src / "build.zig.zon").read_text()
    match = re.search(r'\.minimum_zig_version\s*=\s*"([^"]+)"', zon)
    if match is None:
        die("build.zig.zon has no minimum_zig_version")
    zig_version = match.group(1)
    entry = json.loads(fetch(ZIG_INDEX))[zig_version]["x86_64-linux"]
    archive = download(entry["tarball"], CACHE / Path(entry["tarball"]).name)
    if hashlib.sha256(archive.read_bytes()).hexdigest() != entry["shasum"]:
        archive.unlink()
        die(f"zig {zig_version} sha256 mismatch")
    zig_dir = CACHE / f"zig-x86_64-linux-{zig_version}"
    if not zig_dir.exists():
        with tarfile.open(archive) as tar:
            tar.extractall(CACHE, filter="data")
    return zig_dir / "zig"


def build(version: str, src: Path, zig: Path) -> Path:
    prefix = OPT / f"ghostty-{version}"
    # Stage, then swap in, so a failed build never leaves a half prefix live.
    stage = Path(tempfile.mkdtemp(prefix=f"ghostty-{version}.", dir=OPT))
    try:
        in_toolbox(
            zig,
            "build",
            "-Doptimize=ReleaseFast",
            f"-Dversion-string={version}",
            "-Dpatch-rpath=$ORIGIN/../lib",
            "-p",
            stage,
            cwd=src,
        )
        (stage / "lib").mkdir(exist_ok=True)
        in_toolbox("cp", "-L", "/usr/lib64/libgtk4-layer-shell.so.0", stage / "lib")
        shutil.rmtree(prefix, ignore_errors=True)
        stage.rename(prefix)
    except BaseException:
        shutil.rmtree(stage, ignore_errors=True)
        raise
    return prefix


def install(prefix: Path) -> None:
    binary = prefix / "bin" / "ghostty"
    out = subprocess.run(
        [binary, "+version"], check=True, capture_output=True, text=True
    ).stdout
    if "channel: stable" not in out:
        die(f"{binary} does not report the stable channel:\n{out}")
    BIN_LINK.parent.mkdir(parents=True, exist_ok=True)
    tmp = BIN_LINK.with_name(".ghostty.new")
    tmp.unlink(missing_ok=True)
    tmp.symlink_to(binary)
    tmp.replace(BIN_LINK)
    print(out.splitlines()[0], "->", BIN_LINK)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    which = parser.add_mutually_exclusive_group(required=True)
    which.add_argument("version", nargs="?", help="release tag, e.g. 1.4.0")
    which.add_argument("--latest", action="store_true", help="newest release tag")
    args = parser.parse_args()

    version = latest_version() if args.latest else args.version
    if not VERSION_RE.match(version):
        die(f"{version!r} is not a release tag (want X.Y.Z)")

    CACHE.mkdir(parents=True, exist_ok=True)
    OPT.mkdir(parents=True, exist_ok=True)
    ensure_toolbox()
    src = fetch_source(version)
    install(build(version, src, fetch_zig(src)))
    print(f"old prefixes stay in {OPT}; remove them by hand")


if __name__ == "__main__":
    main()
