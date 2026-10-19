# mise

mise installs the userland CLI tools on Linux and macOS from one shared list. Homebrew on macOS only covers what mise can't (see [`homebrew/README.md`](../homebrew/README.md)).

| File | Package | Linked on | Holds |
|---|---|---|---|
| `mise/.config/mise/config.toml` | `mise` | both | tools used on both OSes |
| `mise-linux/.config/mise/conf.d/linux.toml` | `mise-linux` | Linux | Linux-only by design (bluetui, systemctl-tui) or used only on Linux |
| `mise-macos/.config/mise/conf.d/macos.toml` | `mise-macos` | macOS | Mac-only tools the tracked config needs |
| `~/.config/mise/conf.d/local.toml` (untracked) | — | that machine | per-machine extras |

mise reads `~/.config/mise/conf.d/*.toml` as part of the global config, so each OS gets the shared list plus its own extras. The OS split sits in which package links, not in conditions inside a shared file.

## Where a new tool goes

1. Used on both OSes and mise has a build for both (`mise registry <tool>`, then check the release assets) → `config.toml`.
2. Used on one OS only and needed on every machine of that OS → that OS's `conf.d` file.
3. Only on this machine → `~/.config/mise/conf.d/local.toml`, outside the repo.
4. macOS-only and mise can't install it, or it's a GUI app → `homebrew/` (see its README).

Edit the files in the repo. `mise use -g` writes into `config.toml` through the symlink, which is how an OS-specific tool ends up in the shared list.

## Install

```bash
./dotfiles-sync --apply      # links config.toml and this OS's conf.d file
mise/setup-mise.sh           # bootstraps mise if absent, installs every listed tool, then the Luacheck rock
```

The script refuses to run until `~/.config/mise/config.toml` links into this repo: mise reads `conf.d/` only beside the real global config, so installing from a repo path would skip the OS extras. Repeated runs are safe. Shell activation lives in `zsh/.zsh/tools.zsh`. To verify the mise bootstrap before running it, follow the upstream [GPG instructions](https://mise.jdx.dev/installing-mise.html), then rerun the script.

## Updates and drift

Tools follow `latest` unless pinned (lua 5.4 and node 26, reasons in the file), so the list records which tools a host needs, not their versions.

- `mise outdated` / `mise upgrade`: review and apply updates.
- `mise ls --global --missing`: listed tools that aren't installed.
- `mise ls --prunable` then `mise prune --tools`: remove installed versions nothing lists. The script never prunes.
