# homebrew

macOS-only. CLI tools shared with Linux live in the mise manifest ([`mise/README.md`](../mise/README.md)), so brew holds only what mise can't do on a Mac: GUI casks and Homebrew's `python3` (`dotfiles-sync` needs 3.11+; Apple's is 3.9). rsync 3 for museum-sync comes from mise (`conda:rsync`, `mise-macos/`).

| File | Holds |
|---|---|
| `Brewfile.base` | what the setup needs on any Mac: python, Ghostty + fonts, Hammerspoon + Hyperkey, Caffeine, Chrome |
| `.Brewfile` → `~/.Brewfile` | includes `Brewfile.base`, then `~/.Brewfile.local` if it exists |
| `~/.Brewfile.local` (untracked) | this machine's apps, in Brewfile syntax |

A new app goes in `~/.Brewfile.local` unless every Mac running these dotfiles needs it, in which case it goes in `Brewfile.base`. A fresh or work Mac can install only the base with `brew bundle --file homebrew/Brewfile.base`.

node comes from mise on both OSes, so npm global packages (the nvim LSP servers, pi, mu, murmur, ...) install to `~/.local` with `npm i -g`, the same as on Linux. Brew has no node.

## Use

```bash
brew bundle --global              # install everything listed
brew bundle check --global        # anything listed but missing?
brew bundle cleanup --global      # list installed formulae/casks the Brewfile doesn't name
brew bundle cleanup --global --force   # and uninstall them
```

`cleanup` also reports a formula that has moved to mise and is still installed from brew. Uninstall the brew copy, otherwise it competes with mise's on `$PATH`.

## Moving a Mac from brew to mise

Run once on a Mac that installed its CLI tools from brew:

```bash
./dotfiles-sync --apply
mise/setup-mise.sh
brew bundle cleanup --global      # review: should be the brew copies of mise tools
brew bundle cleanup --global --force
cargo uninstall debrief ramble mdroots-cli   # now from mise
# npm globals: reinstall under mise's node (prefix ~/.local), then drop brew's node
npm ls -g --depth=0 --prefix /opt/homebrew   # note the list
npm i -g <those packages>
brew uninstall node && rm -rf /opt/homebrew/lib/node_modules
```
