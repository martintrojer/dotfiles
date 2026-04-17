# Zsh

## Setup
Install Oh My Zsh and the plugins used by this config:

1. `sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"`
2. `git clone --depth 1 https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions`
3. `git clone --depth 1 https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting`

If you want the `tm` helper and tmux session launcher flow to work, install `sesh` too.

## Plugin Highlights

- `git`: Git aliases and completions.
  - Examples: `gco` for `git checkout`, `gst` for `git status`, `gl` for pull, `gp` for push.
- `jj`: Jujutsu aliases and completions.
  - Examples: `jj st`, `jj log`, `jj diff`.
- Diff review wrappers in `.zsh/aliases.zsh`.
  - `gvd` runs `git difftool --dir-diff --no-prompt --extcmd=nvdiff`.
  - `jvd` runs `jj --no-pager diff --tool nvdiff`.
  - `hvd` runs `hg --config extensions.extdiff= extdiff -p nvdiff`.
  - `nvdiff` launches normal Neovim startup and opens a `:DiffTool` session.
- `mercurial`: Mercurial aliases and completions.
  - Examples: `hg st`, `hg pull`, `hg up`.
- `fzf`: fuzzy finder integration for shell workflows.
  - Example: use your local `F` global alias to turn `history F` or `rg foo F` into an interactive picker.
- `zoxide`: smarter directory jumping.
  - Examples: `z dotfiles`, `zi` for interactive jump selection.
- `zsh-autosuggestions`: suggests commands from history as you type.
  - Example: press Right Arrow in this config to accept the suggestion.
- `zsh-syntax-highlighting`: colors valid and invalid command syntax while typing.
- `eza`: modern file-listing aliases, with final alias overrides kept in `.zsh/overrides.zsh`.
  - Examples: `ls`, `ll`, `la`, `lt`, `l`.
- `sudo`: quick "fix the last command with sudo" workflow.
  - Example: `Esc Esc` prepends `sudo` to the current command line.
- `podman`: Podman aliases and completions.
  - Examples: `pc` for `podman container`, `pi` for `podman image`, `pp` for `podman pod`.
- `toolbox`: toolbox shortcuts.
  - Examples: `tbe dev` for `toolbox enter dev`, `tbr dev command` for `toolbox run dev command`.
- `ssh`: host-aware completion from `~/.ssh/config` for SSH-family commands, plus helpers like `ssh_rmhkey` for removing a host key entry.
- `systemd`: short aliases for `systemctl` and `systemctl --user`.
  - Examples: `sc-status sshd`, `sc-restart foo`, `scu-status waybar`, `scu-restart mako`.
- `tmux`: tmux aliases and completions for terminal workflow.
  - Examples: `ta` to attach, `tl` to list sessions, `tkss name` to kill a session by name.
- `rust`: Rust and Cargo completions/aliases.
  - Examples: `cb` for `cargo build`, `ct` for `cargo test`, `cr` for `cargo run`.
- `alias-finder`: helps discover existing aliases for a command.
  - Example: run `alias-finder git checkout` to see whether a shorter alias already exists.

## Tmux Helper

This config adds a small `tm` shell helper in `.zsh/overrides.zsh`.

- `tm` calls the shared `$HOME/.config/tmux/scripts/sesh-connect` picker script
- after selection, `sesh` handles attaching or creating the tmux session

## Boundary

Keep cross-platform shell behavior in shared files.
Use small OS checks for narrow environment differences.
Move larger platform-specific workflows into dedicated sourced files before the shared layer becomes noisy.
