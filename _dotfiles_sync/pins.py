from __future__ import annotations

from pathlib import Path
from typing import Final

# Zsh plugins managed by clone-on-apply. Same model as nvim's vim.pack:
# pinned commit refs live here, .zshrc sources the checked-out plugin trees
# directly, and updates are "bump ref, re-run dotfiles-sync --apply".
ZSH_PLUGINS: Final[tuple[tuple[str, str, str], ...]] = (
    (
        "zsh-autosuggestions",
        "https://github.com/zsh-users/zsh-autosuggestions",
        "v0.7.1",
    ),
    (
        "zsh-syntax-highlighting",
        "https://github.com/zsh-users/zsh-syntax-highlighting",
        "0.8.0",
    ),
)
ZSH_PLUGINS_DEST: Final[Path] = Path(".local/share/zsh-plugins")

# TPM (tmux plugin manager). dotfiles-sync only bootstraps TPM itself; TPM owns
# the lifecycle of the @plugin entries in tmux/.tmux.conf.
TPM: Final[tuple[str, str, str]] = (
    "tpm",
    "https://github.com/tmux-plugins/tpm",
    "v3.1.0",
)
TPM_DEST: Final[Path] = Path(".tmux/plugins/tpm")

# mu-crew/dotfiles: the tmux wiring shared by mu, murmur and mule (focus hooks,
# picker/dash/side-panel/workstream keys, agent glyph and pill formats).
# .tmux.conf sources tmux/mu-crew.conf from this clone. Same model as the zsh
# plugins: bump the ref and re-run dotfiles-sync --apply.
MU_CREW_DOTFILES: Final[tuple[str, str, str]] = (
    "mu-crew-dotfiles",
    "https://github.com/mu-crew/dotfiles",
    "cf02f0ae385ca591727e3f57950bb6a2bee8c555",
)
MU_CREW_DOTFILES_DEST: Final[Path] = Path(".local/share/mu-crew-dotfiles")
