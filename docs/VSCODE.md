# VS Code / Cursor / Code OSS notes

VS Code is no longer a stowed package in this repo. The settings were small,
manual, and not worth a dedicated top-level pseudo-package.

Apply these by hand in your editor's user settings if you still care about
them:

```json
{
  "dev.containers.dockerPath": "podman",
  "editor.cursorBlinking": "solid",
  "editor.fontFamily": "'JetBrainsMono NF'",
  "editor.minimap.enabled": false,
  "git.autofetch": true,
  "github.copilot.nextEditSuggestions.enabled": true,
  "terminal.integrated.sendKeybindingsToShell": true,
  "vim.normalModeKeyBindings": [
    {
      "before": ["u"],
      "commands": ["undo"]
    },
    {
      "before": ["U"],
      "commands": ["redo"]
    },
    {
      "before": ["<leader>", "u"],
      "after": ["u"]
    },
    {
      "before": ["<leader>", "U"],
      "commands": ["redo"]
    }
  ],
  "window.commandCenter": true,
  "window.zoomLevel": 0.75
}
```

If a Flatpak-hosted editor needs a Podman wrapper similar to the old
`vscode/podman-host`, create a helper on `$PATH` with:

```sh
#!/bin/sh
exec flatpak-spawn --host podman "$@"
```
