# VS Code / Cursor / Code OSS notes

VS Code is no longer a package in this repo. The few manual settings do not
justify a top-level package.

Add them to the editor's user settings if needed:

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
