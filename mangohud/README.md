# MangoHud

Stow package for the [MangoHud](https://github.com/flightlessmango/MangoHud)
Vulkan/OpenGL performance overlay used with the Steam/gaming layer.

- Config: `mangohud/.config/MangoHud/MangoHud.conf` → `~/.config/MangoHud/MangoHud.conf`
- Installed via `fedora/steam-packages.sh` (the `mangohud` package).
- Enable per-game by adding `mangohud %command%` to a title's Steam launch
  options, or run `mangohud <program>` directly.
- Default in-overlay toggle is `Shift_R+F12`.
- In the Steam (gamescope) session, gamescope draws this config with
  `--mangoapp`. The `gamemode` widget is intentionally disabled there: `optirun`
  applies GameMode to the game process, but mangoapp is session-level and would
  report its own context instead.
