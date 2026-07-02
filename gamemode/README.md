# GameMode

Stow package for Feral GameMode.

- Config: `gamemode/.config/gamemode.ini` → `~/.config/gamemode.ini`.
- GameMode reads `$XDG_CONFIG_HOME/gamemode.ini`, not
  `~/.config/gamemode/gamemode.ini`.
- Install package via `fedora/os/steam-packages.sh`.
- Use directly with `gamemoderun %command%`, or use `optirun %command%` for the
  OptiScaler/FSR4/GameMode bundle.

## Local policy

Enabled in `gamemode.ini`:

- `renice=10` — game process nice `-10`.
- `ioprio=0` — best-effort I/O priority 0.
- custom hooks:
  - start: `tuned-adm profile throughput-performance`
  - end: `tuned-adm profile balanced`

`tuned-adm` is used because tuned owns CPU governor policy on this host.
GameMode's native `desiredgov`/`desiredprof` are left unset.

Hooks run as the user and intentionally do not use `sudo`; polkit authorizes the
active local session over D-Bus.

## gamemode group on Fedora Atomic/Silverblue

`renice=10` needs `gamemode` group membership. On rpm-ostree systems the group
may exist in `/usr/lib/group` but not `/etc/group`, so copy it before `usermod`:

```bash
grep -E '^gamemode:' /usr/lib/group | sudo tee -a /etc/group
sudo usermod -aG gamemode "$USER"
systemctl reboot
```

## Verify

```bash
gamemoded -t
gamemoded -s   # while a game is running
```

## Caveat

`[gpu]` settings such as `amd_performance_level` are ignored from the user-local
config.
