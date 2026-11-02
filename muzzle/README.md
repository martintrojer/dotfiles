# muzzle

Config for [muzzle](https://github.com/mu-crew/muzzle), the per-host admission queue for heavy commands.

- `.config/muzzle/config.toml` is shared by every host: the rules, the path tables, and the default `reserve = 2`. Weights are tuned on the main workstation. Path tables for repos a host lacks do nothing.
- `.config/muzzle/hosts/<host>.toml` holds only what differs per host, with no hardware details (public repo): `reserve`, and `[[rule]]` or `[path]` entries that name an existing rule to reweight it.
- config.toml includes `machine.toml`. That file is a local, untracked symlink to this host's file, made once per host:

  ```sh
  ln -sfn hosts/$(hostname -s).toml ~/.config/muzzle/machine.toml
  ```

  A host with no file (or no symlink) runs on the shared config; muzzle skips a missing include.
- muzzle's own state and run log live in `~/.local/state/muzzle`, not here.

Check with `muzzle doctor` (config hash, include found) and `muzzle match -- '<command>'`. Tune from `muzzle report`; see the muzzle skill's `recipes/tune.md`.
