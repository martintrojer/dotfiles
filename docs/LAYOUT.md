# Layout Language

The desktop should feel like one terminal-native system rather than a pile of
matching themes. The shared visual grammar is borrowed from tmux, but not every
surface should pretend to be a tmux status line.

## Core rule

**Blocks are affordances, not decoration.**

A filled rectangular cell means one of:

- navigation/place (`tmux` session/window, Waybar workspaces)
- active selection/focus
- an actionable state that needs attention
- a modal/control surface such as a picker, popup, or notification

Ambient context should stay quieter: plain text/icons, transparent background,
or muted foreground. If everything is boxed, boxes stop meaning anything.

## Geometry

Keep the geometry boring and terminal-like:

- square corners, no pills
- thin hard borders where a surface needs containment
- small gaps and compact padding
- monospace where text lives next to terminal content
- no blur/glass/shadow as a primary design element

Rounded corners are reserved for places where the toolkit forces the issue or
where a temporary overlay genuinely benefits from separation. They are not the
default language.

## Color and weight

Catppuccin Mocha is the palette, but the important part is restraint:

- `base` / transparent: page/background
- `surface0`: normal filled cell
- `surface1`: secondary filled cell or contained cluster
- `surface2`: current/selected/focused block
- `overlay0` / subtext: inactive or ambient text
- `lavender`: focus/accent
- `red`: critical/problem state

Prefer foreground changes for mild warnings. Use filled red backgrounds or
blinking only for states that should interrupt.

## Typography

Fonts follow the same structural rule: pick the face for the role, not because a
new surface wants novelty.

- Foot uses Hack Nerd Font Mono: it is the primary Linux terminal face.
- Waybar uses SF Compact for ambient chrome such as the clock and window title,
  with JetBrains Mono Nerd Font as the fallback and as the forced face for
  workspaces, issue blocks, notification blocks, and Nerd-heavy status cells.
- Temporary desktop control surfaces (`fuzzel`, Mako, Sway title text) keep
  JetBrains Mono Nerd Font. Its heavier, high-legibility shape works better for
  scanning popups/notifications than the calmer Waybar ambient face.
- VS Code/Cursor may also keep JetBrains; editor font choice is outside the
  Linux desktop chrome contract.
- Dense terminal working surfaces prefer monospace. This includes terminals,
  tmux status, code/editor UI, and status text that sits next to terminal
  content.
- Proportional fonts are acceptable for document/content surfaces, not for
  status cells whose job is alignment, compactness, and predictable icon spacing.
- Nerd Font glyphs are allowed as compact labels, but icons should not become a
  second visual language. If an icon needs a tooltip to be understood, it should
  probably be paired with text or appear only in a familiar context.

Prefer fewer font families over perfect per-tool aesthetics. A calm mismatch is
better than novelty, but a shared font family is better still when the surface is
part of the same workflow.

## Theme adherence

Theme adherence is structural, not ornamental. A new UI surface should not just
borrow Catppuccin hex values; it should use the same roles as the rest of the
system.

- Use palette roles consistently: `base` for background, `surface*` for cells
  and contained surfaces, `overlay*`/subtext for inactive context, `lavender` for
  focus, `red` for problems.
- Do not introduce one-off colors, gradients, shadows, transparency tricks, or
  rounded shapes to make a single tool feel special.
- If a tool needs generated colors, add it to the renderer described in
  [`THEME.md`](./THEME.md) rather than hand-copying hex values across files.
- If a tool cannot express the layout grammar exactly, preserve the hierarchy:
  focus beats decoration, warnings beat normal state, ambient context stays
  quiet.

A tool is on-theme only when its structure matches the shared language. Matching
hex codes alone is not enough.

## Waybar

Waybar is not a cockpit. It is an interruption filter.

Default shape:

- workspaces are blocks because they are navigation objects
- window title is ambient context
- clock and tray are ambient context
- notifications are hidden when empty, blocked when actionable
- issues are hidden when healthy, blocked when attention/action is needed

Interaction rule:

- left click opens the relevant TUI/control surface
- right click performs the obvious safe fix, if one exists
- tooltip carries the nerd details

This keeps power-user depth without making every metric visible all the time.

## Tmux

Tmux is the source grammar, but even inside tmux not every segment needs maximum
chrome.

- session/current window are strong blocks: they define place
- inactive windows are flatter: they remain switch targets without shouting
- `PREFIX`/agent-attention are blocks because they are modal/action states
- CPU/RAM/host/uptime are quieter unless thresholds make them important
- popups, choose-tree, menus, and prompts are bordered rectangular surfaces

Tmux can be denser than Waybar because tmux is the working surface. Waybar is
outer chrome and should be quieter.

## Sway, fuzzel, mako

- Sway borders mark focus and tree structure; they should be crisp, not ornate.
- Fuzzel is a temporary command surface, so a rectangular bordered box fits.
- Mako notifications are temporary interruption surfaces; square borders and
  severity color are enough.

## Test for new UI changes

Before adding a block, badge, color, or always-visible module, ask:

1. Is this navigation, focus, modal state, or a problem?
2. Can I click it or act on it?
3. Should it be visible when everything is normal?
4. Would a tooltip, keybind, or picker preserve the power-user path more calmly?

If the answer is mostly no, make it ambient or hide it until needed.
