---
name: catppuccin
description: Exact Catppuccin palette (Latte, Frappé, Macchiato, Mocha) plus the rules for which color goes on which UI element. Use when theming a project, site, app, terminal, editor, chart or doc with Catppuccin, or when the user mentions Catppuccin, a flavor name, or "my theme".
---

# Catppuccin theme context

Use Catppuccin's named colors, never invented hex values. Every value below is from the official
palette v1.8.0 (`@catppuccin/palette`). Flavors: **Latte** is the only light one (dark text on a
light background, `color-scheme: light`). **Frappé**, **Macchiato** and **Mocha** are dark and get
darker in that order (`color-scheme: dark`). Default pair: Latte for light mode, Mocha for dark mode.

## Palette

| Color | Latte | Frappé | Macchiato | Mocha |
|---|---|---|---|---|
| `rosewater` | `#dc8a78` | `#f2d5cf` | `#f4dbd6` | `#f5e0dc` |
| `flamingo` | `#dd7878` | `#eebebe` | `#f0c6c6` | `#f2cdcd` |
| `pink` | `#ea76cb` | `#f4b8e4` | `#f5bde6` | `#f5c2e7` |
| `mauve` | `#8839ef` | `#ca9ee6` | `#c6a0f6` | `#cba6f7` |
| `red` | `#d20f39` | `#e78284` | `#ed8796` | `#f38ba8` |
| `maroon` | `#e64553` | `#ea999c` | `#ee99a0` | `#eba0ac` |
| `peach` | `#fe640b` | `#ef9f76` | `#f5a97f` | `#fab387` |
| `yellow` | `#df8e1d` | `#e5c890` | `#eed49f` | `#f9e2af` |
| `green` | `#40a02b` | `#a6d189` | `#a6da95` | `#a6e3a1` |
| `teal` | `#179299` | `#81c8be` | `#8bd5ca` | `#94e2d5` |
| `sky` | `#04a5e5` | `#99d1db` | `#91d7e3` | `#89dceb` |
| `sapphire` | `#209fb5` | `#85c1dc` | `#7dc4e4` | `#74c7ec` |
| `blue` | `#1e66f5` | `#8caaee` | `#8aadf4` | `#89b4fa` |
| `lavender` | `#7287fd` | `#babbf1` | `#b7bdf8` | `#b4befe` |
| `text` | `#4c4f69` | `#c6d0f5` | `#cad3f5` | `#cdd6f4` |
| `subtext1` | `#5c5f77` | `#b5bfe2` | `#b8c0e0` | `#bac2de` |
| `subtext0` | `#6c6f85` | `#a5adce` | `#a5adcb` | `#a6adc8` |
| `overlay2` | `#7c7f93` | `#949cbb` | `#939ab7` | `#9399b2` |
| `overlay1` | `#8c8fa1` | `#838ba7` | `#8087a2` | `#7f849c` |
| `overlay0` | `#9ca0b0` | `#737994` | `#6e738d` | `#6c7086` |
| `surface2` | `#acb0be` | `#626880` | `#5b6078` | `#585b70` |
| `surface1` | `#bcc0cc` | `#51576d` | `#494d64` | `#45475a` |
| `surface0` | `#ccd0da` | `#414559` | `#363a4f` | `#313244` |
| `base` | `#eff1f5` | `#303446` | `#24273a` | `#1e1e2e` |
| `mantle` | `#e6e9ef` | `#292c3c` | `#1e2030` | `#181825` |
| `crust` | `#dce0e8` | `#232634` | `#181926` | `#11111b` |

## Which color goes where

Every flavor uses the same role names, so you swap flavors by swapping values only.

**Surfaces, from deepest to most raised.** In Latte the hex values run the other way, but the roles stay the same.

- `crust`: deepest recess, such as window borders, a blockquote's left border, or a status bar.
- `mantle`: sunken panels, such as sidebars, code block backgrounds, popups, table headers and search inputs.
- `base`: the main page or editor background.
- `surface0`/`1`/`2`: raised elements, borders, hover backgrounds and inactive tabs. Go one step up for each nested level.
- `overlay0`/`1`/`2`: muted chrome, such as icons (`overlay0` at rest, `overlay2` on hover), scrollbars, gutters, disabled items and dividers. Use `overlay2` at 20-30% opacity for selection backgrounds.

**Text**
- `text`: body text and headings.
- `subtext1` for secondary labels, `subtext0` for captions and placeholders.
- On a solid accent background (buttons, badges), use `base` as the text color, not `text`.

**Accents and meanings**
- `blue`: links, the primary action, focus, and the active item.
- `lavender`: secondary accent, such as header underlines or the active border.
- `green` = success or tip, `yellow` = warning, `red` = error or danger, `peach` = attention or search highlight,
  `mauve` = important or keywords, `teal`/`sky`/`sapphire` = info or alternate accents.
- `rosewater`: cursor. `pink`/`flamingo`/`maroon`: decorative accents (the active gutter line, tags).
- GitHub-style callouts: NOTE `blue`, TIP `green`, IMPORTANT `mauve`, WARNING `yellow`, CAUTION `red`.

**Charts and series:** in this order, use `blue`, `green`, `peach`, `mauve`, `teal`, `red`, `yellow` and `pink`. For gridlines, use `surface0`/`surface1`. For axis labels, use `subtext0`.

## How to implement it

1. **CSS / web:** copy [`palette.css`](palette.css) from this skill folder into the project. It defines
   `--ctp-<color>` for every color in four classes (`.latte`, `.frappe`, `.macchiato`, `.mocha`). Put a
   flavor class on `<html>`, or on any container. Then map the app's own tokens onto it, for example
   `--bg: var(--ctp-base); --fg: var(--ctp-text); --link: var(--ctp-blue);`. To follow the OS theme, apply
   `.latte` by default and `.mocha` under `@media (prefers-color-scheme: dark)`.
2. **JS / TS build:** use `@catppuccin/palette` (`flavors.mocha.colors.base.hex`) instead of hardcoding values.
   **Tailwind:** use `@catppuccin/tailwindcss` (classes like `bg-base text-text`).
3. **Known tools** (terminal, editor, syntax highlighting): check for an official port at
   `github.com/catppuccin/<tool>` before writing one by hand. For example, `@catppuccin/highlightjs` covers code blocks.
4. **Check contrast:** `subtext0` and `overlay*` are only for secondary content. Don't use them for body text.

## Reference: mdBook mapping (one project's full setup, to use as a pattern)

`--bg` base · `--fg` text · `--sidebar-bg` mantle · `--sidebar-fg` text · `--sidebar-active` blue ·
`--sidebar-non-existant`/`--sidebar-spacer` overlay0 · `--sidebar-header-border-color` lavender ·
`--links` blue · `--icons`/`--icons-hover` overlay0/overlay2 · `--inline-code-color` text (code bg mantle) ·
`--quote-bg` mantle · `--quote-border` crust · `--table-border-color` surface0 · `--table-header-bg`/`--table-alternate-bg` mantle ·
`--searchbar-bg` mantle · `--searchbar-fg` text · `--searchbar-border-color` surface0 · `--search-mark-bg` peach ·
`--theme-popup-bg` mantle · `--theme-popup-border` overlay0 · `--theme-hover` surface0 · `--warning-border` peach ·
`--color-scheme` light (latte) / dark (others) · Ace active-line gutter pink.
