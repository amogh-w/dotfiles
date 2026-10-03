# Working with GNU stow

Source dir is `~/.dotfiles`. Each top-level directory (`fish/`, `nvim/`, `bash/`, ...) is a stow
"package" that mirrors `$HOME` from that point down — e.g. `fish/.config/fish/config.fish` stows to
`~/.config/fish/config.fish`. Stow just symlinks; there's no `dot_` renaming, no templating, no
source/dest translation step like chezmoi had.

| Situation | Command | Effect |
|---|---|---|
| Deploy a package (or all of them) | `cd ~/.dotfiles && stow -t ~ fish` (or list several: `stow -t ~ fish nvim zsh`) | Symlinks every file under `fish/` into the matching path under `~`, creating parent dirs as needed |
| Preview what stow would do first | `stow -n -v -t ~ fish` | Dry-run with verbose logging; touches nothing |
| Remove a package's symlinks | `stow -D -t ~ fish` | Deletes the symlinks stow created for that package, leaves the repo files untouched |
| Re-deploy after adding/removing files in a package | `stow -R -t ~ fish` | Unstow + restow in one step; use after adding a new file to a package so its symlink gets created |
| Edited a file, want to see the change deployed | Just edit the file in `~/.dotfiles/<package>/...` directly | No separate apply step — `$HOME` already points at the repo file via symlink, so it's live immediately |
| Adding a brand-new dotfile that isn't tracked yet | Move the real file into `~/.dotfiles/<package>/<same path relative to $HOME>`, then `stow -t ~ <package>` | Puts it under version control and symlinks it back into place in one motion |
| A target file already exists and isn't a symlink (stow refuses) | Either `rm` the live file first (if the repo copy is authoritative) or `stow --adopt -t ~ <package>` (pulls the live file's content into the repo, overwriting the repo copy) | `--adopt` is the safe move when the live file has drifted and you want to keep what's on disk |
| Want to add a whole new package for an app not yet tracked | `mkdir -p ~/.dotfiles/newapp/.config/newapp`, put its config there, `cd ~/.dotfiles && stow -t ~ newapp` | Same pattern as every other package — path under the package dir mirrors the path under `$HOME` |
| Setting up a new machine | `brew install stow`, `git clone <repo> ~/.dotfiles`, `cd ~/.dotfiles`, `stow -t ~ bash zsh git tmux vim kitty nvim ranger zathura joshuto`, `stow --no-folding -t ~ fish herdr`, `mkdir -p ~/.claude/skills && stow -t ~ claude` | Clones the repo and symlinks every active package into `$HOME`; `fish`/`herdr` need `--no-folding`, `claude` needs `~/.claude/skills` created first (see below) |
| Check what's actually symlinked from the repo | `find ~/.dotfiles -maxdepth 1 -type d ! -name .git` then `ls -la ~/.bashrc ~/.config/fish` etc. | Stow keeps no manifest of its own — a symlink pointing back into `~/.dotfiles/<package>/...` is confirmation it's active |
| Want to undo a bad edit | `cd ~/.dotfiles && git log --oneline -- <path>` then `git checkout <commit> -- <path>` | No separate deploy step needed — the live symlink picks up the reverted content immediately |

## Active packages

`bash`, `zsh`, `git`, `tmux`, `vim`, `fish`, `kitty`, `nvim`, `ranger`, `zathura`, `joshuto`, `herdr`,
`claude` — each stowed into `$HOME`.

## Runtime/generated files inside a stowed package

By default stow symlinks a whole directory when every file under it comes from one package (tree
folding) — e.g. `stow -t ~ fish` makes `~/.config/fish` itself a symlink to
`~/.dotfiles/fish/.config/fish`. That's a problem for apps that also write *runtime* state next to
their config (fish's `fish_variables`, herdr's logs/lockfile/sockets): those writes land straight in
the git working tree, because the whole directory *is* the repo directory.

Fix: stow that package with `--no-folding` (`stow -v --no-folding -t ~ fish herdr`). That symlinks each
file individually and leaves `~/.config/fish` / `~/.config/herdr` as real directories, so app-written
runtime files stay local to `$HOME` and only the files actually tracked in the repo are symlinks. This
repo already stows `fish` and `herdr` with `--no-folding` for exactly this reason — if you add a new
package where the app mixes config and runtime files in the same directory, do the same.

## Claude skills

`claude/.claude/skills/<name>/` holds personal Claude Code skills. `~/.claude` is full of local
state (credentials, history, sessions), so it must never become a symlink into the repo. Always make
sure `~/.claude/skills` exists as a real directory before stowing:

```sh
mkdir -p ~/.claude/skills && stow -t ~ claude
```

Stow then links each skill folder on its own (`~/.claude/skills/local-pdf-translator` →
`~/.dotfiles/claude/.claude/skills/local-pdf-translator`). Edits inside a skill are live right away.
After adding a new skill folder to the repo, run `stow -R -t ~ claude` to link it. Skills you create
directly in `~/.claude/skills` stay local until you move them into the repo and restow.

On Windows (no stow), link each skill with a directory junction from PowerShell, which needs no admin
rights:

```powershell
New-Item -ItemType Directory -Force "$HOME\.claude\skills" | Out-Null
Get-ChildItem "$HOME\.dotfiles\claude\.claude\skills" -Directory | ForEach-Object {
  $dest = "$HOME\.claude\skills\$($_.Name)"
  if (-not (Test-Path $dest)) { New-Item -ItemType Junction -Path $dest -Target $_.FullName }
}
```

## Dormant packages

`doom/` (`.doom.d`) and `jupyter/` (`.jupyter`) exist in the repo but are **not stowed** — same
"not yet reviewed for deployment" status they had under chezmoi's `.chezmoiignore`. Your live
`~/.doom.d` and `~/.jupyter` are real, independent files, untouched by this repo. Stow them
(`stow -t ~ doom`) only after reconciling any drift with `stow --adopt -t ~ doom`.

## Legacy packages

`awesomewm/`, `alacritty/`, `compton/`, `dunst/`, `i3/`, `i3blocks/`, `nitrogen/`, `optimus-manager/`,
`picom/`, `polybar/`, `rofi/`, `xresources/`, `gtk3/`, `nvimbackup/` — old Linux i3 rice, already in
stow-compatible layout (e.g. `alacritty/.config/alacritty/alacritty.yml`), kept for reference and not
stowed on macOS.
