# Working with chezmoi

Quick reference for common situations. Source dir is `~/.dotfiles` (symlinked from
`~/.local/share/chezmoi`).

## I edited a dotfile directly in `$HOME` and want to save the change

```sh
chezmoi add ~/.config/fish/config.fish
```

Pulls the current on-disk version into the source tree (as `dot_config/fish/config.fish`), overwriting
what was there. Then commit as usual:

```sh
cd ~/.dotfiles
git add -A && git commit -m "Update fish config"
```

## I edited a file in the source repo and want to deploy it

```sh
chezmoi apply
```

Applies every pending change. To apply just one file:

```sh
chezmoi apply ~/.config/nvim/init.lua
```

## I want to see what would change before applying anything

```sh
chezmoi diff
```

## I want to edit a managed file and apply it in one flow

```sh
chezmoi edit ~/.tmux.conf
chezmoi apply
```

Opens the *source* file (`dot_tmux.conf`) in `$EDITOR`. Nothing deploys until you `apply`.

## I edited several already-managed files in `$HOME` and want to sync them all back

```sh
chezmoi re-add
```

Like `chezmoi add`, but re-scans every already-managed file for on-disk changes instead of naming one
path. Use this after a session of editing live configs directly instead of via `chezmoi edit`.

## I want to add a brand-new dotfile that doesn't exist in the repo yet

```sh
chezmoi add ~/.config/newapp/config.toml
cd ~/.dotfiles && git add -A && git commit -m "Add newapp config"
```

## I want to check what chezmoi thinks it's managing

```sh
chezmoi managed
```

## I want to know what an on-disk file's source path is (or vice versa)

```sh
chezmoi source-path ~/.config/fish/config.fish
# -> ~/.dotfiles/dot_config/fish/config.fish

chezmoi target-path ~/.dotfiles/dot_vimrc
# -> ~/.vimrc
```

## I don't want chezmoi to touch a file/directory

Add it to `.chezmoiignore` (plain paths relative to `$HOME`, gitignore-style syntax):

```
.config/fish/fish_variables
.jupyter/lab/workspaces
```

## I renamed/moved something in the source tree and it's not deploying right

```sh
chezmoi apply --dry-run --verbose
```

Shows exactly what would be created/removed/modified without touching anything.

## Setting up a new machine

```sh
chezmoi init --apply https://github.com/<you>/dotfiles.git
```

Clones straight into `~/.local/share/chezmoi` and applies everything in one step.

## I want to undo the last `apply` because it broke something

chezmoi doesn't keep its own history — recover via git:

```sh
cd ~/.dotfiles
git log --oneline -- dot_config/nvim/init.lua
git checkout <commit> -- dot_config/nvim/init.lua
chezmoi apply
```

## Naming cheatsheet

| On disk (`$HOME`)          | In source (`~/.dotfiles`)       |
|-----------------------------|----------------------------------|
| `~/.vimrc`                  | `dot_vimrc`                      |
| `~/.config/nvim/init.lua`   | `dot_config/nvim/init.lua`       |
| `~/.gitconfig`              | `dot_gitconfig`                  |
| `~/.config/fish/` (dir)     | `dot_config/fish/` (dir)         |
