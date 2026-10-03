# Setting up a new machine

Situations and the exact commands to run. Assumes macOS with Homebrew.

## 1. Install stow, clone this repo to ~/.dotfiles, and symlink everything in

```sh
brew install stow
git clone https://github.com/amogh-w/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
stow -t ~ bash zsh git tmux vim kitty nvim ranger zathura joshuto
stow --no-folding -t ~ fish herdr
mkdir -p ~/.claude/skills && stow -t ~ claude
```

That symlinks every active package into `$HOME` (e.g. `fish/.config/fish/config.fish` →
`~/.config/fish/config.fish`). `git/.gitconfig` has git name/email hardcoded directly — edit it in
the source tree if those ever need to change. `fish` and `herdr` use `--no-folding` because those
apps write runtime state (fish's `fish_variables`, herdr's logs/lockfile/sockets) into the same
directory as their config — see [STOW.md](STOW.md) for why that matters. `claude` needs
`~/.claude/skills` to exist first so only the skills get symlinked, not all of `~/.claude` (see
[Claude skills](STOW.md#claude-skills)).

### Windows (Claude skills only)

Stow isn't available on Windows, so only the Claude skills get linked, using directory junctions
(no admin rights needed). From PowerShell:

```powershell
git clone https://github.com/amogh-w/dotfiles.git "$HOME\.dotfiles"
New-Item -ItemType Directory -Force "$HOME\.claude\skills" | Out-Null
Get-ChildItem "$HOME\.dotfiles\claude\.claude\skills" -Directory | ForEach-Object {
  $dest = "$HOME\.claude\skills\$($_.Name)"
  if (-not (Test-Path $dest)) { New-Item -ItemType Junction -Path $dest -Target $_.FullName }
}
```

Re-run the `Get-ChildItem ...` loop after adding a new skill to the repo. To unlink a skill, run
`(Get-Item "$HOME\.claude\skills\<name>").Delete()`. That removes only the junction, not the
repo folder. Avoid `Remove-Item -Recurse` on a junction, because in Windows PowerShell 5.1 it can
delete the real files it points to.

## 2. Only some packages are stowed by default

`doom/` and `jupyter/` live in the repo but aren't in the `stow` command above — same "not yet
reviewed for deployment" status they had before. Check what's actually symlinked:

```sh
ls -la ~/.bashrc ~/.config/fish ~/.doom.d ~/.jupyter
```

A path pointing back into `~/.dotfiles/...` is stowed; a real file/dir is not. To bring one in:

```sh
stow -t ~ doom
```

If a live file already exists at that path and isn't a symlink, stow will refuse — see
[STOW.md](STOW.md) for the `--adopt` / manual-remove options.

## 3. Install the actual applications

Stow only manages *config files* — it doesn't install the apps themselves. Install what you need
via Homebrew:

```sh
brew install fish kitty neovim ranger joshuto zathura tmux
brew install --cask herdr   # or however herdr is currently distributed — check herdr.dev/docs/install
```

Then set fish as your default shell if desired:

```sh
which fish   # note the path, e.g. /opt/homebrew/bin/fish
sudo sh -c 'echo /opt/homebrew/bin/fish >> /etc/shells'
chsh -s /opt/homebrew/bin/fish
```

## 4. Fish plugins (fisher)

`fish/.config/fish/fish_plugins` lists the plugins but doesn't install them automatically. After
fish is your shell:

```fish
curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source
fisher update
```

`fisher update` reads `fish_plugins` and installs everything listed there.

Some fish completions aren't from fisher plugins and won't come back automatically — regenerate
them manually if you use these tools:

```fish
copilot completion fish > ~/.config/fish/completions/copilot.fish
```

`bun`'s fish completion ships with the bun install itself; check `bun completions` or the bun docs
if it's not already present after installing bun.

## 5. Neovim plugins (lazy.nvim)

Open neovim once — `lua/core/lazy.lua` bootstraps lazy.nvim automatically on first launch and
installs every plugin pinned in `lazy-lock.json`:

```sh
nvim
```

Wait for the plugin install to finish, then quit and reopen.

### Resetting neovim (clean reinstall)

If plugins get into a bad state, wipe nvim's installed plugins/state and let lazy.nvim reinstall
from scratch. This only clears data/state/cache — `~/.config/nvim` (stow-symlinked to the repo) is
untouched:

```sh
rm -rf ~/.local/share/nvim ~/.local/state/nvim ~/.cache/nvim
nvim
```

## 6. herdr plugins

The `herdr-agent-quota` plugin referenced in `herdr/.config/herdr/config.toml` is **not**
auto-installed — its manifest (`~/.config/herdr/plugins.json`) points at a local dev checkout path
specific to this machine, so it's deliberately left out of the repo.

Clone and register it manually:

```sh
git clone https://github.com/levi-qiao/herdr-agent-quota.git ~/dev/herdr-agent-quota
```

Then register the local checkout with herdr per the plugin's own install instructions (check its
README for the exact `plugins.json` entry format), or check `herdr.dev` for a public distribution
method if one now exists.

## 7. Verify everything actually matches

```sh
stow -n -v -t ~ bash zsh git tmux vim fish kitty nvim ranger zathura joshuto herdr claude
```

A dry run with no `LINK:`/`WARNING:` output for a package means it's already correctly stowed.

## Common follow-up commands

See [STOW.md](STOW.md) for the day-to-day command reference (editing files — no apply step needed,
adding new dotfiles, unstow/restow, resolving conflicts, etc.).
