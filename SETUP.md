# Setting up a new machine

Situations and the exact commands to run. Assumes macOS with Homebrew.

## 1. Install chezmoi, clone this repo to ~/.dotfiles, and point chezmoi at it

The chezmoi source lives at `~/.dotfiles`, not the default `~/.local/share/chezmoi` — clone it
there directly, then symlink chezmoi's expected source path to it:

```sh
brew install chezmoi
git clone https://github.com/amogh-w/dotfiles.git ~/.dotfiles
ln -s ~/.dotfiles ~/.local/share/chezmoi
chezmoi init --apply
```

`chezmoi init` (no URL, since the source is already in place) and `--apply` deploys every
currently-enabled dotfile into `$HOME`. `dot_gitconfig` has git name/email hardcoded directly —
edit it in the source tree if those ever need to change.

## 2. Only some apps deploy by default

`.chezmoiignore` is set up as an explicit allowlist — apps get added to it one at a time as
they're reviewed, not all at once. Check what actually deployed:

```sh
chezmoi managed
```

If an app you expect is missing, it just hasn't been re-enabled in `.chezmoiignore` yet. Edit
that file, remove its ignore line, then:

```sh
chezmoi apply
```

## 3. Install the actual applications

chezmoi only manages *config files* — it doesn't install the apps themselves. Install what you
need via Homebrew:

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

`dot_config/fish/fish_plugins` lists the plugins but doesn't install them automatically. After
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
from scratch. This only clears data/state/cache — `~/.config/nvim` (chezmoi-managed config) is
untouched:

```sh
rm -rf ~/.local/share/nvim ~/.local/state/nvim ~/.cache/nvim
nvim
```

## 6. herdr plugins

The `herdr-agent-quota` plugin referenced in `dot_config/herdr/config.toml` is **not**
auto-installed — it was deliberately left out of chezmoi because its manifest
(`~/.config/herdr/plugins.json`) points at a local dev checkout path specific to this machine.

Clone and register it manually:

```sh
git clone https://github.com/levi-qiao/herdr-agent-quota.git ~/dev/herdr-agent-quota
```

Then register the local checkout with herdr per the plugin's own install instructions (check its
README for the exact `plugins.json` entry format), or check `herdr.dev` for a public distribution
method if one now exists.

## 7. Verify everything actually matches

```sh
chezmoi diff      # should be empty — if not, something didn't apply cleanly
chezmoi status
```

## Common follow-up commands

See [CHEZMOI.md](CHEZMOI.md) for the day-to-day command reference (editing files, re-adding
changes made outside chezmoi, adding new dotfiles, etc.).
