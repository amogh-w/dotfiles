if status is-interactive
    # Commands to run in interactive sessions can go here
end

if test -x /opt/homebrew/bin/brew
    eval (/opt/homebrew/bin/brew shellenv)
else if test -x /home/linuxbrew/.linuxbrew/bin/brew
    eval (/home/linuxbrew/.linuxbrew/bin/brew shellenv)
end

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
set -l _conda_base /opt/homebrew/Caskroom/miniconda/base
if not test -d $_conda_base
    set _conda_base $HOME/miniconda3
end
if not test -d $_conda_base
    set _conda_base $HOME/anaconda3
end

if test -f $_conda_base/bin/conda
    eval $_conda_base/bin/conda "shell.fish" "hook" $argv | source
else
    if test -f "$_conda_base/etc/fish/conf.d/conda.fish"
        . "$_conda_base/etc/fish/conf.d/conda.fish"
    else
        set -x PATH "$_conda_base/bin" $PATH
    end
end
set -e _conda_base
# <<< conda initialize <<<

# https://github.com/starship/starship
starship init fish | source

# disable greeting
set -g fish_greeting

# aliases
alias startLatex="conda activate latex && latexocr"
alias startTranslate="conda activate libre && libretranslate --load-only en,de"
alias spotifyFix="spicetify update && spicetify restore backup apply"

# spicetify
fish_add_path $HOME/.spicetify

# bun
set --export BUN_INSTALL "$HOME/.bun"
set --export PATH $BUN_INSTALL/bin $PATH

function gen_llm -d "Generate output.txt with all Python files concatenated"
    python3 -c "
import os
with open('output.txt','w',encoding='utf-8') as out:
    for r,_,fs in os.walk('.'):
        for f in fs:
            if f.endswith('.py'):
                p=os.path.join(r,f)
                out.write(f'=== FILE: {p} ===\\n\\n')
                try: out.write(open(p,encoding='utf-8').read())
                except Exception as e: out.write(f'[ERROR READING FILE: {e}]\\n')
                out.write('\\n\\n')
print('✅ Done: output.txt created')
"
end

# Added by Antigravity
fish_add_path $HOME/.antigravity/antigravity/bin

# Added by Antigravity CLI installer
set -gx PATH "$HOME/.local/bin" $PATH

# herdr-automatic-rename: live tab naming hook
for _f in $HOME/.config/herdr/plugins/github/herdr-automatic-rename-*/shell/hook.fish
    test -r "$_f"; and source "$_f"; and break
end
