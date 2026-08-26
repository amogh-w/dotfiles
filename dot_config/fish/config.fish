if status is-interactive
    # Commands to run in interactive sessions can go here
end

eval (/opt/homebrew/bin/brew shellenv)

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if test -f /opt/homebrew/Caskroom/miniconda/base/bin/conda
    eval /opt/homebrew/Caskroom/miniconda/base/bin/conda "shell.fish" "hook" $argv | source
else
    if test -f "/opt/homebrew/Caskroom/miniconda/base/etc/fish/conf.d/conda.fish"
        . "/opt/homebrew/Caskroom/miniconda/base/etc/fish/conf.d/conda.fish"
    else
        set -x PATH "/opt/homebrew/Caskroom/miniconda/base/bin" $PATH
    end
end
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
fish_add_path /Users/amogh/.spicetify

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
fish_add_path /Users/amogh/.antigravity/antigravity/bin

# Added by Antigravity CLI installer
set -gx PATH "/Users/amogh/.local/bin" $PATH
