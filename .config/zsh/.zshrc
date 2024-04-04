# Source plugins
eval "$(starship init zsh)"
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Use emacs bindings by default but with vicmd mode upon hitting escape
bindkey -e
bindkey -e "^[" vi-cmd-mode

# Binds $1 to command in $2 and pipes $CUTBUFFER into clipboard
generate-vi-xclip-pipe-cmd() {
    local keybind="$1"
    local cmdname="$2"

    eval "${cmdname}-xclip() { zle ${cmdname} && echo \"\$CUTBUFFER\" | xclip -selection clipboard }"
    zle -N "${cmdname}-xclip"
    bindkey -M vicmd ${keybind} ${cmdname}-xclip
}

# Binds $1 to the command in $2 after exporting clipboard to $CUTBUFFER
generate-vi-xclip-export-cmd() {
    local keybind="$1"
    local cmdname="$2"

    eval "${cmdname}-xclip() { export CUTBUFFER=\"\$(xclip -selection clipboard -out)\" && zle ${cmdname} }"
    zle -N "${cmdname}-xclip"
    bindkey -M vicmd ${keybind} ${cmdname}-xclip
}

generate-vi-xclip-pipe-cmd "y" vi-yank 
generate-vi-xclip-pipe-cmd "Y" vi-yank-eol 
generate-vi-xclip-pipe-cmd "d" vi-delete 
generate-vi-xclip-pipe-cmd "D" vi-delete-eol 
generate-vi-xclip-pipe-cmd "x" vi-delete-char 
generate-vi-xclip-pipe-cmd "X" vi-backward-delete-char 
generate-vi-xclip-pipe-cmd "c" vi-change 
generate-vi-xclip-pipe-cmd "C" vi-change-eol 

generate-vi-xclip-export-cmd "p" vi-put-after 
generate-vi-xclip-export-cmd "P" vi-put-before 

# Reduce delay between keycords (effectively removes delay for entering vi mode)
KEYTIMEOUT=1

# History in .cache directory
HISTSIZE=50000
SAVEHIST=50000
HISTFILE=$XDG_CACHE_HOME/zsh/history

# Completion
autoload -U compinit
compinit -d $XDG_CACHE_HOME/zsh/zcompdump-$ZSH_VERSION
zstyle ":completion:*" menu select
zstyle ":completion:*" matcher-list "m:{a-zA-Z}={A-Za-z}"
zstyle ":completion:*" list-colors ${(s.:.)LS_COLORS}
zmodload zsh/complist

# Include hidden files.
_comp_options+=(globdots)

fzfcd() {
    local out="$(fzf)"

    if [ -z $out ]; then
        echo "fzf search cancelled"
    elif [ -d "$out" ]; then
        cd "$out"
    else
        cd "$(dirname $out)"
    fi
}

# Aliases
alias c='clear'
alias n='nvim'
alias f='fzfcd'
alias i='nsxiv'
alias o='xdg-open'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../'
alias .....='cd ../../../'

alias ls='ls -F --group-directories-first --color=auto'
alias ll='ls -AFl --group-directories-first --color=auto'
alias grep='grep --color=auto'

alias shutdown='shutdown now'
