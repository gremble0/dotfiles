# Source plugins
eval "$(starship init zsh)"
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Vi mode things
bindkey -v

# Binds $2 to command in $1 and pipes $CUTBUFFER into clipboard
generate-vi-xclip-pipe-cmd() {
    local cmdname="$1"
    local keybind="$2"

    eval "${cmdname}-xclip() { zle ${cmdname} && echo \"\$CUTBUFFER\" | xclip -selection clipboard }"
    zle -N "${cmdname}-xclip"
    bindkey -M vicmd ${keybind} ${cmdname}-xclip
}

# Binds $2 to the command in $1 after exporting clipboard to $CUTBUFFER
generate-vi-xclip-export-cmd() {
    local cmdname="$1"
    local keybind="$2"

    eval "${cmdname}-xclip() { export CUTBUFFER=\"\$(xclip -selection clipboard -out)\" && zle ${cmdname} }"
    zle -N "${cmdname}-xclip"
    bindkey -M vicmd ${keybind} ${cmdname}-xclip
}

generate-vi-xclip-pipe-cmd vi-yank "y"
generate-vi-xclip-pipe-cmd vi-yank-eol "Y"
generate-vi-xclip-pipe-cmd vi-delete "d"
generate-vi-xclip-pipe-cmd vi-delete-eol "D"
generate-vi-xclip-pipe-cmd vi-delete-char "x"
generate-vi-xclip-pipe-cmd vi-backward-delete-char "X"
generate-vi-xclip-pipe-cmd vi-change "c"
generate-vi-xclip-pipe-cmd vi-change-eol "C"

generate-vi-xclip-export-cmd vi-put-after "p"
generate-vi-xclip-export-cmd vi-put-before "P"

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

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../'
alias .....='cd ../../../'

alias ls='ls -F --group-directories-first --color=auto'
alias ll='ls -AFl --group-directories-first --color=auto'
alias grep='grep --color=auto'

alias shutdown='shutdown now'
