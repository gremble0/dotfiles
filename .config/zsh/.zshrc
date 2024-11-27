# Source plugins
eval "$(starship init zsh)"
[ -f "/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ] && \
    source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# History
HISTFILE=$XDG_CACHE_HOME/zsh/history
HISTSIZE=50000
SAVEHIST=50000

# Completion
autoload -U compinit
compinit -d $XDG_CACHE_HOME/zsh/zcompdump-$ZSH_VERSION
zstyle ":completion:*" menu select
zstyle ":completion:*" matcher-list "m:{a-zA-Z}={A-Za-z}"
zstyle ":completion:*" list-colors ${(s.:.)LS_COLORS}
zmodload zsh/complist
_comp_options+=(globdots)
setopt nocaseglob

# Search files with fzf
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

# Reverse search with fzf
fzf-reverse-search() {
    local selected=($(history -n 1 | awk '!seen[$0]++' | fzf))
    if [ -n "$selected" ]; then
        BUFFER=$selected
        CURSOR=$#BUFFER
    fi
    zle reset-prompt
}

zle -N fzf-reverse-search
bindkey '^R' fzf-reverse-search

# Aliases
alias c='clear'
alias n='nvim'
alias f='fzfcd'
alias i='nsxiv'
alias g='git'
alias o='xdg-open'
alias syu='sudo pacman -Syu'

# Faster cding
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../'
alias .....='cd ../../../'
alias cdc="cd $XDG_CONFIG_HOME"
alias cdn="cd $XDG_CONFIG_HOME/nvim"
alias cdz="cd $XDG_CONFIG_HOME/zsh"
alias cdt="cd $XDG_CONFIG_HOME/tmux"

alias ls='ls -F --group-directories-first --color=auto'
alias ll='ls -AFl --group-directories-first --color=auto'
alias lh='ll ~'
alias grep='grep --color=auto'

alias shutdown='shutdown now'

# Extra setup for work
[ -f $XDG_CONFIG_HOME/nova/workrc ] && source $XDG_CONFIG_HOME/nova/workrc
