# Source plugins
eval "$(starship init zsh)"
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
    local selected num
    setopt localoptions noglobsubst noposixbuiltins pipefail no_aliases 2> /dev/null
    selected=( $(fc -rl 1 | perl -ne 'print if !$seen{(/^\s*[0-9]+\**\s+(.*)/, $1)}++' | fzf) )
    local ret=$?
    if [ -n "$selected" ]; then
        num=$selected[1]
        if [ -n "$num" ]; then
            zle vi-fetch-history -n $num
            zle accept-line
        fi
    fi
    zle reset-prompt
    return $ret
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
alias cdn='cd ~/.config/nvim'
alias cdz='cd ~/.config/zsh'
alias cdt='cd ~/.config/tmux'

alias ls='ls -F --group-directories-first --color=auto'
alias ll='ls -AFl --group-directories-first --color=auto'
alias grep='grep --color=auto'

alias shutdown='shutdown now'

# pnpm
export PNPM_HOME="/home/herman/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

[ -f $HOME/.workrc ] && source $HOME/.workrc
