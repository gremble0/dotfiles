# Source plugins
source $XDG_CONFIG_HOME/zsh/powerlevel10k/p10k.zsh
source $XDG_CONFIG_HOME/zsh/vi-mode/zsh-vi-mode.plugin.zsh
source $XDG_CONFIG_HOME/zsh/syntax-highlighting/zsh-syntax-highlighting.zsh

# Remove underline from syntax highlighting
(( ${+ZSH_HIGHLIGHT_STYLES} )) || typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[path]=none
ZSH_HIGHLIGHT_STYLES[path_prefix]=none

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

# Navigate menuselect with vi keybinds
bindkey -M menuselect "h" vi-backward-char
bindkey -M menuselect "k" vi-up-line-or-history
bindkey -M menuselect "l" vi-forward-char
bindkey -M menuselect "j" vi-down-line-or-history

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
alias l='lfcd'
alias i='nsxiv'

alias ls='eza -F --group-directories-first'
alias ll='eza -AFl --group-directories-first'
alias grep='grep --color=auto'

alias shutdown='shutdown now'

export FZF_DEFAULT_COMMAND='find .'
export FZF_DEFAULT_OPTS='--layout=reverse --separator=" " --ansi --prompt="❯ " --color=dark,prompt:3,pointer:4,info:2,query:7:regular,hl:3,hl+:3,gutter:0,bg+:#333333'
