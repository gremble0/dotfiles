# Source plugins
eval "$(starship init zsh)"

source $ZDOTDIR/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
source $ZDOTDIR/plugins/fzf/shell/key-bindings.zsh 2>/dev/null

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

# Readline emacs mode
bindkey -e

# Aliases
alias c='clear'
alias n='nvim'
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
alias cdd="cd $HOME/Documents"

# `ls` stuff
alias ls='ls -AFh --group-directories-first --color=auto'
alias ll='ls -l'
alias grep='grep --color=auto'

# Piping into grep
alias eg='env | grep'
alias lg='ll | grep'

alias shutdown='shutdown now'

# Extra setup for work
if NOVARC_PATH="${XDG_CONFIG_HOME:-$HOME/.config}/nova/novarc" && [ -f "$NOVARC_PATH" ]; then
    source $NOVARC_PATH
elif NOVARC_PATH="$HOME/.novarc" && [ -f "$NOVARC_PATH" ]; then
    source $NOVARC_PATH
fi
