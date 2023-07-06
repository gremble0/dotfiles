# History in .cache directory
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=$XDG_CACHE_HOME/zsh/history

# Completion
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
# Move zcompdump to .cache folder
compinit -d $XDG_CACHE_HOME/zsh/zcompdump-$ZSH_VERSION
# Include hidden files.
_comp_options+=(globdots)

# Navigate menuselect with vi keybinds
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

# Aliases
alias c="clear"
alias n="nvim"
alias ra="rangercd"
alias lf="lfcd"

alias ls="ls -CF --group-directories-first --color=auto"
alias ll="ls -AhgGoF --group-directories-first --color=auto"
alias grep="grep --color=auto"

alias shutdown="shutdown now"

# Setting prompt
PS1='%F{yellow}%n@%m%f%F{blue}%~%f %F{green}$%f '

# Case insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# Syntax highlighting
source $XDG_CONFIG_HOME/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Remove underline from syntax highlighting
(( ${+ZSH_HIGHLIGHT_STYLES} )) || typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[path]=none
ZSH_HIGHLIGHT_STYLES[path_prefix]=none

# Run tmux on startup

if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
    exec tmux new-session -A -s main
fi

# opam configuration
[[ ! -r $XDG_DATA_HOME/opam/opam-init/init.zsh ]] || source $XDG_DATA_HOME/opam/opam-init/init.zsh > /dev/null 2> /dev/null
