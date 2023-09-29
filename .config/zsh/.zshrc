# Source plugins
source $XDG_CONFIG_HOME/zsh/powerlevel10k/p10k.zsh
source $XDG_CONFIG_HOME/zsh/vi-mode/zsh-vi-mode.plugin.zsh
source $XDG_CONFIG_HOME/zsh/syntax-highlighting/zsh-syntax-highlighting.zsh

# Remove underline from syntax highlighting
(( ${+ZSH_HIGHLIGHT_STYLES} )) || typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[path]=none
ZSH_HIGHLIGHT_STYLES[path_prefix]=none

# History in .cache directory
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=$XDG_CACHE_HOME/zsh/history

# Completion
autoload -U compinit
zstyle ":completion:*" menu select
zstyle ":completion:*" matcher-list "m:{a-zA-Z}={A-Za-z}"
zmodload zsh/complist

# Move zcompdump to .cache folder
compinit -d $XDG_CACHE_HOME/zsh/zcompdump-$ZSH_VERSION

# Include hidden files.
_comp_options+=(globdots)

# Navigate menuselect with vi keybinds
bindkey -M menuselect "h" vi-backward-char
bindkey -M menuselect "k" vi-up-line-or-history
bindkey -M menuselect "l" vi-forward-char
bindkey -M menuselect "j" vi-down-line-or-history

# Aliases
alias c="clear"
alias n="nvim"
alias lf="lfcd"
alias sxiv="nsxiv"

alias ls="ls -CF --group-directories-first --color=auto"
alias ll="ls -AhgGoF --group-directories-first --color=auto"
alias grep="grep --color=auto"
alias fzf="fzf --reverse --height 50%"

alias shutdown="shutdown now"

# Opam configuration
[[ ! -r $XDG_DATA_HOME/opam/opam-init/init.zsh ]] || source $XDG_DATA_HOME/opam/opam-init/init.zsh > /dev/null 2> /dev/null
