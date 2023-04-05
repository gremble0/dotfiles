# System defaults
export EDITOR="nvim"
export VISUAL=$EDITOR

export BROWSER="/bin/firefox"

export ZSH_COMPDUMP=$HOME/.cache/.zcompdump-$HOST

# Aliases
alias c="clear"
alias n="nvim"
alias vim="nvim"
alias ex="ranger_cd"

alias ls='ls -CF --color'
alias ll='ls -AhgGoF --group-directories-first --color'

# Make ranger cd to dir after usage
ranger_cd() {
    temp_file="$(mktemp -t "ranger_cd.XXXXXXXXXX")"
    ranger --choosedir="$temp_file" -- "${@:-$PWD}"
    if chosen_dir="$(cat -- "$temp_file")" && [ -n "$chosen_dir" ] && [ "$chosen_dir" != "$PWD" ]; then
        cd -- "$chosen_dir"
    fi
    rm -f -- "$temp_file"
}

# Setting prompt
function parse_git_branch() {
    git branch 2> /dev/null | sed -n -e 's/^\* \(.*\)/(\1)/p'
}

setopt PROMPT_SUBST
export PROMPT='%F{green}%n@%m%f%F{blue}%~%f %F{yellow}$(parse_git_branch)%f $ '
