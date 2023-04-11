# idk
export PATH="$PATH:$HOME/.local/bin/"

# Cleaning up home directory
export ZDOTDIR="$HOME/.config/zsh"
export GTK2_RC_FILES="$HOME/.config/gtk-2.0/gtkrc"
export GRADLE_USER_HOME="$HOME/Android/gradle"
export M2_HOME="$HOME/Android/m2"
export XDG_CONFIG_HOME="$HOME/.config"

# System defaults
export EDITOR="nvim"
export VISUAL=$EDITOR
export TERMINAL="terminator"
export BROWSER="firefox"
export FILE="ranger"

# Fix sorting in ls command (take . into consideration when sorting)
export LC_COLLATE="C"

if [[ "$(tty)" = "/dev/tty1" ]]; then
    pgrep i3 || startx "$XDG_CONFIG_HOME/X11/xinitrc"
fi 

eval "$(gh completion -s zsh)"
