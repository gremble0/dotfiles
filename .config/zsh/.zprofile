# XDG defaults
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

# idk
export PATH="$PATH:$HOME/.local/bin/"

# Cleaning up home directory
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/config"
export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority"
export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME/java"
export GRADLE_USER_HOME="$HOME/Android/gradle"
export M2_HOME="$HOME/Android/m2" # not working

# System defaults
export EDITOR="nvim"
export VISUAL=$EDITOR
export TERMINAL="terminator"
export BROWSER="firefox"
export FILE="ranger"

# Fix sorting in ls command (take . into consideration when sorting)
export LC_COLLATE="C"

# Run startx if i3 session is not already active
if [[ "$(tty)" = "/dev/tty1" ]]; then
    pgrep i3 || startx "$XDG_CONFIG_HOME/X11/xinitrc"
fi 

eval "$(gh completion -s zsh)"
