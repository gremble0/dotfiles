# XDG defaults
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

# Ensure zsh has a directory under XDG_CACHE_HOME
if [ ! -d "$XDG_CACHE_HOME/zsh" ]; then
    mkdir -p $XDG_CACHE_HOME/zsh
fi

# Cleaning up home directory
# Config files
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"

alias nvidia-settings=nvidia-settings --config="$XDG_CONFIG_HOME/nvidia/settings"

# Data files
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export GOPATH="$XDG_DATA_HOME/go"
export GRADLE_USER_HOME="$XDG_DATA_HOME/gradle"
export JAVA_HOME="/usr/lib/jvm/java-21-openjdk"
export M2_HOME="$XDG_DATA_HOME/m2"
export OPAMROOT="$XDG_DATA_HOME/opam"
export PSQL_HISTORY="$XDG_DATA_HOME/psql_history"
export PYTHONSTARTUP="$XDG_DATA_HOME/pythonrc"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
export XCURSOR_PATH="/usr/share/icons:$XDG_DATA_HOME/icons"
export PNPM_HOME="$XDG_DATA_HOME/pnpm"

# Cache files
export CUDA_CACHE_PATH="$XDG_CACHE_HOME/nv"
export GRIPHOME="$XDG_CACHE_HOME/grip"

# Runtime files
export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority"

# Extend PATH
export PATH="$PATH:$HOME/.local/bin:$HOME/.local/bin/statusbar:$GOPATH/bin:$JAVA_HOME/bin:$PNPM_HOME"

# Fix sorting in ls command (take . into consideration when sorting)
export LC_COLLATE="C"
export LC_ALL="C.UTF-8"

# Colors for more filetypes when using ls
export LS_COLORS='di=1;34:*.jpg=00;35:*.jpeg=00;35:*.avif=00;35:*.mjpeg=00;35:*.gif=00;35:*.bmp=00;35:*.pbm=00;35:*.pgm=00;35:*.ppm=00;35:*.tga=00;35:*.xbm=00;35:*.xpm=00;35:*.tif=00;35:*.tiff=00;35:*.png=00;35:*.svg=00;35:*.svgz=00;35:*.mng=00;35:*.pcx=00;35:*.mov=00;35:*.mpg=00;35:*.mpeg=00;35:*.m2v=00;35:*.mkv=00;35:*.webm=00;35:*.webp=00;35:*.ogm=00;35:*.mp4=00;35:*.m4v=00;35:*.mp4v=00;35:*.vob=00;35:*.qt=00;35:*.nuv=00;35:*.wmv=00;35:*.asf=00;35:*.rm=00;35:*.rmvb=00;35:*.flc=00;35:*.avi=00;35:*.fli=00;35:*.flv=00;35:*.gl=00;35:*.dl=00;35:*.xcf=00;35:*.xwd=00;35:*.yuv=00;35:*.cgm=00;35:*.emf=00;35:*.ogv=00;35:*.ogx=00;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.pdf=00;33:*.md=00;33'

# Fzf environment variables
export FZF_DEFAULT_COMMAND='find .'
export FZF_DEFAULT_OPTS='--layout=reverse --separator=" " --ansi --prompt="❯ " --height=50% --color=dark,prompt:3,pointer:3,info:2,query:7:regular,hl:3,hl+:3,bg+:#333333'

# Extra setup for work
if NOVA_PROFILE_PATH="${XDG_CONFIG_HOME:-$HOME/.config}/nova/nova_profile" && [ -f "$NOVA_PROFILE_PATH" ]; then
    source $NOVA_PROFILE_PATH
elif NOVA_PROFILE_PATH="$HOME/.nova_profile" && [ -f "$NOVA_PROFILE_PATH" ]; then
    source $NOVA_PROFILE_PATH
fi

if [ "$(tty)" = "/dev/tty1" ]; then
    startx
fi
