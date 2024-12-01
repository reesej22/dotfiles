#:::::'########:::::'###:::::'######::'##::::'##:'########:::'######:::#
#::::: ##.... ##:::'## ##:::'##... ##: ##:::: ##: ##.... ##:'##... ##::#
#::::: ##:::: ##::'##:. ##:: ##:::..:: ##:::: ##: ##:::: ##: ##:::..:::#
#::::: ########::'##:::. ##:. ######:: #########: ########:: ##::::::::#
#::::: ##.... ##: #########::..... ##: ##.... ##: ##.. ##::: ##::::::::#
#'###: ##:::: ##: ##.... ##:'##::: ##: ##:::: ##: ##::. ##:: ##::: ##::#
# ###: ########:: ##:::: ##:. ######:: ##:::: ##: ##:::. ##:. ######:::#
#...::........:::..:::::..:::......:::..:::::..::..:::::..:::......::::#
# Author: Joshua
# Date: 09-20-2024
# Config for HP Laptop
 
# Doom Emacs Path
export PATH="/home/jgr/.config/emacs/bin:$PATH"

# Zoxide Config
eval "$(zoxide init bash)"

# Fzf Config
eval "$(fzf --bash)"

# Default Cmd
export FZF_DEFAULT_COMMAND='fd --type f --hidden'

# Default Opt
export FZF_DEFAULT_OPTS="--preview 'file {}' --preview-window up,1,border-horizontal \
    --bind 'ctrl-/:change-preview-window(50%|hidden|)' \
    --color 'fg:#bbccdd,fg+:#ddeeff,bg:#334455,preview-bg:#223344,border:#778899'"

# Set Vim as MANPAGER
# Must add "runtime ftplugin/man.vim" to vimrc
export MANPAGER="vim +MANPAGER --not-a-term -"

# Set Vim as EDITOR
export EDITOR="nvim"

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Prompt
PS1='\[\033[1;36m\] \u\[\033[1;31m\]@\[\033[1;32m\]\h:  \[\033[1;35m\]\W\[\033[1;31m\]  \[\033[0m\]'

# no duplicate lines in the history
HISTCONTROL=ignoredups:ignorespace

# Increase History size
HISTSIZE=1000
HISTFILESIZE=2000

# append to the history file, don't overwrite it
shopt -s histappend

# check the window size after each command
# update the values of Lines and Columns
shopt -s checkwinsize

# Bash Completion
if [[ -r /usr/share/bash-completion/bash_completion ]]; then
  . /usr/share/bash-completion/bash_completion
fi

# Automatically start or attach to a tmux session
if command -v tmux &> /dev/null; then
    # Check if we're already inside a tmux session
    if [ -z "$TMUX" ]; then
        # Start a new session or attach to an existing one
        tmux attach-session -t default || tmux new-session -s default
    fi
fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Aliases
alias cd='z'
alias ls='lsd --color=auto'
alias ll='lsd --color=auto -l'
alias la='lsd --color=auto -a'
alias grep='grep --color=auto'
alias vf='vim "$(fzf)"'
alias bash-config='vim ~/.bashrc'
alias bash-src='source ~/.bashrc'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Weather
function weather() {
    curl wttr.in/fort_polk
}
