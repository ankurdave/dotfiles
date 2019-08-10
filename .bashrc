# Add ~/bin to path if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# Include local path config
if [ -f ~/.bashrc.d/local-config ]; then
    . ~/.bashrc.d/local-config path
fi

# Do nothing else if non-interactive
[ -z "$PS1" ] && return

# Include pre-bashrc local config
if [ -f ~/.bashrc.d/local-config ]; then
    . ~/.bashrc.d/local-config pre
fi

# Keep history forever
export HISTDIR=$HOME/history
export HISTCONTROL=ignoreboth
export HISTTIMEFORMAT='%Y-%m-%d %H:%M:%S - '
export HISTFILE=$HISTDIR/$HOSTNAME
export HISTIGNORE=ls:exit
export HISTFILESIZE=
export HISTSIZE=
export PROMPT_COMMAND='history -a'

# Set various shell options
shopt -s checkwinsize histappend cdspell
if [ ${BASH_VERSINFO[0]} -ge 4 ]
then
    shopt -s checkjobs
fi

# Use colored prompt and ls
PS1='\u@\h \D{%Y-%m-%d %H:%M:%S} \w\n\$ '
term_colors=$(tput colors)
if test -n "$term_colors"; then
    if test $term_colors -ge 256; then
        hostname_colors=(146 141 152 228 210 117 218 156)
        hostname_crc=$(echo $HOSTNAME | tr 'A-Z' 'a-z' | cksum)
        hostname_crc=${hostname_crc%% *}
        hostname_color=${hostname_colors[${hostname_crc} % ${#hostname_colors[@]}]}
        PS1="\[\033[01;38;5;${hostname_color}m\]\u@\h\[\033[00m\] \D{%Y-%m-%d %H:%M:%S} \[\033[01;34m\]\w\[\033[00m\]\n\$ "
    fi
    if test $term_colors -ge 8; then
        case $(uname) in
            Darwin)
                alias ls='ls -G'
                ;;
            Linux)
                eval "`dircolors -b`"
                alias ls='ls --color=auto'
                ;;
        esac
    fi
fi

if [ -n "$SSH_CLIENT" ]; then
    export EDITOR='emacsclient -nw'
else
    export EDITOR='emacsclient'
fi
alias e='emacsclient -n'
alias magit='emacsclient -n --eval '\''(let ((display-buffer-alist `(("^\\*magit: " display-buffer-same-window) ,display-buffer-alist))) (magit-status))'\'''

# Enhance less
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"
export LESS="$LESS -FSX"

command_exists () {
    type "$1" &> /dev/null ;
}
if command_exists brew; then
    if [ -f $(brew --prefix)/etc/bash_completion ]; then
	. $(brew --prefix)/etc/bash_completion
    fi
fi
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

stty werase undef
bind '"\C-w": backward-kill-word'

export JAVA_OPTS="-Dscala.color"

# Include post-bashrc local config
if [ -f ~/.bashrc.d/local-config ]; then
    . ~/.bashrc.d/local-config post
fi
