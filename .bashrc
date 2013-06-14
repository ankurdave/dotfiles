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
case "$TERM" in
    xterm*|rxvt*|eterm-color|screen)
        SMART_TERM=yes
        ;;
    *)
        ;;
esac

case "$SMART_TERM" in
    yes)
        PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

        case $(uname) in
            Darwin)
                alias ls='ls -G'
                ;;
            Linux)
                eval "`dircolors -b`"
                alias ls='ls --color=auto'
                ;;
        esac
        ;;
    *)
        PS1='\u@\h:\w\$ '
        ;;
esac

export EDITOR='emacsclient'

# Enhance less
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"
export LESS="$LESS -FSX"

if [ -f /opt/local/etc/bash_completion ]; then
    . /opt/local/etc/bash_completion
fi

stty werase undef
bind '"\C-w": backward-kill-word'

# Include post-bashrc local config
if [ -f ~/.bashrc.d/local-config ]; then
    . ~/.bashrc.d/local-config post
fi
