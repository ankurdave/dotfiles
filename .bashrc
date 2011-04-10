# Add ~/bin to path if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

[ -z "$PS1" ] && return

# History
export HISTDIR=$HOME/history
export HISTCONTROL=ignoreboth
export HISTTIMEFORMAT='%Y-%m-%d %H:%M:%S - '
export FQDN=$(hostname)
export HISTDATE=$(date +%Y%m%dT%H%M%S)
export HISTFILE=$HISTDIR/$HISTDATE-$FQDN
unset HISTFILESIZE
unset HISTSIZE
shopt -s checkwinsize histappend cdspell checkjobs

[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# Colors
case "$TERM" in
    xterm*|rxvt*|eterm-color|screen)
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

case "$TERM" in
    screen)
        PS1='\[\033k\033\\\]'$PS1
        ;;
esac

# Use Emacs as a server
if [[ -n "`emacs --help | grep -- --daemon`" ]]
then
    export EDITOR='emacsclient -a "" -c'
    e() {
        if [[ -n $DISPLAY ]]
        then $EDITOR -n $@
        else $EDITOR -nw $@
        fi
    }
else
    export EDITOR='emacs'
    e() {
        $EDITOR $@
    }
fi


# Include site-local config
if [ -f ~/.bash_sitelocal ]; then
    . ~/.bash_sitelocal
fi

# # Finally, start screen (unless already in screen)
if [ -z "$STY" ]; then
    screen -xRR
fi
