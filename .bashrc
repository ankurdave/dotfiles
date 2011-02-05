[ -z "$PS1" ] && return

mkdir -p $HOME/history
export HISTCONTROL=ignoreboth
export HISTTIMEFORMAT='%Y-%m-%d %H:%M:%S - '
export FQDN=$(hostname --long)
export HISTDATE=$(date +%Y%m%dT%H%M%S)
export HISTFILE=$HOME/history/$HISTDATE-$FQDN
unset HISTFILESIZE
unset HISTSIZE
shopt -s checkwinsize histappend cdspell checkjobs

[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# Colors
PS1='\u@\h:\w\$ '
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
        ;;
esac

# Add ~/bin to path if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# Use Emacs as a server
if [[ -n "`emacs --help | grep -- --daemon`" ]]
then
    emacs --daemon >/dev/null 2>&1
    export EDITOR='emacsclient -a emacs -c'
    alias e="$EDITOR"
else
    export EDITOR='emacs'
    alias e="$EDITOR"
fi


# Include site-local config
if [ -f ~/.bash_sitelocal ]; then
    . ~/.bash_sitelocal
fi

# # Finally, start screen (unless already in screen)
# if [ -z "$STY" ]; then
#     screen -xRR
# fi
