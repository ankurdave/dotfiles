[ -z "$PS1" ] && return

# History
_hgcommit_history() {
    history -a
    (
        hg -R $HISTDIR pull
        hg -R $HISTDIR merge
        hg -R $HISTDIR commit -m "Automated merge from $USER@$FQDN"
        hg -R $HISTDIR add
        hg -R $HISTDIR commit -m "Automated history commit from $USER@$FQDN"
        hg -R $HISTDIR push
    ) &
    disown -h
}

_read_old_history() {
    export HISTSIZE=5
    for file in $(ls $HISTDIR/*-$FQDN | tail -n 5)
    do
        history -r $file
    done
    unset HISTSIZE
}

export HISTDIR=$HOME/history
mkdir -p $HISTDIR
export HISTCONTROL=ignoreboth
export HISTTIMEFORMAT='%Y-%m-%d %H:%M:%S - '
export FQDN=$(hostname --long)
export HISTDATE=$(date +%Y%m%dT%H%M%S)
export HISTFILE=$HISTDIR/$HISTDATE-$FQDN
unset HISTFILESIZE
unset HISTSIZE
shopt -s checkwinsize histappend cdspell checkjobs
trap _hgcommit_history EXIT

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
# if [ -z "$STY" ]; then
#     screen -xRR
# fi
