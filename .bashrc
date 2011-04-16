# Add ~/bin to path if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# Do nothing else if non-interactive
[ -z "$PS1" ] && return

# Keep history forever
export HISTDIR=$HOME/history
export HISTCONTROL=ignoreboth
export HISTTIMEFORMAT='%Y-%m-%d %H:%M:%S - '
export FQDN=$(hostname)
export HISTDATE=$(date +%Y%m%dT%H%M%S)
export HISTFILE=$HISTDIR/$HISTDATE-$FQDN
unset HISTFILESIZE
unset HISTSIZE

# Set various shell options
shopt -s checkwinsize histappend cdspell
if [ ${BASH_VERSINFO[0]} -ge 4 ]
then
    shopt -s checkjobs
fi

# Use colored prompt and ls
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

# Automatically set screen title to short_cmd:short_pwd:hostname
short_cmd() {
    case ${1%% *} in
        ssh)
            echo ${1##* }:
            ;;
        screen)
            ;;
        *)
            echo ${1%% *}:
            ;;
    esac
}
short_pwd() {
    pwd | perl -pe 's#'$HOME'#~#; until (length() < 20 || $_ eq $prev) { $prev = $_; s#^(\.\.\./)?([^/]+/)(.*)$#$3# }'
}
case "$TERM" in
    screen)
        PROMPT_COMMAND='echo -ne "\033k$(short_pwd):${HOSTNAME:0:3}\033\\"'
        trap 'echo -ne "\033k$(short_cmd $BASH_COMMAND)$(short_pwd):${HOSTNAME:0:3}\033\\"' DEBUG
        ;;
esac

# Launch emacsclient by typing e
if [[ -n "`which emacsclient`" ]]
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

# Enhance less
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# Include site-local config
if [ -f ~/.bash_sitelocal ]; then
    . ~/.bash_sitelocal
fi

# # Finally, start screen (unless already in screen)
if [ -z "$STY" ]; then
    screen -xRR
fi
