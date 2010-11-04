# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# History settings: Ignore duplicates and repeated lines
export HISTCONTROL=ignoredups
export HISTCONTROL=ignoreboth

# Update window size between commands
shopt -s checkwinsize

# Make less more friendly for non-text files
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# Colored prompt
case "$TERM" in
xterm*|rxvt*|eterm-color|screen)
    PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
    ;;
*)
    ;;
esac

# Colored ls
if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
fi

# Add ~/bin to path if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set editor
export EDITOR='emacsclient --alternate-editor=emacs'

# make del a safer rm
alias del='gvfs-trash'

# make vless a syntax-highlighting pager
alias vless='/usr/share/vim/vim72/macros/less.sh -'

# Add line editing to ocaml toplevel
alias ocaml='rlwrap ocaml'

# Include site-local config
if [ -f ~/.bash_sitelocal ]; then
    . ~/.bash_sitelocal
fi

# finally, start screen (unless already in screen)
if [ -z "$STY" ]; then
    screen -xRR
fi
