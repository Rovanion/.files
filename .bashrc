export PATH="$HOME/.local/bin:$PATH"
export XDG_CONFIG_HOME="$HOME/.config"
export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"

# If not running interactively, don't do anything.
[ -z "$PS1" ] && return

green='\e[0;32m'
red='\e[0;31m'
blue='\e[0;34m'
cyan='\e[0;36m'
clear='\e[0m'

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"


# Are we connected from a remote host?
[ ! -z "$(who am i | cut -f2 -d\( | cut -f1 -d: | cut -f1 -d\))" ] && ssh_info="[${cyan}ssh${clear}]"

# Root?
if (( $(id -u) == 0 )); then
  user_colour=$red
  prompt_character="#"
else
  user_colour=$green
  prompt_character="$"
fi

function new-prompt {
  user_exit_code="$?"
  # Are we in a git tree and has it been modified?
  if git_diff=$(git diff-files --no-ext-diff 2>/dev/null) ; then
    branch=$(git branch --color=never | sed -ne 's/* //p')
    if [ -z "$git_diff" ]; then
      branch=[$blue$branch$clear]
    else
      branch=[$red$branch$clear]
    fi
  else
    unset branch
  fi
  if [ -n "$GUIX_ENVIRONMENT" ]; then
    guix_env="[${cyan}guix${clear}]"
	else
		unset guix_env
	fi
  if [ -n "$IN_NIX_SHELL" ]; then
    nix_env="[${cyan}nix-${IN_NIX_SHELL}${clear}]"
	else
		unset nix_env
	fi
	if [ -n "$PIPENV_ACTIVE" ]; then
    pipenv="[${cyan}pipenv${clear}]"
	else
		unset pipenv
	fi
	if [ -n "$VIRTUAL_ENV" ]; then
    python_venv="[${cyan}venv${clear}]"
	else
		unset python_venv
	fi

  if [[ $user_exit_code == 0 ]]; then
    path_colour=${green}
  else
    path_colour=${red}
  fi

  PS1="┌[${user_colour}\u${clear}][\h]${branch:+$branch}${guix_env}${nix_env}${pipenv}${python_venv}${ssh_info}:${path_colour}\w${clear}\n└${prompt_character} "
}

new-prompt

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# Allow <C-s> to pass through to shell and programs
stty -ixon -ixoff

# I want my visudos and git commits to be in emacs
export EDITOR="emacsclient -nw -a=\"\""
export VISUAL="emacsclient -a=\"\""

# Fixing not being able to type dead keys in emacs
XMODIFIERS="emacs"

# Tell golang where my workspace is.
export GOPATH=$HOME/.local/share/go

# For setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=10000
HISTFILESIZE=20000
# Avoid duplicates
export HISTCONTROL=ignoredups:erasedups
# When the shell exits, append to the history file instead of overwriting it.
shopt -s histappend
# After each command, append to the history file.
PROMPT_COMMAND="new-prompt; history -a;"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" --no-use  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Different systems have different names for the terminfofile.
if [ -f /usr/share/terminfo/r/rxvt-unicode-256color ]; then
  export TERM=rxvt-unicode-256color
elif [ -f /usr/share/terminfo/r/rxvt-256color ]; then
  export TERM=rxvt-256color
elif [ -f /usr/share/terminfo/x/xterm-256color ]; then
  export TERM=xterm-256color
else  # In the end we give up and hope xterm exists.
	export TERM=xterm
fi

if command -v direnv >/dev/null; then
	eval "$(direnv hook bash)"
fi

# Make bash readline behave like emacs so that you can copy and paste regions
stty werase undef
bind '"\C-w":kill-region'

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
  source $HOME/.nix-profile/etc/profile.d/nix.sh
	export NIXPKGS=$HOME/source/nix/nixpkgs
fi

if [ -e "$HOME/.config/guix/current/bin" ]; then
	export PATH="$HOME/.config/guix/current/bin:$HOME/.guix-profile/bin:$PATH"
	export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"
	# The below is supposedly doing some good and sourcing the profile is apparently "the way".
	export GUIX_PROFILE="$HOME/.guix-profile"
	source "$GUIX_PROFILE/etc/profile"
fi

# Assume that we have administrative privileges.
export PATH="$PATH:/usr/sbin:$HOME/.local/go/bin:$HOME/.cabal/bin:$HOME/.cargo/bin"

# Depends on ssh-agent process started in ~/.config/awesome/autostart.lua.
if [ -S "${XDG_RUNTIME_DIR}/ssh-agent.socket" ]; then
	export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"
fi
