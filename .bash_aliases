# Work around my mistyping.
alias "atp-get"="apt-get"

# Fix so that sudo <alias of choice> works.
alias sudo="sudo "

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias some handy ls commands
alias ll='ls -lFh'
alias la='ls -lah'
alias l='ls -CFh'

# Always show the diffs at the bottom of the commits
alias gc="git commit -v"

alias fuck=sl

alias agi="sudo apt-get install"
alias agr='sudo apt-get remove'
alias agu='sudo apt-get update'
alias acs='apt-cache search'
alias afs='apt-file search'

# Start the emacs mail client.
alias mu4e="mu index --maildir=~/.cache/mail/; ew -e '(mu4e)'"


# Open any file with the default application as defined by the desktop environment with o.
o() { xdg-open "$@" &>/dev/null & }

alias lns="ln -s"

alias digg="dig +nocmd any +multiline +noall +answer"

alias calc="PYTHONSTARTUP=<(echo \"from math import *\") python3"

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
  test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
  alias ls='ls --color=auto -h'
  alias dir='dir --color=auto'

  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
fi


countdown() {
  stop="$(( $(date '+%s') + $1))"
	term_width=$(tput cols)
	counter_width=10
  while [ $stop -ge $(date +%s) ]; do
    delta="$(( $stop - $(date +%s) ))"
		complete_percent=$(( 100 - ($delta * 100) / $1))
		bar_width=$(($complete_percent * ($term_width - $counter_width) / 100))
		printf '\r'
		printf '%s ' "$(date -u -d "@$delta" +%H:%M:%S)"
		printf '%0.s-' $(seq 1 $bar_width)
    sleep 0.5
  done
	printf '\n'
	notify-send --expire-time 10000 "Dags För rast!" "Släpp allt din dumme jävel."
}
