# Work around my mistyping.
alias "atp-get"="apt-get"

# Fix so that sudo <alias of choice> works.
alias sudo="sudo "

# Console emacs
alias ew="emacsclient -nw -a=\"\""
alias en="emacsclient -a=\"\""

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias some handy ls commands
alias ll='ls -lFh'
alias la='ls -lAh'
alias l='ls -CFh'

# Always show the diffs at the bottom of the commits
alias gc="git commit -v"

# Shorthand for upgrading debian/ubuntu
alias upg="sudo apt-get update && sudo apt-get autoremove && sudo apt-get dist-upgrade"

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

alias clj="rlwrap java -jar ~/.m2/repository/org/clojure/clojure/1.8.0/clojure-1.8.0.jar"