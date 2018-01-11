# Need to migrate some settings from old machine
export ZSH=/home/jay/.oh-my-zsh
ZSH_THEME="robbyrussell"
plugins=(git command-not-found)
source $ZSH/oh-my-zsh.sh || git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh

source $HOME/.profile

# Custom aliases
alias ag='ag --pager less'
alias gdb='gdb -q'
alias peda='gdb -q -ex peda'
alias pwngdb='gdb -q -ex pwngdb'
alias pwndbg='gdb -q -ex pwndbg'

alias manually-installed-to-auto='sudo apt-mark auto'

alias record-term='asciinema rec --yes --max-wait 1 --title'

alias temax='emacs -nw'

alias ncdu='ncdu -rx' # Make ncdu safe (no delete) and fast (don't
		      # cross FS boundary)

# Allow .local binaries
export PATH=~/.local/bin:$PATH

# ntfy integration
eval "$(ntfy shell-integration)"
mobile-send () {
    k="$@[*]";
    k=$(if [ -z "$k" ]; then echo "ping"; else echo "$k"; fi);
    ntfy -b insta send "$k";
}
mobile-when-finished () {
    k="${history[$HISTCMD]}";
    k="${k//; mobile-when-finished/}";
    k="${k//;mobile-when-finished/}";
    k="${k//&& mobile-when-finished}";
    k="${k//&&mobile-when-finished}";
    mobile-send "$k";
}

# opam configuration
test -r /home/jay/.opam/opam-init/init.zsh && . /home/jay/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
