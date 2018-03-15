# Need to migrate some settings from old machine
export ZSH=/home/jay/.oh-my-zsh
ZSH_THEME="robbyrussell"
plugins=(git command-not-found vagrant)
source $ZSH/oh-my-zsh.sh || git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh

source $HOME/.profile

# Custom aliases
alias n='nautilus . &!' # Opens nautilus and disowns it from current shell
alias ag='ag --pager less'

alias gdb='gdb -q'
alias peda='gdb -q -ex peda'
alias pwngdb='gdb -q -ex pwngdb'
alias pwndbg='gdb -q -ex pwndbg'

alias manually-installed-to-auto='sudo apt-mark auto'

alias record-term='asciinema rec --yes --max-wait 1 --title'

alias temax='emacs -nw'
alias cemax='emacsclient'
export ALTERNATE_EDITOR='emacs' # Opens emacs if no emacs server is
				# already started

alias ncdu='ncdu -rx' # Make ncdu safe (no delete) and fast (don't
		      # cross FS boundary)

alias clip='xclip -selection clipboard'

alias fetch-recursive-website='wget --recursive --no-parent -e robots=off'

alias screensaver='cmatrix -abs'

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

xbox-bluetooth () {
    echo 1 | sudo tee /sys/module/bluetooth/parameters/disable_ertm
}

# opam configuration
test -r /home/jay/.opam/opam-init/init.zsh && . /home/jay/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# connect up local gem repository
export PATH="/home/jay/.gem/ruby/2.3.0/bin:$PATH"

llvm () {
    export LLVM_ROOT="/home/jay/llvm-5.0.1.install/"
    export PATH="/home/jay/llvm-5.0.1.install/bin:$PATH"
    llvm () { } # Prevent the command from being an issue by running
		# too many times
}
