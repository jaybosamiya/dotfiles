# Need to migrate some settings from old machine
export ZSH=$HOME/.oh-my-zsh
ZSH_THEME="robbyrussell"
plugins=(git command-not-found vagrant)
source $ZSH/oh-my-zsh.sh || git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh

source $HOME/.profile

# Custom aliases
alias n='nautilus . &!' # Opens nautilus and disowns it from current shell
alias ag='ag --pager less'

alias ls='ls -h --color=tty' # Human readable file sizes, and color :)

alias gdb='gdb -q'
alias peda='gdb -q -ex peda'
alias pwngdb='gdb -q -ex pwngdb'
alias pwndbg='gdb -q -ex pwndbg'
alias gef='gdb -q -ex gef'

alias fstar='fstar --query_stats --__no_positivity --include /home/jay/everest/kremlin/kremlib/'
alias fstaru='fstar --use_hints --detail_hint_replay'
alias fstarr='fstar --record_hints'
alias fstarru='fstaru --record_hints'
alias fstarur='fstarru'

alias manually-installed-to-auto='sudo apt-mark auto'

alias record-term='asciinema rec --yes -i 1 --title'

alias temax='emacs -nw'
alias cemax='emacsclient'
export ALTERNATE_EDITOR='emacs' # Opens emacs if no emacs server is
				# already started

alias ncdu='ncdu -rx' # Make ncdu safe (no delete) and fast (don't
		      # cross FS boundary)

alias clip='xclip -selection clipboard'

alias fetch-recursive-website='wget --recursive --no-parent -e robots=off'

alias screensaver='cmatrix -abs'

function gitignore() {
    wget -O .gitignore https://www.gitignore.io/api/c++,vim,ocaml,latex,emacs,python,sublimetext,visualstudio,visualstudiocode,linux,mac,windows
}

# Allow .local binaries
export PATH=~/.local/bin:$PATH

# Speed up compile times using ccache if available
if [ -d /usr/lib/ccache ]; then
    export PATH="/usr/lib/ccache:$PATH"
fi

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
test -r $HOME/.opam/opam-init/init.zsh && . $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# connect up local gem repository
export PATH="$HOME/.gem/ruby/2.3.0/bin:$PATH"

# Set stuff up to be able to switch to other versions of llvm
llvm5 () {
    export LLVM_ROOT="$HOME/llvm/llvm-5.0.1.install/"
    export PATH="${LLVM_ROOT}/bin:$PATH"
    llvm5 () { } # Prevent the command from being an issue by running
		 # too many times
}
llvm6 () {
    export LLVM_ROOT="$HOME/llvm/llvm-6.0.0.install/"
    export PATH="${LLVM_ROOT}/bin:$PATH"
    llvm5 () { } # Prevent the command from being an issue by running
		 # too many times
}

# Enable nice syntax highlighting if available
# Install via [sudo apt install zsh-syntax-highlighting]
# NOTE: This MUST be at the end of .zshrc
test -f /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh && source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
