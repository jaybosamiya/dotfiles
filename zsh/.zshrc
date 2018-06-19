# Need to migrate some settings from old machine
export ZSH=$HOME/.oh-my-zsh
ZSH_THEME="robbyrussell"
plugins=(git command-not-found vagrant taskwarrior)
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

alias uniquify='awk '"'"'!_[$0]++'"'" # Equivalent to uniq, but preserves order

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

alias top='htop'

alias fetch-recursive-website='wget --recursive --no-parent -e robots=off'

alias screensaver='cmatrix -abs'

alias axel='axel -a -n 10'

function dlist() { pushd ~/this-sem/Research/misc-testing/dlist/ind }
function caps() { pushd ~/this-sem/Research/CAPS-project/ }

alias t='task'
alias ta='task add'
alias tm='task modify'
alias tn='task next'
alias ts='task start'
function _tt() {
    task ready 2>/dev/null | \
	sed '/^\s*$/d' |  \
	sed '/^ID.*Age.*Description.*Urg$/d' | \
	sed '/^\(-*\s*\)*$/d' | \
	sed '/\d* tasks*$/d' | \
	awk '{$1="TODO[" $1 "]"; $2=""; print $0}' | \
	rev | awk '{$1=""; print $0}' | rev
    # TODO: Use the JSON export and write a nicer output
}
function tt() {
    _tt || echo "No tasks"
}
function td() {
    task done "$@" && tt
}
function tph() {
    task modify "$1" scheduled:"$(date -Iseconds --date='next hour')"
}
function tpt() {
    task modify "$2" scheduled:"$(date -Iseconds --date=$1)"
}
function tpd() {
    task modify "$1" scheduled:tomorrow
}

function gitignore() {
    wget -O .gitignore https://www.gitignore.io/api/c++,vim,ocaml,latex,emacs,python,sublimetext,visualstudio,visualstudiocode,linux,mac,windows
}

# Create a temporary directory, with current time until minutes, and
# link /tmp/tempdir to it
function tempdir() {
    DIR="/tmp/tmp.$(date +%F/%H-%M-%S)"
    mkdir -p "$DIR"
    if [ -d /tmp/tempdir ]; then
	rm -f /tmp/tempdir.old
	mv /tmp/tempdir /tmp/tempdir.old
    fi
    ln -s "$DIR" /tmp/tempdir
    cd /tmp/tempdir
    echo "$DIR"
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

# connect up local cabal repository
export PATH="$HOME/.cabal/bin/:$PATH"

# Show the next set up of tasks upon zsh load, if they exist; but
# don't display of recording
if [ -z "$ASCIINEMA_REC" ]; then
    _tt
    true # Prevent failure if no task exists
fi

# Colorized wdiff
function cwdiff() {
    wdiff -n -w $'\033[1;31m' -x $'\033[0m' -y $'\033[1;32m' -z $'\033[0m' -s "$1" "$2"
}

# Compare two texts, and nicely word-diff them
function 2compare() {
    A="$(mktemp /tmp/text1.XXXXXX)"
    B="$(mktemp /tmp/text2.XXXXXX)"
    echo "Enter text1 (press Enter,Ctrl+D when done):"
    cat > "$A"
    echo "Enter text2 (press Enter,Ctrl+D when done):"
    cat > "$B"
    echo "Comparing..."
    cwdiff "$A" "$B"
    rm -f "$A" "$B"
}

function duplicate-repo() {
    USAGE="Usage: duplicate-repo {from} {to}"
    [ -z "$1" ] && echo $USAGE && return 1
    [ -z "$2" ] && echo $USAGE && return 1
    FROM="$1"
    TO="$2"
    DIR="$(mktemp -d /tmp/tmp-duplicate-XXXXXX)"
    git clone --bare "$FROM" "$DIR" || return 2
    pushd "$DIR"
    git push --mirror "$TO" || (popd; return 3)
    popd
    rm -rf "$DIR"
    echo "Done duplicating repository"
}

# Enable nice syntax highlighting if available
# Install via [sudo apt install zsh-syntax-highlighting]
# NOTE: This MUST be at the end of .zshrc
test -f /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh && source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
