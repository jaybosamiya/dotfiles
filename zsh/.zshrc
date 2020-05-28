# Need to migrate some settings from old machine
export ZSH=$HOME/.oh-my-zsh
ZSH_THEME="robbyrussell"
plugins=(git command-not-found vagrant taskwarrior rust cargo)
source $ZSH/oh-my-zsh.sh || git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh

# Custom aliases
alias n='nautilus . &!' # Opens nautilus and disowns it from current shell
alias ag='ag --pager less'

function rg() {
    if [ -t 1 ]; then
        command rg -p "$@" | less -RFX
    else
        command rg "$@"
    fi
}

alias music-dl='youtube-dl --audio-format=mp3 --extract-audio --metadata-from-title "%(artist)s - %(title)s"'

alias ls='ls -h --color=tty' # Human readable file sizes, and color :)

alias gdb='gdb -q'
alias peda='gdb -q -ex peda'
alias pwngdb='gdb -q -ex pwngdb'
alias pwndbg='gdb -q -ex pwndbg'
alias gef='gdb -q -ex gef'

alias uniquify='awk '"'"'!_[$0]++'"'" # Equivalent to uniq, but preserves order

alias dockerubuntu='docker run --rm -it -v "$(pwd):/connect" --cap-add=SYS_PTRACE ubuntu' # Runs a docker container in current spot, and connects it to /connect ; enables ptrace

alias fzf="fzf --layout=reverse-list --multi"

alias fstar='fstar --query_stats --__no_positivity --include /home/jay/everest/kremlin/kremlib/'
alias fstaru='fstar --use_hints --detail_hint_replay'
alias fstarr='fstar --record_hints'
alias fstarru='fstaru --record_hints'
alias fstarur='fstarru'

function fstar-profile() {
    if [ "$#" -ne 2 ]; then
	echo "Usage: fstar-profile {fst file} {admit except this}"
	return 1
    fi
    if test -n "$(find . -maxdepth 1 -name 'queries-*.smt2' -print -quit)"
    then
	echo "Pre existing smt2 files found. Quitting".
	return 1
    fi
    echo "\033[1m\033[4mRunning F*\033[0m";
    fstar.exe --query_stats --admit_except "${2}" --log_queries "${1}"
    for i in queries-*.smt2; do
	echo "\033[1m\033[4mProfiling ${i}\033[0m";
	z3 smt.case_split=3 smt.relevancy=2 model=true auto_config=false smt.qi.profile=true "${i}" |& grep quantifier_instances | sort -t ':' -k 2 | tee "${i}.profiled"
    done
}

alias manually-installed-to-auto='sudo apt-mark auto'

alias record-term='asciinema rec --yes -i 1 --title'
# TODO: Also look into termtosvg

alias haven='ssh haven'
alias proxyhaven='echo "Port 10000. Ctrl+C to stop proxy" && ssh -D 10000 -C -q -N haven'
alias diavola='ssh jayb@diavola.andrew.cmu.edu'
alias vsekardesk='ssh jayb@vsekardesk.ece.cmu.edu'

alias aurora='ssh jayb@aurorar8-0d73.wv.cc.cmu.edu'
alias xaurora='xpra attach ssh:jayb@aurorar8-0d73.wv.cc.cmu.edu:13'
function xaurora-start() {
    xpra start ssh:jayb@aurorar8-0d73.wv.cc.cmu.edu --start="$1"
}

alias temax='emacs -nw'
alias cemax='emacsclient'
function e() {
    emacs "$@" &!
}
function ef() {
    X="$(fzf)"
    if [ -n "$X" ]; then
	e "$X"
    else
	false
    fi
}
export ALTERNATE_EDITOR='emacs' # Opens emacs if no emacs server is
				# already started

alias ncdu='ncdu -rx' # Make ncdu safe (no delete) and fast (don't
		      # cross FS boundary)

alias clip='xclip -selection clipboard'

alias top='htop'

alias fetch-recursive-website='wget --recursive --no-parent -e robots=off'

alias screensaver='cmatrix -abs'

alias axel='axel -a -n 10'

# Make things easy to copy over into markdown/slack :)
function shcopy() {
    export PS1='\`\`\`

\`\`\`
$ '
}

function comment_aux() {
    if [ "$#" -ne 3 ]; then
	echo "Usage: comment_aux {start} {comment-text} {end}"
	return 1
    fi
    figlet "$2" | awk '{print "'$1'" $0 "'$3'"}'
}
function comment_c() {
    comment_aux '// ' "$(echo "$@")" ''
}
function comment_cpp() {
    comment_aux '/* ' "$(echo "$@")" ' */'
}
function comment_ocaml() {
    comment_aux '(* ' "$(echo "$@")" ' *)'
}
function comment_shell() {
    comment_aux '# ' "$(echo "$@")" ''
}

function public-ip() {
    curl https://ipinfo.io/ip
}

function latexmakefile() {
    # Drop the super nice Makefile I've written into current directory
    wget --no-clobber https://raw.githubusercontent.com/jaybosamiya/latex-paper-template/master/Makefile
}

function dockermakefile() {
    # Drop the super nice Makefile I've written into current directory
    wget --no-clobber https://git.jaybosamiya.com/build/docker-makefile/raw/branch/master/Makefile
}

function waitmake() {
    while true; do
	inotifywait -e modify -r .
	make "$@"
	sleep 0.1
    done
}

function latexwaitmake() {
    yes q | waitmake
}

function pdfsmaller() {
    case $1 in
	vlow)
	    SET=screen
	    ;;
	low)
	    SET=ebook
	    ;;
	high)
	    SET=printer
	    ;;
	supercolor)
	    SET=prepress
	    ;;
	*)
	    echo "Usage: "
	    echo "  $0 [vlow|low|high|supercolor] input.pdf output.pdf"
	    echo ""
	    echo "Types:"
	    echo "  vlow       : 72 dpi images : screen-view-only quality"
	    echo "  low        : 150 dpi images"
	    echo "  high       : 300 dpi images"
	    echo "  supercolor : 300 dpi images : color preserving"
	    return
	    ;;
    esac
    gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/${SET} -dNOPAUSE -dQUIET -dBATCH -sOutputFile=${3} ${2}
}

function dlist() { pushd ~/this-sem/Research/dlist/ind/ }
function caps() { pushd ~/this-sem/Research/CAPS-project/ }

function rc() { emacs ~/.zshrc && source ~/.zshrc }

alias t='task'
alias ta='task add'
alias tm='task modify'
alias tn='task next'
alias ts='task start'
function _tt() {
    task ready 2>/dev/null | \
        awk '$1~/^[0-9]+$/{$1="TODO[" $1 "]"; $2=""; print $0}' | \
	rev | awk '{$1=""; print $0}' | rev
    # TODO: Use the JSON export and write a nicer output
}
function tt() {
    _tt || echo "No tasks"
}
function td() {
    task done "$@" && tt
}
alias tph='task modify scheduled:"$(date -Iseconds --date='"'"'next hour'"'"')"'
function tpt() {
    task modify "$2" scheduled:"$(date -Iseconds --date=$1)"
}
alias tpd='task modify scheduled:tomorrow'
function tpw() {
    task modify "$1" scheduled:"$(date -Iseconds --date='next week')"
}

function gitignore() {
    curl -L -s --output .gitignore https://www.gitignore.io/api/c++,vim,ocaml,latex,emacs,python,sublimetext,visualstudio,visualstudiocode,linux,mac,windows
}

# Create a temporary directory, with current time until minutes, and
# link /tmp/tempdir to it
function tempdirnew() {
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
# Jump to tempdir
function tempdir() {
    if [ -d /tmp/tempdir ]; then
	cd /tmp/tempdir
    else
	tempdirnew
    fi
}

# # ntfy integration
# eval "$(ntfy shell-integration)"
# mobile-send () {
#     k="$@[*]";
#     k=$(if [ -z "$k" ]; then echo "ping"; else echo "$k"; fi);
#     ntfy -b insta send "$k";
# }
# mobile-when-finished () {
#     k="${history[$HISTCMD]}";
#     k="${k//; mobile-when-finished/}";
#     k="${k//;mobile-when-finished/}";
#     k="${k//&& mobile-when-finished}";
#     k="${k//&&mobile-when-finished}";
#     mobile-send "$k";
# }

xbox-bluetooth () {
    echo 1 | sudo tee /sys/module/bluetooth/parameters/disable_ertm
}

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

# Enable the cod completion daemon
#
# See https://github.com/dim-an/cod
source <(cod init $$ zsh)

# Enable fish like auto-suggestions when available
# Install via [sudo apt install zsh-autosuggestions]
test -f /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh && source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh

# Enable nice syntax highlighting if available
# Install via [sudo apt install zsh-syntax-highlighting]
# NOTE: This MUST be at the end of .zshrc
test -f /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh && source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
