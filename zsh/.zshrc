# Early direnv initialization, required due to how powerlevel10k's instant prompt
# works. Since direnv may produce output in some directories, we need to do this
# bit early, before the instant prompt is enabled.
(( ${+commands[direnv]} )) && emulate zsh -c "$(direnv export zsh)"

# rtx (https://github.com/jdxcode/rtx) initialization to support automatically
# setting up different versions of various tools like python, node, etc.
#
# This needs to be run _after_ the direnv activation, but before the
# powerlevel10k instant prompt.
(( ${+commands[rtx]} )) && eval "$(rtx activate zsh)"

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
    source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Actually enable direnv
(( ${+commands[direnv]} )) && emulate zsh -c "$(direnv hook zsh)"

# Need to migrate some settings from old machine
export ZSH=$HOME/.oh-my-zsh
if [[ -f ~/.p10k.zsh ]]; then
    ZSH_THEME="powerlevel10k/powerlevel10k"
else
    ZSH_THEME="robbyrussell"
fi
plugins=(git command-not-found rust pass just)
source $ZSH/oh-my-zsh.sh || git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
if command -v nix-index >/dev/null; then source ~/.nix-command-not-found.sh || echo "Might want to stow nix-command-not-found for niceties"; fi

# Custom aliases
function n {
    case ${HOST%%.*} in
        eden) function n { nautilus . &! } ;;
        Valhalla) function n { explorer.exe . } ;;
        arcadia) function n { open . } ;;
        *) function n { echo "Unknown machine. n is unbound." } ;;
    esac
    n "$@"
}

alias units='units -1v' # verbose single line output for GNU units

if [[ "$HOST" == "Valhalla" ]]; then
    alias screenperm='sudo /etc/init.d/screen-cleanup start' # Needed for WSL2 when `screen` gives the "Cannot make directory '/run/screen': Permission denied" error
fi

# Make it easy to clear out any existing open SSH sockets
function ssh-clear {
    # Ask for confirmation on each file in ~/.ssh/*.sock
    rm -i ~/.ssh/*.sock
}

# A convenience function that is both `cat` and `ls`, based upon what is being looked at
function c {
    if [ "$1" = "-l" ]; then
        local LSEXTRAARGS="-l"
        shift
    elif [ "$1" = "-al" ]; then
        local LSEXTRAARGS="-al"
        shift
    fi
    if [ -e "$1" ]; then
        if [ -d "$1" ]; then
            ls $LSEXTRAARGS "$1";
        else
            cat "$1";
        fi
    else
        echo "$1 does not exist" 1>&2;
        return 1
    fi
}

# Set up zoxide if it exists to make cd easier
#
# Install using `cargo install zoxide --locked`
if command -v zoxide >/dev/null; then
    eval "$(zoxide init zsh)"
else
    function z () { 1>&2 echo 'No zoxide. Install via `cargo install zoxide --locked`'; return 1 }
    function zi () { z }
fi

if command -v rg >/dev/null; then
    function rg() {
        if [ -t 1 ]; then
            command rg -p "$@" | less -RFX
        else
            command rg "$@"
        fi
    }
fi

if command -v phd >/dev/null; then
    function phd() {
        if [ -t 1 ]; then
            command phd --color=always "$@" | less -RFX
        else
            command phd "$@"
        fi
    }
fi

# Replace `cat` with `bat` when available
if command -v bat >/dev/null; then alias cat=bat; fi

# Upload a file to allow temporary download for 24 hours
function upload_temporary() {
    if [ $# -lt 1 ]; then
        echo "Usage: $0 [--autodestroy] {filename}" 1>&2
    echo "  Uploads a file (to oshi.at) to be available for download for 24 hours" 1>&2
        echo "    --autodestroy causes file to delete itself on first download" 1>&2
           return -1
    fi
    local AUTODESTROY=""
    if [ "$1" = "--autodestroy" ]; then
        AUTODESTROY="&autodestroy=1"
    shift
    fi
    curl -T "$1" "https://oshi.at/?expire=1440${AUTODESTROY}"
}

# Make it easier to read out what/where commands are after doing a
# `which` on them
function cwhich() {
    ls -l $(which "$1");
    file $(which "$1");
    cat $(which "$1");
}

if command -v delta >/dev/null; then
    function diff() {
        # Use delta (from `cargo install git-delta`) to better colorize
        # output, when at the terminal. Also, automatically enter into a
        # pager if needed (done by delta).
        if [ -t 1 ]; then
            command diff -u "$@" | delta
        else
            command diff -u "$@"
        fi
    }
fi

if command -v youtube-dl >/dev/null; then
    alias music-dl='youtube-dl --audio-format=mp3 --extract-audio --metadata-from-title "%(artist)s - %(title)s"'
    alias twitch-dl="youtube-dl -o '%(id)s-%(title)s.%(ext)s'"
fi

case "$OSTYPE" in
    darwin*)
        alias ls='ls -h -G --color=auto' # Human readable file sizes, and color :)
        ;;
    linux*)
        alias ls='ls -h --color=tty' # Human readable file sizes, and color :)
        ;;
    *)
        echo "Unknown OS type $OSTYPE"
        ;;
esac

# Convenience function to enable coredumps
case "$OSTYPE" in
    darwin*)
        function enable_coredumps() {
            if [ -x "$1" ]; then
                if touch "/cores/tmp" && rm "/cores/tmp"; then
                    local TMP_ENTITLEMENTS="$(mktemp -d)"
                    /usr/libexec/PlistBuddy -c "Add :com.apple.security.get-task-allow bool true" "$TMP_ENTITLEMENTS/tmp.entitlements" >/dev/null
                    # Perform ad-hoc signing (not using any specific
                    # identity; this is quite restrictive, see man
                    # page for `codesign`, but prob good enough)
                    codesign --sign - --force  --entitlements "$TMP_ENTITLEMENTS/tmp.entitlements" "$1" >/dev/null 2>&1
                    rm -f "$TMP_ENTITLEMENTS/tmp.entitlements"
                    rmdir "$TMP_ENTITLEMENTS"
                    ulimit -c unlimited
                    echo "[i] Core dumping for '$1' enabled. Cores will be dumped to /cores/."
                    echo ""
                    echo "    Reminder: clean out that directory semi-regularly, since cores can be massive."
                else
                    echo "[!] Insufficient permissions on /cores. Fix by running:" 1>&2
                    echo "" 1>&2
                    echo "         sudo chmod 1777 /cores" 1>&2
                    echo "" 1>&2
                    echo "[i] To reset back to default, use 0755" 1>&2
                    return 1
                fi
            else
                echo "Usage: enable_coredumps {executable}" 1>&2
                return 1
            fi
        }
        ;;
    linux*)
        function enable_coredumps() {
            ulimit -c unlimited
        }
        ;;
    *)
        echo "Unknown OS type $OSTYPE"
        ;;
esac

if command -v gdb >/dev/null; then
    alias gdb='gdb -q'
    alias peda='gdb -q -ex peda'
    alias pwngdb='gdb -q -ex pwngdb'
    alias pwndbg='gdb -q -ex pwndbg'
    alias gef='gdb -q -ex gef'
fi

if command -v docker >/dev/null; then
    alias dockerubuntu='docker run --rm -it -v "$(pwd):/connect" --cap-add=SYS_PTRACE ubuntu' # Runs a docker container in current spot, and connects it to /connect ; enables ptrace
fi
# limactl allows nice management of nerdctl docker-like instances
# across different archs on a Mac.
#
# See the `lima` stow to have the right setup for the VMs.
#
# The `--cap-add=SYS_PTRACE` allows ptrace, thereby allowing GDB.  The
# `--security-opt seccomp=unconfined` is to allow disabling ASLR
# within GDB.
if command -v limactl >/dev/null; then
    alias limax86='limactl shell x86instance nerdctl run --rm -it --cap-add=SYS_PTRACE --security-opt seccomp=unconfined -v "$(pwd):/connect"'
    alias limaarm='limactl shell default nerdctl run --rm -it --cap-add=SYS_PTRACE --security-opt seccomp=unconfined -v "$(pwd):/connect"'
fi

if command -v fzf >/dev/null; then
    alias fzf="fzf --layout=reverse-list --multi"
fi

if command -v apt-mark >/dev/null; then
    alias manually-installed-to-auto='sudo apt-mark auto'
fi

if command -v asciinema >/dev/null; then
    alias record-term='asciinema rec --yes -i 1 --title'
    # TODO: Also look into termtosvg
fi

function e() {
    if [ $# = 0 ]; then
        emacs . &!
    elif [ "$1" = "-nw" ]; then
        echo "Don't run 'e -nw'. It'll send it into the background lol."
        local choice
        if read -q "choice?Press y to open it in foreground: "; then
            if [ $# = 1 ]; then
                emacs -nw .
            else
                emacs "$@"
            fi
        else
            echo "" 1>&2
            echo "Got '$choice'. Not doing anything..." 1>&2
            return 1
        fi
    else
        emacs "$@" &!
    fi
}

export ALTERNATE_EDITOR='emacs' # Opens emacs if no emacs server is already started

if command -v ncdu >/dev/null; then
    alias ncdu='ncdu -rx' # Make ncdu safe (no delete) and fast (don't cross FS boundary)
fi

case "$OSTYPE" in
    darwin*)
        alias clip='pbcopy'
        ;;
    linux*)
        alias clip='xclip -selection clipboard'
        ;;
    *)
        echo "Unknown OS type $OSTYPE"
        ;;
esac

if command -v axel >/dev/null; then
    alias axel='axel -a -n 10'
fi

# Make things easy to copy over into markdown/slack :)
function shcopy() {
    export PS1='\`\`\`

\`\`\`
$ '
    bash
}

function comment_aux() {
    if [ "$#" -ne 3 ]; then
        echo "Usage: comment_aux {start} {comment-text} {end}"
        return 1
    fi
    figlet "$2" | awk '{print "'$1'" $0 "'$3'"}'
}
function comment_c() { comment_aux '// ' "$(echo "$@")" '' }
function comment_cpp() { comment_aux '/* ' "$(echo "$@")" ' */' }
function comment_ocaml() { comment_aux '(* ' "$(echo "$@")" ' *)' }
function comment_shell() { comment_aux '# ' "$(echo "$@")" '' }

function public-ip() {
    curl https://f0xtr0t.xyz/ip
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
    case ${HOST%%.*} in
        eden|Valhalla)
            function waitmake() {
                while true; do
                    inotifywait -e modify -r .
                    make "$@"
                    sleep 0.1
                done
            }
            waitmake "$@"
            ;;
        arcadia)
            function waitmake() {
                while true; do
                    fswatch --one-event --exclude '\.#.*' --exclude '.*~' --recursive .
                    sleep 0.1   # Give time to kill via Ctrl-C
                    make "$@"
                    sleep 0.1
                done
            }
            waitmake "$@"
            ;;
        *)
            echo 'Unknown machine. "waitmake" is unbound.'
            ;;
    esac
}

if command -v task >/dev/null; then
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
    # Show the next set up of tasks upon zsh load, if they exist; but
    # don't display of recording
    if [ -z "$ASCIINEMA_REC" ]; then
        _tt
        true # Prevent failure if no task exists
    fi
fi

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
        cd /tmp/tempdir/$@
    else
        tempdirnew
    fi
}

if command -v wdiff >/dev/null; then
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
fi

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
# source <(cod init $$ zsh)
which cod >/dev/null && source <(cod init $$ zsh) || true

# Enable fish like auto-suggestions when available
# Install via [sudo apt install zsh-autosuggestions]
#
# Make sure not to enable them when recording for asciinema, to
# prevent accidental information leakage.
if [ -z "$ASCIINEMA_REC" ]; then
    test -f /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh && source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh
    test -f ~/.nix-profile/share/zsh-autosuggestions/zsh-autosuggestions.zsh && source ~/.nix-profile/share/zsh-autosuggestions/zsh-autosuggestions.zsh
fi

# Enable nice syntax highlighting if available
# Install via [sudo apt install zsh-syntax-highlighting]
# NOTE: This MUST be at the end of .zshrc
test -f /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh && source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
test -f ~/.nix-profile/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh && source ~/.nix-profile/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
