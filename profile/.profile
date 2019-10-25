# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.
# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022
# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi
# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi
# everest related
export EVEREST_SCONS_CACHE_DIR=/tmp/everest
export EVERESTHOME="$HOME/everest"
export PATH="$EVERESTHOME/FStar/bin:$PATH"
export PATH="$EVERESTHOME/kremlin:$PATH"
export FSTAR_HOME="$EVERESTHOME/FStar"
export KREMLIN_HOME="$EVERESTHOME/kremlin"
export KRML_HOME="$KREMLIN_HOME"
export HACL_HOME="$EVERESTHOME/hacl-star"
export VALE_HOME="$EVERESTHOME/vale"
export MLCRYPTO_HOME="$EVERESTHOME/mlcrypto"

export PATH="$HOME/.cargo/bin:$PATH"
export PPP_EXPLOIT_LOCAL=1
# Set up "hit only" sequences for modifiers
#   Hitting only left control means escape
#   Hitting only left shift means open paren
#   Hitting only right shift means close paren
pgrep xcape >/dev/null || xcape -t 100 -e 'Caps_Lock=Escape;Shift_L=Shift_L|parenleft;Shift_R=Shift_R|parenright'
export GOPATH="$HOME/.local/gocode"

# Allow .local binaries
export PATH=~/.local/bin:$PATH

# Speed up compile times using ccache if available
if [ -d /usr/lib/ccache ]; then
    export PATH="/usr/lib/ccache:$PATH"
fi

# connect up local gem repository
export GEM_HOME="$HOME/.gem"
export PATH="$HOME/.gem/bin:$PATH"

# connect up local cabal repository
export PATH="$HOME/.cabal/bin/:$PATH"

export PATH="$HOME/.cargo/bin:$PATH"
