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

if [[ "$HOST" == "eden" ]]; then

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

    # Set up "hit only" sequences for modifiers
    #   Hitting only left control means escape
    #   Hitting only left shift means open paren
    #   Hitting only right shift means close paren
    pgrep xcape >/dev/null || xcape -t 100 -e 'Caps_Lock=Escape;Shift_L=Shift_L|parenleft;Shift_R=Shift_R|parenright'

elif [[ "$HOST" == "Valhalla" ]]; then

    if [[ "$DISPLAY" == "" ]]; then
	# Set up connection to vcxsrv since we are on WSL2
        export DISPLAY=$(grep -oP "(?<=nameserver ).+" /etc/resolv.conf):0.0
        export LIBGL_ALWAYS_INDIRECT=1
    else
	# We got a DISPLAY variable, likely from an incoming SSH connection. Honor that.
	true
    fi

fi

# Speed up compile times using ccache if available
if [ -d /usr/lib/ccache ]; then
    export PATH="/usr/lib/ccache:$PATH"
fi

# Connect to Go
export GOPATH="$HOME/.local/gocode"

# Allow .local binaries
export PATH="$HOME/.local/bin:$PATH"

# connect up local gem repository
export GEM_HOME="$HOME/.gem"
export PATH="$HOME/.gem/bin:$PATH"

# connect up local cabal repository
export PATH="$HOME/.cabal/bin/:$PATH"

export PATH="$HOME/.cargo/bin:$PATH"

# connect up to nix
if [ -e /home/jay/.nix-profile/etc/profile.d/nix.sh ]; then . /home/jay/.nix-profile/etc/profile.d/nix.sh; fi

# opam configuration
if [ -n "$ZSH_VERSION" ]; then
    test -r $HOME/.opam/opam-init/init.zsh && . $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
fi

# wasmtime configuration
if [ -d "$HOME/.wasmtime" ]; then
    export WASMTIME_HOME="$HOME/.wasmtime"
    export PATH="$WASMTIME_HOME/bin:$PATH"
fi

# lean configuration -- https://leanprover-community.github.io/install/debian_details.html
if [ -d "$HOME/.elan" ]; then
    export PATH="$HOME/.elan/bin:$PATH"
fi

# Wasmer configuration
if [ -d "$HOME/.wasmer" ]; then
    export WASMER_DIR="/home/jay/.wasmer"
    [ -s "$WASMER_DIR/wasmer.sh" ] && source "$WASMER_DIR/wasmer.sh"
fi

# pyenv configuration
if [ -d "$HOME/.pyenv" ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init --path)"
fi

# Set up auto change-dir
#
# Is on Windows Terminal folks' radar, and hopefully should be fixed
# soon. Relevant issue: https://github.com/microsoft/terminal/issues/3158
if [[ "$HOST" == "Valhalla" ]]; then
    # Run only on zsh
    if [ -n "$ZSH_VERSION" ]; then
        # Run only on interactive shells
        if [[ $- == *i* ]]; then
            if [ -r "/dev/shm/.cwd-$USER" ]; then
                cd "$(<"/dev/shm/.cwd-$USER")" || true
            else
                touch "/dev/shm/.cwd-$USER"
                chmod 600 "/dev/shm/.cwd-$USER"
            fi
            __store_cwd () {
                pwd > "/dev/shm/.cwd-$USER"
            }
            chpwd_functions+=(__store_cwd) # fine on zsh, but flycheck doesn't like it, so ignore the squigglies
            function __no_store_cwd() {
                # Remove store cwd from chpwd_functions
                #
                # TODO: Is this syntax OK to keep it an array, or does
                # it end up becoming a string?
                chpwd_functions=${chpwd_functions:#__store_cwd}
            }
        fi
    fi
fi
