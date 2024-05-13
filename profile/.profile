# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.
# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# XDG Configuration
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# if running zsh
if [ -n "$ZSH_VERSION" ]; then
    # include iterm2 shell integration if it exists
    if [ -f "$HOME/.iterm2_shell_integration.zsh" ]; then
        . "$HOME/.iterm2_shell_integration.zsh"
    fi
fi

# Use vim as the default editor if an editor is called for
export EDITOR="vim"

if [ "$HOST" = "eden" ]; then

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

elif [ "$HOST" = "Valhalla" ]; then

    if [ "$DISPLAY" = ":0" -o "$DISPLAY" = "" ]; then
        # Set up connection to vcxsrv since we are on WSL2
        #
        # A way to guarantee that this `:0` check isn't needed is to set
        # `guiApplications=false` in C:\Users\username\.wslconfig
        # so that WSLg is disabled entirely. However, I would like this
        # to work without disabling it too, and thus I check for WSLg via `:0`
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

# connect up to cargo
if [ -f ~/.cargo/env ]; then
    . "$HOME/.cargo/env"
fi

# connect up to anyenv (https://anyenv.github.io/)
if [ -d ~/.anyenv/bin ]; then
    export PATH="$HOME/.anyenv/bin:$PATH"
    eval "$(anyenv init -)"
fi

# connect up to nix
if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi

# opam configuration
if [ -n "$ZSH_VERSION" ]; then
    test -r "$HOME/.opam/opam-init/init.zsh" && . $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
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
    [ -s "$WASMER_DIR/wasmer.sh" ] && . "$WASMER_DIR/wasmer.sh"
fi

# doom-emacs configuration
if [ -d "$HOME/.emacs.d/bin" ]; then
    export PATH="$HOME/.emacs.d/bin:$PATH"
fi

# Verus + Singular configuration
if [ -x "$HOME/.nix-profile/bin/Singular" ]; then
    export VERUS_SINGULAR_PATH="$HOME/.nix-profile/bin/Singular"
fi

# Set up auto change-dir
#
# Is on Windows Terminal folks' radar, and hopefully should be fixed
# soon. Relevant issue: https://github.com/microsoft/terminal/issues/3158
if [ "$HOST" = "Valhalla" ]; then
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

# Use Cargo's "sparse" registry protocol, to speed up accesses to crates.io
#
# TODO: Remove once Rust hits 1.70.0 which should make this default, sometime around June 2023
export CARGO_REGISTRIES_CRATES_IO_PROTOCOL=sparse

# Disable telemetry for the Eternal Terminal
export ET_NO_TELEMETRY=t

# Enable OrbStack setup if it exists
if [ -d "$HOME/.orbstack/bin" ]; then
    export PATH="$HOME/.orbstack/bin:$PATH"
fi

# Set up rye if it exists
if [ -s "$HOME/.rye/env" ]; then
    . "$HOME/.rye/env"
fi

