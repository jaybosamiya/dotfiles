#!/bin/sh

# for bash 4
# this will be called when a command is entered
# but not found in the user’s path + environment
command_not_found_handle () {

    # taken from http://www.linuxjournal.com/content/bash-command-not-found
    # - do not run when inside Midnight Commander or within a Pipe
    if [ -n "${MC_SID-}" ] || ! [ -t 1 ]; then
        >&2 echo "$1: command not found"
        return 127
    fi

    toplevel=nixpkgs # nixpkgs should always be available even in NixOS
    cmd=$1
    attrs=$(nix-locate --minimal --no-group --type x --type s --top-level --whole-name --at-root "/bin/$cmd")
    len=$(echo -n "$attrs" | grep -c "^")

    case $len in
        0)
            >&2 echo "$cmd: command not found"
            ;;
        1)
            >&2 /bin/cat <<EOF
The program '$cmd' is currently not installed. You can install it
by typing:
  nix-env -iA $toplevel.$attrs

Or run it once with:
  nix-shell -p $attrs --run ...
EOF
            ;;
        *)
            >&2 /bin/cat <<EOF
The program '$cmd' is currently not installed. It is provided by
several packages. You can install it by typing one of the following:
EOF

            # ensure we get each element of attrs
            # in a cross platform way
            while read attr; do
                >&2 echo "  nix-env -iA $toplevel.$attr"
            done <<< "$attrs"

            >&2 /bin/cat <<EOF

Or run it once with:
EOF

            while read attr; do
                >&2 echo "  nix-shell -p $attr --run ..."
            done <<< "$attrs"
            ;;
    esac

    return 127 # command not found should always exit with 127
}

# for zsh...
# we just pass it to the bash handler above
# apparently they work identically
command_not_found_handler () {
    command_not_found_handle $@
    return $?
}

