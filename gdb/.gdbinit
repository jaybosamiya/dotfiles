define peda
  source ~/.local/pwn/peda/peda.py
end

define pwndbg
  source ~/.local/pwn/pwndbg/gdbinit.py
end

define pwngdb
  peda
  source ~/.local/pwn/Pwngdb/mod_gdbinit
end

define gef
  source ~/.local/pwn/gef/gef.py
end

# Add glibc sources for malloc
#
#   Requires:
#     sudo apt-get install libc6-dbg glibc-source
#     cd /usr/src/glibc && sudo tar xvf glibc-2.24.tar.xz
#
#   Gotcha:
#     Do NOT break on `malloc` but on the `malloc` call to
#     be able to step through better
dir /usr/src/glibc/glibc-2.24/malloc
