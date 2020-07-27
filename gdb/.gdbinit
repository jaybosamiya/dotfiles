define peda
  source ~/.local/pwn/peda/peda.py
  set follow-fork-mode parent
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

# Enable gdb history, and make it persist only until restart; but
# store an unlimited number of commands
set history save on
set history size -1
set history filename /tmp/.gdb_history

# Disable pagination. Prevents the annoying "press enter for more"
# style stuff.
set pagination off

# Add glibc sources for malloc
#
#   Requires:
#     sudo apt-get install libc6-dbg glibc-source
#     cd /usr/src/glibc && sudo tar xvf glibc-2.24.tar.xz
#
#   Gotcha:
#     Do NOT break on `malloc` but on the `malloc` call to
#     be able to step through better
# dir /usr/src/glibc/glibc-2.24/malloc

# Add in the "exploitable" package. Given a crash, running
# "exploitable" gives an analysis of whether the crash _might_ be
# exploitable.
# source /usr/local/lib/python2.7/dist-packages/exploitable-1.32-py2.7.egg/exploitable/exploitable.py
