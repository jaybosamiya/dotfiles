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
