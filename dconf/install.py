from sys import argv, exit
from os import system

if len(argv) != 2 or argv[1][-len(".dconf"):] != ".dconf":
    print argv
    print ("Usage: %s {name}.dconf" % argv[0])
    exit(1)

ret = system('cat "' + argv[1] + '" | dconf load /')

if ret == 0:
    print("[+] Done installing %s" % repr(argv[1]))
else:
    print("[!] Failed installing %s" % repr(argv[1]))
