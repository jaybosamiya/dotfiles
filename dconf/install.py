from sys import argv, exit

if len(argv) != 2 or argv[1][-len(".dconf"):] != ".dconf":
    print argv
    print ("Usage: %s {name}.dconf" % argv[0])
    exit(1)

# TODO: Actually install :)
print("Tried to install %s" % repr(argv[1]))
