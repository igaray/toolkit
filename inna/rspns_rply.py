#!/usr/bin/python
import sys

USAGE = """
$ ./rspns_rply v Hello, tesseractee, I've missed your interdimensional presence.
eo eeaee e ie you ieieioa eee

$ ./rspns_rply c Hello, tesseractee, I've missed your interdimensional presence.
Hll, tssrct, I'v mssd r ntrdmnsnl prsnc.
"""

def usage():
    print(USAGE)

def vowel(c):
    return c in "aeiou"

def consonant(c):
    return not vowel(c)

def v(s):
    return " ".join(["".join([c for c in w if vowel(c)]) for w in s])

def c(s):
    return " ".join(["".join([c for c in w if consonant(c)]) for w in s])

def main(argv):
    if len(argv) < 3:
        usage(l)
    elif argv[1] == "v":
        print(v(argv[2:]))
    elif argv[1] == "c":
        print(c(argv[2:]))
    else:
        usage()

if __name__ == "__main__":
    main(sys.argv)