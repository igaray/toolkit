#!/usr/bin/python
# This script assists in removing duplicated files.
# It consumes a file containing the output of fdupes.

import os
import subprocess

import sys
import tty
import termios

def getch():
    fd = sys.stdin.fileno()
    old_settings = termios.tcgetattr(fd)
    try:
        tty.setraw(sys.stdin.fileno())
        ch = sys.stdin.read(1)
    finally:
        termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
    return ch

def allsame(files):
    same = True
    quit = False
    ext  = files[0].split('.')[-1]
    i    = 0
    while (same and not quit):
        if (files[i].split('.')[-1] != ext): same = False
        i += 1
        if (i == len(files)): quit = True
    return (same, ext)

DOCS    = ['txt', 'rtf', 'doc', 'odt', 'ods', 'pdf']
WEB     = ['htm', 'html', 'css', ]
CODE    = ['js']
IMAGES  = ['gif', 'png', 'jpg', 'jpeg']
MEDIA   = ['mp3', 'avi', 'mpg', 'mpeg', 'mp4']

IGNORED = WEB + CODE + DOCS

def ignore(files):
    same, ext = allsame(files)
    return ((same and ext in IGNORED), ext)

f = open(sys.argv[1], "r")
# read the lines in the duplicates file in batches.
while 1:
    line = f.readline()
    if not line: break

    files = []
    while (line != "\n"):
        # strip the newline from the filename
        if line.endswith('\n'): 
            line = line[:-1]
        files.append(line)
        line = f.readline()

    ignore_files, ext = ignore(files) 
    if ignore_files:
        print("ignoring files with extension:", ext)
    else:
        # show the user which files are in this batch.
        for i, filename in zip([fn for fn in range(0, len(files))], files):
            print(i, filename)
        # prompt the user for which file to keep.
        print("keep: ", end="")
        sys.stdout.flush()
        if (len(files) < 10):
            # if the amount of files is less than 10,
            # then it can be represented with a single
            # character, and an enter will not be 
            # necessary.
            ch = getch()
        else:
            # if the amount of files is more than 9, 
            # then a string is necessary to represent
            # the number and we will need an enter to
            # singal end of input.
            ch = sys.stdin.readline()
        print()
        # perform the correct action.
        if (ch[0] == 'q'):
            # q is for quit, so we quit the loop alltogether.
            print("aborting")
            break
        elif (ch[0] == 's'):
            # s is for skip, so we skip this batch.
            print("skipping")
            continue
        elif (ch[0] == 'a'):
            print("deleting all files")
            for filename in files:
                subprocess.call(["rm", filename])
        else:
            #try:
            if ch.endswith('\n'): 
                ch = ch[:-1]
            i = int(ch)
            for filename in files:
                if (filename != files[i]):
                    print("deleting", filename)
                    subprocess.call(["rm", filename])
            print()
            #except:
            #    print("error")
            #    pass
f.close()
