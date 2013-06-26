#!/usr/bin/python2
import os
import tty
import sys
import termios
import pyid3lib
import mutagen.mp3

def getch():
    fd = sys.stdin.fileno()
    old_settings = termios.tcgetattr(fd)
    try:
        tty.setraw(sys.stdin.fileno())
        ch = sys.stdin.read(1)
    finally:
        termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
    return ch

def ismp3(filename):
    _, ext = os.path.splitext(filename)
    return ext == ".mp3"

cwd    = os.getcwd()
path   = cwd.split("/")
artist = path[-2]
album  = path[-1]
year   = album[:4]
files  = filter(ismp3, os.listdir(cwd))
files.sort()
print "artist: ", artist
print "album:  ", album
print "year:   ", year
print "files:"
for filename in files:
    basename, ext = os.path.splitext(filename)
    track = basename[:2]
    name  = basename[3:]
    print "    track: ", track
    print "    name:  ", name
    print "-" * 80

print "ok? (y/n)"
ch = getch()
if ch == 'y': 
    print "you said yes! here goes..."
    for filename in files:
        basename, ext = os.path.splitext(filename)
        track = basename[:2]
        name  = basename[3:]
        try:
            mutagen_tag = mutagen.mp3.MP3(filename)
            mutagen_tag.delete()
            mutagen_tag.save()
        except:
            pass
        pyid3_tag = pyid3lib.tag(filename)
        pyid3_tag.artist = artist
        pyid3_tag.album  = album
        pyid3_tag.year   = year
        pyid3_tag.track  = track
        pyid3_tag.update()
elif ch == 'n':
    print "dinna trust, me, eh laddie?"
else:
    print "you hafta choose y or n!"

