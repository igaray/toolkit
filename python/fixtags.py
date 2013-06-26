#!/usr/bin/python2

# This script requires mutagen and pyid3lib.
# To use it, put it in your path, or copy it to a folder containing your mp3 files.
# It makes the following assumptions, all of which are reasonable in MY collection.
# - Every filename is of the form "NN <title>.mp3", 
#   where NN is the track number leading 0, and <title> is the title of the song.
# - The files in the current folder all correspond to the same album.
# - The current folder name is of the form "NNNN <album>", 
#   where NNNN is the year the album was released and <album> is the name of the album.
# - The current folder's parent folder is the name of the artist.
# - You like having the year included in the album name, so that retarded mp3 players sort
#   albums in the correct order (chronologically, not alphabetically).
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
    print "you said aye! here we go..."
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

