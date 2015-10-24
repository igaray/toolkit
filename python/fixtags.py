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

# This is the list of tags the mutagen EasyID3 class can handle:
# album albumartistsort albumsort arranger artist artistsort asin author barcode
# bpm compilation composer composersort conductor copyright date discnumber
# discsubtitle encodedby genre isrc length lyricist media mood
# musicbrainz_albumartistid musicbrainz_albumid musicbrainz_albumstatus
# musicbrainz_albumtype musicbrainz_artistid musicbrainz_discid
# musicbrainz_trackid musicbrainz_trmid musicip_fingerprint musicip_puid
# organization performer performer:* releasecountry replaygain_*_gain
# replaygain_*_peak title titlesort tracknumber version website 

import datetime
import os
import tty
import sys
import termios
import time
import argparse
from mutagen.id3 import ID3, TPE1, TALB, TDRC, TRCK, TIT2

VERSION = 19
NO_MP3_FILES = "I cannae find any mp3 files in the current directory!"
YES_MSG      = "You said aye! here we go..."
NO_MSG       = "Dinna trust, me, eh laddie?"
UNKNOWN_MSG  = "You hafta choose y or n!"
MUTAGEN_ERR  = "Error while processing"

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

def tag(filename, artist, album, year):
    basename, ext = os.path.splitext(filename)
    track = basename[:2]
    title = basename[3:]
    t0 = datetime.datetime.now()
    try:
        mutagen_tag = ID3()
        mutagen_tag.add(TPE1(encoding=3, text=artist))
        mutagen_tag.add(TALB(encoding=3, text=album))
        mutagen_tag.add(TDRC(encoding=3, text=year))
        mutagen_tag.add(TRCK(encoding=3, text=track))
        mutagen_tag.add(TIT2(encoding=3, text=title))
        mutagen_tag.save(filename)
    except Exception, e:
        print MUTAGEN_ERR, filename, ":", e
    t1 = datetime.datetime.now()
    print track, str(t1 - t0)

def process_file(filename, artist, album, year):
    tag(filename, artist, album, year)

def get_params(options, path):
    if options.multicd:
        if options.categorized:
            artist = path[-4]
        else:
            artist = path[-3]
        album  = path[-2][5:] + " " + path[-1]
        year   = path[-2][:4]
    else:
        if options.categorized:
            artist = path[-3]
        else:
            artist = path[-2]
        album  = path[-1][5:]
        year   = path[-1][:4]
    return (artist, album, year)

def main(options):
    #print "version:", VERSION
    cwd    = unicode( os.getcwd(), "utf-8" )
    path   = cwd.split("/")

    artist, album, year = get_params(options, path)

    files = filter(ismp3, os.listdir(cwd))
    files.sort()
    if not files:
        print NO_MP3_FILES
        exit()

    print "artist: ", artist
    print "album:  ", album
    print "year:   ", year
    print "track: title:"
    for filename in files:
        basename, ext = os.path.splitext(filename)
        track    = basename[:2]
        name     = basename[3:]
        print "   ", track, name
    print "-" * 80
    print "ok? (y/n)"
    ch = getch()
    if ch == 'y':
        print YES_MSG
        for filename in files:
            process_file(filename, artist, album, year)
        print
    elif ch == 'n':
        print NO_MSG
    else:
        print UNKNOWN_MSG

if (__name__ == "__main__"):
    parser = argparse.ArgumentParser(description = "Clean id3 tags of mp3 files in current directory.")
    parser.add_argument("-m", dest="multicd", action='store_true')
    parser.add_argument("-c", dest="categorized", action='store_true')
    args = parser.parse_args()
    main(args)
