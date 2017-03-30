#!/usr/bin/python
import os
import hashlib
import pickle
import argparse
import sys

DBFILE = "/home/igaray/.elefant.db"

def init_db():
    db = {}
    if os.path.exists(DBFILE):
        with open(DBFILE, "rb") as dbfile:
            db = pickle.load(dbfile)
        print("db exists with {} elements".format(len(db)))
    return db

def save_db(db):
    with open(DBFILE, "wb") as dbfile:
        pickle.dump(db, dbfile)

def file_digest(fullpath):
    hash = hashlib.md5()
    with open(fullpath, "rb") as f:
        for chunk in iter(lambda: f.read(4096), b""):
            hash.update(chunk)
    return hash.hexdigest()

def check(db, filename):
    fullpath = os.path.abspath(filename)
    digest = file_digest(fullpath)
    if digest in db:
        size, existing = db[digest]
        print("file {} ({}) exists as: {}".format(fullpath, digest, existing))
    else:
        print("file does not exist")

def update(db):
    root_dir = "."
    for dir_name, subdirs, files in os.walk(root_dir):
        for filename in files:
            fullpath = os.path.abspath(dir_name + "/" + filename)
            size = os.path.getsize(fullpath)
            digest = file_digest(fullpath)
            if digest not in db:
                db[digest] = (size, fullpath)
                print("+", end="", flush=True)
            else: 
                print(".", end="", flush=True)
    print()

def remove(db, filename):
    fullpath = os.path.abspath(filename)
    digest = file_digest(fullpath)
    print("removing file {} (digest {}) from db".format(fullpath, digest))
    if digest in db:
        del db[digest]

def watch(db):
    while True:
        pass

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('mode', choices=['check', 'remove', 'update', 'watch'])
    parser.add_argument('file', default=None, nargs='?')
    return parser.parse_args()

def main(args):
    filename = args.file

    db = init_db()
    if args.mode == 'check':
        check(db, filename)
    elif args.mode == 'remove':
        remove(db, filename)
    elif args.mode == 'update':
        update(db)
    elif args.mode == 'watch':
        watch(db)
    save_db(db)

if __name__ == "__main__":
    args = parse_args()
    main(args)
