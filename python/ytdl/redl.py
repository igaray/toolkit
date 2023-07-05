import os
import re

ID = re.compile(r"(.*)-(.{11})\.mp4", re.IGNORECASE)

def build_db():
    """
    """
    all = set()
    for root, _dirs, files in os.walk("./"):
        for filename in files:
            _, ext = os.path.splitext(filename)
            if ".mp4" == ext:
                m = ID.match(filename)
                if m:
                    (_, id) = m.groups()
                    all.add(id)
    return all

def download_files(all):
    for id in all:
        exit_code = os.system("youtube-dl --skip-download --write-description --write-info-json -o '%(channel)s-%(upload_date)s-%(title)s-%(id)s.%(ext)s' 'https://www.youtube.com/watch?v={}'".format(id))
        if exit_code != 0:
            print("Error downloading video {}".format(id))

def main():
    """
    """
    print("Running")
    all = build_db()
    download_files(all)
    print("Completed")
    

if __name__ == "__main__":
    main()
