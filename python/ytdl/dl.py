import os
import re

ID = re.compile(r"(.*)-(.{11})\.mp4", re.IGNORECASE)

def build_db():
    """
    build db:
        for every file in the data folder
            extract date and id and put it in a channel dictionary
        for every file in other folders
            extract id and put in downloaded dictionary

    """
    all = {}
    dl = set()
    for _root, _dirs, files in os.walk("./data"):
        for file in files:
            # example expected filename: "20200909-fUfoOEDZqbU.info.json"
            # these files were generated by the command
            # "youtube-dl -o '%(upload_date)s-%(id)s.%(ext)s' --write-info-json --skip-download CHANNEL_URL"
            dateid = file.split('.')[0] # discard ".info.json"
            date = dateid[:8]
            id = dateid[9:]
            all[id] = date
    for root, _dirs, files in os.walk("./"):
        if root not in ["./data"]:
            for filename in files:
                _, ext = os.path.splitext(filename)
                if ".mp4" == ext:
                    m = ID.match(filename)
                    if m:
                        (_, id) = m.groups()
                        dl.add(id)
    print("{} files in total, {} downloaded, {} missing".format(len(all), len(dl), len(all) - len(dl)))
    return all, dl

def download_files(all, dl):
    for id in all:
        if id not in dl:
            print("Video {} not downloaded".format(id))
            exit_code = os.system("youtube-dl -f best --write-description --write-info-json -o '%(channel)s-%(upload_date)s-%(title)s-%(id)s.%(ext)s' 'https://www.youtube.com/watch?v={}'".format(id))
            if exit_code != 0:
                print("Error downloading video {}".format(id))

def main():
    """
    download files:
    for every id in the channel folder;
        if the id is not in the downloaded dictionary
            download it with youtube-dl
    """
    print("Running")
    all, dl = build_db()
    download_files(all, dl)
    print("Completed")
    

if __name__ == "__main__":
    main()
