#!/usr/bin/zsh 
export opentabs=$(find ~/.mozilla/firefox*/*.default*/sessionstore-backups/recovery.jsonlz4);

python3 <<< $'import os, json, lz4.block
with open(os.environ["opentabs"], "rb") as f:
    magic = f.read(8)
    jdata = json.loads(lz4.block.decompress(f.read()).decode("utf-8"))
    for win in jdata["windows"]:
        for tab in win["tabs"]:
            if tab["entries"] == []:
                urls = tab["userTypedValue"]
            else:
                i = int(tab["index"]) - 1
                urls = tab["entries"][i]["url"]
            print(urls)'

