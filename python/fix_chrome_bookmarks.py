#!/usr/bin/python
import sys
import json

id = 3
def new_id():
    global id 
    id += 1
    return id

def is_url(d):
    return d["type"] == "url"

def is_dir(d):
    return d["type"] == "folder"

def new_url(name, url):
    d = { "url"           : url
        , "date_added"    : "13016946543709863"
        , "date_modified" : "0"
        , "id"            : new_id()
        , "name"          : name
        , "type"          : "url"
        }
    return d

def new_dir(name):
    d = { "children"      : []
        , "date_added"    : "13016946543709863"
        , "date_modified" : "0"
        , "id"            : new_id()
        , "name"          : name
        , "type"          : "folder"
        }
    return d

def child_index(name, children):
    i = 0
    for child in children:
        if (child["name"] == name): break
        i += 1
    return i

def flag(path, item, inserted):
    pathstr = ""
    for entry in path:
        pathstr += (entry + "/")
    if inserted:
        print('insert "', item["name"], ' into: "', pathstr, '"', sep="")
    else:
        print('repeat "', item["name"], '"', sep="")

def insert(tree, path, item):
    node = tree
    if is_url(item): newitem = new_url(item["name"], item["url"])
    if is_dir(item): newitem = new_dir(item["name"])
    # find the proper place in the tree in which to insert the item
    for entry in path:
        # if the current entry in the path is among the node's children
        # then set that child as the current node and continue with the next entry
        # else insert this item into the current node
        children_names = [child["name"] for child in node["children"]]
        if (entry in children_names):
            node = node["children"][child_index(entry, node["children"])]
        else:
            break
    # insert it
    if is_url(newitem):
        urls = [x["url"] for x in node["children"] if is_url(x)]
        if (not item["url"] in urls):
            node["children"].append(newitem)
            inserted = True
        else:
            inserted = False
    if is_dir(newitem):
        dirs = [x["name"] for x in node["children"] if is_dir(x)]
        if (not item["name"] in dirs):
            node["children"].append(newitem)
            inserted = True
        else:
            inserted = False
    flag(path, item, inserted)

def walkjson(current_path, node, tree):
    # print("processing: ", end="")
    # for i in current_path:
    #     print(i, "/", sep="", end="")
    # print(node["name"])
    insert(tree, current_path, node)
    if is_dir(node):
        for child in node["children"]:
            walkjson(current_path + [node["name"]], child, tree)

def main(filepath):
    f = open(filepath, "r")
    j = json.loads(f.read())
    f.close()

    path = []
    node = j["roots"]["bookmark_bar"]
    tree = new_dir("bookmark_bar")

    k = { "version" : 1 
        , "roots"   : {}
        }
    k["roots"]["other"]        = new_dir("Other Bookmarks")
    k["roots"]["synced"]       = new_dir("Mobile Bookmarks")
    k["roots"]["bookmark_bar"] = tree

    walkjson(path, node, tree)

    f = open(filepath + "_clean", "w")
    f.write(json.dumps(k, sort_keys=True, indent=4, separators=[',',': ']))
    f.close()

if (__name__ == "__main__"):
    main(sys.argv[1])
