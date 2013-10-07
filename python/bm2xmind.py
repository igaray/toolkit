# This script consumes a chrome Bookmarks file and prints the content of the content.xml 
# of an xmind file. To use it, create an empty xmind file. Make sure it is not open in xmind.
# Change the extension from .xmind to .zip and extract it to its own folder.
# Get your chrome Bookmarks file.
# Run: python bm2xmind Bookmarks > content.xml
# Replace the content.xml of the extracted xmind file with the output of bm2xmind.
# Re-zip the file and change the extension back to .xmind.
# Open in xmind.
# If all went well, you should now have a xmindmap with your bookmarked links.
import json
import random
import sys
import time
import xml.etree.ElementTree as ET

def current_time():
    return str(int(round(time.time() * 1000)))

def create_id():
    node_id = random.randint(0, (10 ** 26 - 1))
    return "{0:026d}".format(node_id)

def parse_bookmarks():
    with open("bookmarks.json", "r") as f:
        return json.loads(f.read())

def walk_bookmarks(bookmarks_json, xml_parent):
    if is_link(bookmarks_json):
        add_link(xml_parent, bookmarks_json["name"], bookmarks_json["url"])
    if is_folder(bookmarks_json):
        folder = add_folder(xml_parent, bookmarks_json["name"])
        for child in bookmarks_json["children"]:
            walk_bookmarks(child, folder)

def is_folder(json):
    return (json["type"] == "folder")

def is_link(json):
    return (json["type"] == "url")

def add_children(parent):
    # if parent has children subnode
    #     then add it to children
    #     else create children subnode and add folder to it
    children = parent.find('children')
    if (children == None):
        children = ET.SubElement(parent, 'children')
        topics = ET.SubElement(children, 'topics', {"type":"attached"})
    else:
        topics = children.find('topics')
        if (topics == None):
            topics = ET.SubElement('topics', {"type":"attached"})
    return topics


def add_folder(parent, name):
    topics = add_children(parent)
    attrs = {"id":create_id(), "timestamp":current_time()}
    folder_node = ET.SubElement(topics, 'topic', attrs)
    folder_node_title = ET.SubElement(folder_node, 'title')
    folder_node_title.text = name
    return folder_node

def add_link(parent, name, url):
    topics = add_children(parent)
    attrs = {"id":create_id(), "timestamp":current_time(), "xlink:href":url}
    link_node = ET.SubElement(topics, 'topic', attrs)
    link_node_title = ET.SubElement(link_node, 'title')
    link_node_title.text = name
    return link_node

def main():
    # PARSE BOOKMARKS
    bookmarks_json = parse_bookmarks()

    # OUTPUT XMIND
    # - CREATE BASE XML TREE
    # xmap content
    #     sheet
    #         title
    #         children
    #             topics
    xmap_content_attrs = { "xmlns"       : "urn:xmind:xmap:xmlns:content:2.0",
                           "xmlns:fo"    : "http://www.w3.org/1999/XSL/Format",
                           "xmlns:svg"   : "http://www.w3.org/2000/svg",
                           "xmlns:xhtml" : "http://www.w3.org/1999/xhtml",
                           "xmlns:xlink" : "http://www.w3.org/1999/xlink",
                           "version"     : "2.0",
                           "timestamp"   : current_time()
                         }
    xmap_content = ET.Element('xmap-content', xmap_content_attrs)
    sheet_attrs = {"id":create_id(), "timestamp":current_time()}
    sheet = ET.SubElement(xmap_content, 'sheet', sheet_attrs)
    sheet_title = ET.SubElement(sheet, 'title')
    sheet_title.text = "Chrome Bookmarks"
    root_topic_attrs = {"id":create_id(), "timestamp":current_time()}
    root_topic = ET.SubElement(sheet, 'topic', root_topic_attrs)
    root_topic_title = ET.SubElement(root_topic, 'title')
    root_topic_title.text = "Chrome Bookmarks"
    tree = ET.ElementTree(xmap_content)

    # - WALK JSON AND CONVERT TO XML
    walk_bookmarks(bookmarks_json["roots"]["bookmark_bar"], root_topic)
    ET.dump(tree)

if (__name__ == "__main__"):
    main()
