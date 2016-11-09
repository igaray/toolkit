#!/usr/bin/python
import sys

usage_str = """
Error: wrong number of arguments.
Usage:
print_book STARTPAGE ENDPAGE

"""

if __name__ == "__main__":
    if (len(sys.argv) != 3):
        print(usage_str)
        exit()

    page            = int(sys.argv[1]) # start page goes here
    pages           = int(sys.argv[2]) # end page + 1 goes here
    regular_pages   = []
    inverted_pages  = []
    invert          = False

    while (page <= pages):
        if (invert):
            inverted_pages.append(page)
            page += 1
            inverted_pages.append(page)
            page += 1
        else:
            regular_pages.append(page)
            page += 1
            regular_pages.append(page)
            page += 1
        invert = not invert

    i = 1
    print("Regular pages:")
    for page in regular_pages:
        print(str(page), end = "")
        if (i == 6):
            i = 1
            print()
        else:
            print(",", end="")
            i += 1   
    print()
    print()

    #i = 1
    #print("Inverted pages:")
    #for page in inverted_pages:
    #    print(str(page) + ",", end = "")
    #    if (i == 6):
    #        i = 1
    #        print()
    #    else:
    #        i += 1   
    #print()
    #print()

    print("Inverted pages:")
    inverted_pages.reverse()
    i = 0
    m = 1
    while (i < len(inverted_pages)):
        print(str(inverted_pages[i+1]) + ",", end = "")
        if (m == 3):
            print(str(inverted_pages[i]))
            m = 1
        else:
            print(str(inverted_pages[i]) + ",", end = "")
            m += 1   
        i += 2
    print()
