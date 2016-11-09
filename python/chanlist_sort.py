#!/usr/bin/python
# -*- coding: utf-8 -*-
# 
# Chanlist sorter
# Author: Iñaki Garay
# Date: 14/11/2010
# This script was written to sort a file containing the list of channels.
#
# To generate the input data in weechat, make sure it is logging, and request
# a list of all channels by issuing the command /list
# Extract the list from the logfile and save it in a text file.
# Pass the name of the text file as a parameter to this script, and the 
# name of the output file as the second parameter.
import sys
import re

if (len(sys.argv) != 3):
    print("Use: chanlist_sort input.txt output.txt")
else:
    ifile = open(sys.argv[1], "r")
    ofile = open(sys.argv[2], "w")
    pattern = re.compile(r"#(.*?)\((.*?)\): (.*)")
    results = []

    for line in ifile:
        match = pattern.search(line)
        if (match):
            try:
                # This is in case the regular expression picks something that 
                # is not a valid number representation in the population field.
                # Shit happens, and it makes your code ugly.
                channel_name, population, description = match.groups()
                results.append((int(population), channel_name, description))
            except ValueError:
                pass

    results.sort(reverse = True)
    for chan in results:
        #                 channel_name       population        description
        ofile.write("#" + chan[1] + "("+ str(chan[0]) + "):" + chan[2] + "\n")

    ifile.close()
    ofile.close()
