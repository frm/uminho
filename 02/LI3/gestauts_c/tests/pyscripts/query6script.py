#! /bin/python

import codecs

dict = {}

fname = input()
ch = input()
ch = ch.upper()

f = codecs.open(fname, "r", 'windows-1252')

for line in f:
    shit = line.split(", ")
    authors = shit[:-1]
    for author in authors:
        if author[0] == ch:
            dict[author] = None

for key in sorted(dict.keys()):
    print(key)

f.close()