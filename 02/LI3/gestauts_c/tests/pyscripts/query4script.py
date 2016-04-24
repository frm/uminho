#!/usr/bin/python

import codecs

dict = {}

fname = input()

f = codecs.open(fname, "r", 'windows-1252')

for line in f:
    authors = line.split(", ")
    authors = authors[:-1]
    for author in authors:
        if author in dict:
            if dict[author] == 1:
                if len(authors) > 1:
                    dict[author] = 0
        else:
            if len(authors) > 1:
                dict[author] = 0
            else:
                dict[author] = 1
f.close()

t = 0

for val in dict.values():
    t += val

print(t)
