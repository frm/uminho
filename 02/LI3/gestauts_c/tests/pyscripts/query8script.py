#! /bin/python

import codecs

dict = {}
fname = input()

f = codecs.open(fname, "r", 'windows-1252')

for line in f:
    authors = line.split(", ")[:-1]
    for author in authors:
        if author in dict:
            dict[author] += 1
        else:
            dict[author] = 1

sl = sorted(dict.items(), key=lambda k: k[1], reverse=True)
top = sl[0][1]

i = 0

while i < len(sl) and sl[i][1] == top:
    print (sl[i][0], sl[i][1])
    i += 1

f.close()