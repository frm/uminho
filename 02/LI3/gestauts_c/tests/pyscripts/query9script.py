#! /bin/python

import codecs

fname = input()
dif = int(input())

dict = {}

f = codecs.open(fname, "r", 'windows-1252')

for line in f:
    content = line.split(", ")
    year = content[-1]
    authors = content[:-1]
    for author in authors:
        if not author in dict:
            dict[author] = {}
        if not year in dict[author]:
            dict[author][year] = None

sl = sorted(dict.items(), key=lambda k: len(k[1]), reverse=True)

i = 0

while i < len(sl) and len(sl[i][1]) == dif:
    print (sl[i][0])
    i += 1
