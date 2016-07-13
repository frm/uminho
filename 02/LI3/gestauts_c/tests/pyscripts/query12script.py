#! /bin/python

import codecs

dict = {}

fname = input()
n = int(input())

f = codecs.open(fname, "r", 'windows-1252')

for line in f:
    authors = line.split(', ')[:-1]
    for author in authors:
        if author in dict:
            dict[author] += 1
        else:
            dict[author] = 1

f.close()

sl = sorted(dict.items(), key=lambda k: k[1], reverse=True)

for i in range(0, n):
    print(sl[i][0], sl[i][1])


