#! /bin/python

import codecs

l = [0] * 3
fname = input()

f = codecs.open(fname, "r", 'windows-1252')

for line in f:
    authors = len(line.split(", ")) - 1
    if authors <= 3:
        authors -= 1
        l[authors] += 1

for i in range(0, 3):
    print(i+1, l[i])
        
