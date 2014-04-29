#! /bin/python

import codecs

dict = {}

fname = input()

f = codecs.open(fname, "r", 'windows-1252')

for line in f:
    shit = line.split(", ")
    year = shit[-1]
    year = year[:-1]
    if year in dict:
        dict[year] += 1
    else:
        dict[year] = 1

sl = sorted(dict.items(), key=lambda k: k[0])

for it in sl:
    print(it[0], it[1])

f.close()