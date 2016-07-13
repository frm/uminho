#! /bin/python

import codecs

dict = {}

fname = input()

f = codecs.open(fname, "r", 'windows-1252')

for line in f:
    year = line.split(', ')[-1]
    if year in dict:
        dict[year] += 1
    else:
        dict[year] = 1

f.close()

sl = sorted(dict.items(), key=lambda k: k[0])

for item in sl:
    print(item[0], item[1])
