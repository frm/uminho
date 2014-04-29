#! /bin/python

import codecs 

dict = {}

fname = input()

f = codecs.open(fname, "r", 'windows-1252')

for line in f:
    content = line.split(', ')
    authors = len(content) - 1
    year = content[-1]
    if year in dict:
        if authors in dict[year]:
            dict[year][authors] += 1
        else:
            dict[year][authors] = 1
    else:
        dict[year] = {}
        dict[year][authors] = 1

sorted_years = sorted(dict.items(), key=lambda k: k[0])
years = []
for year in sorted_years:
    l = list(year)
    l[1] = sorted(year[1].items(), key=lambda k: k[0])
    years.append(l)

output = open("out.csv", "w")

output.write('"Year","#Authors","Publications"\n')
for year in years:
    for (n, t) in year[1]:
        s = '"' + year[0][:-1] + '","' + str(n) + '","' + str(t) + '"\n'
        output.write(s)

output.close()

