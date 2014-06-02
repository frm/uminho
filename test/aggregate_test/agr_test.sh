#! /bin/bash

rm a.out
rm *.dat

gcc -std=c99 -Wall -Wextra -pedantic -g -DDEBUG ../../src/aggregation.c ../../src/aggregate.c ../../includes/strutil.c ./aggregate_test.c
./a.out

for i in {1..8}
do
	if [ -f "$i.dat" ]
		then
			echo "------ $i.dat ------"
			cat "$i.dat"
	fi
done
