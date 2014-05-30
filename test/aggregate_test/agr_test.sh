#! /bin/bash

rm a.out
rm *.dat

clang -Wall -Wextra -pedantic -g -DDEBUG ../../src/aggregation.c ../../src/aggregate.c ./aggregate_test.c
./a.out

for i in {1..8}
do
	if [ -f "$i.dat" ]
		then
			echo "------ $i.dat ------"
			cat "$i.dat"
	fi
done
