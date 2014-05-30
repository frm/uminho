#! /bin/bash

clang -Wall -Wextra -pedantic -g -DDEBUG ../../src/aggregation.c ../../src/aggregate.c ./aggregate_test.c
./a.out

FILENAMES=()
for i in {1..8}
do
	FILENAMES+=("$i.dat")
done

for file in $FILENAMES
do
	if [ -f $file ];
		then 
			cat $file
	fi
done

