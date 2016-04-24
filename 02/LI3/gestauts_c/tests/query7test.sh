#! /bin/bash

echo file
read file
echo min
read min
echo max
read max

sum=0

for i in $(eval echo {$min..$max})
    do
        sum=$(expr $sum + $(grep $i $file | wc -l) )
    done

echo $sum