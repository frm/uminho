#! /bin/bash

DIR="$( cd "$( dirname "$0" )" && pwd )"

echo file
read file
echo min
read min
echo max
read max

dif=$(expr $max - $min + 1)
str=$min
min=$(expr $min + 1)

for i in $(eval echo {$min..$max})
    do
        str="$str\|$i"
    done

grep "$str" $file > brol

echo brol > shitshitshit
echo $dif >> shitshitshit

$DIR/pyscripts/query9script.py < shitshitshit

rm brol
rm shitshitshit