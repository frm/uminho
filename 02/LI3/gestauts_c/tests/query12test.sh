#! /bin/bash

DIR="$( cd "$( dirname "$0" )" && pwd )"

echo file
read file
echo year
read year
echo n
read n

grep $year $file > brol

echo brol > shitshitshit
echo $n >> shitshitshit

$DIR/pyscripts/query12script.py < shitshitshit

rm shitshitshit
rm brol
