#! /bin/bash

DIR="$( cd "$( dirname "$0" )" && pwd )"

echo file
read file
echo year
read year

grep "$year" $file > brol

echo brol > shitshitshit

$DIR/pyscripts/query10script.py < shitshitshit

rm brol
rm shitshitshit
