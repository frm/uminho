#! /bin/bash

DIR="$( cd "$( dirname "$0" )" && pwd )"

echo file
read file
echo name
read name

grep "$name" $file > brol

echo brol > shitshitshit

$DIR/pyscripts/query5script.py < shitshitshit

rm brol
rm shitshitshit
