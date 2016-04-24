#! /bin/bash

DIR="$( cd "$( dirname "$0" )" && pwd )"

echo file
read file
echo name
read name

grep "$name" $file | sed "s/$name, //g" > brol

echo brol > shitshitshit

$DIR/pyscripts/query8script.py < shitshitshit

rm brol
rm shitshitshit