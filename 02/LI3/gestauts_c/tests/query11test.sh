#! /bin/bash

DIR="$( cd "$( dirname "$0" )" && pwd )"

echo file
read file

echo $file > brol
echo brol > shitshitshit

$DIR/pyscripts/query11script.py < brol

rm brol
rm shitshitshit
