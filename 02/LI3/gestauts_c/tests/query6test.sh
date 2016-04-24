#! /bin/bash

DIR="$( cd "$( dirname "$0" )" && pwd )"

echo file
read file
echo initial
read initial

echo $file > shitshitshit
echo $initial >> shitshitshit

$DIR/pyscripts/query6script.py < shitshitshit

rm shitshitshit