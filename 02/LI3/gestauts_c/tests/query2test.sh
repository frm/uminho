#! /bin/bash

DIR="$( cd "$( dirname "$0" )" && pwd )"

echo file
read file

echo $file > brol

echo DIR

$DIR/pyscripts/query2script.py < brol

rm brol
