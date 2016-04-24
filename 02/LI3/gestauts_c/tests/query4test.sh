#! /bin/bash

DIR="$( cd "$( dirname "$0" )" && pwd )"

echo file
read file
echo $file > BROLOLOLOLOL

$DIR/pyscripts/query4script.py < BROLOLOLOLOL

rm BROLOLOLOLOL
