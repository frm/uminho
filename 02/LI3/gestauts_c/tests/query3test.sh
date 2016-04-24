#! /bin/bash

echo file
read file
echo name
read name
echo year
read year

TOTAL=$(grep "$name" $file | grep $year | wc -l)

echo $TOTAL
