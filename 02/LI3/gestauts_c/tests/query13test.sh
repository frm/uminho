#! /bin/bash

echo file
read file
echo author
read author
echo year
read year

yearcontent=$(grep $year $file)
yeartotal=$(echo "$yearcontent" | wc -l )
authortotal=$(echo "$yearcontent" | grep "$author" | wc -l)

bc -l <<< "$authortotal/$yeartotal"
