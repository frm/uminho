#!/bin/bash

rm *.dat
rm increment
rm agregate

GCCAGR=`gcc -std=c99 -Wall -Wextra -pedantic src/server_lib.c test/final_agr.c -o agregate`
GCCINC=`gcc -std=c99 -Wall -Wextra -pedantic src/server_lib.c test/final_inc.c -o increment`
$GCCINC
$GCCAGR

for i in {1..3000}
do
    ./increment 5 Braga Braga Real &
    ./increment 5 Braga Braga Dume &
    ./increment 5 Braga Guimarães Pevidém &
    ./increment 5 Lisboa Bairro Alto &
    ./increment 5 Lisboa Estrela Amadora &
    ./increment 5 Porto Porto Miragaia &
done

./agregate 0 brg_agr Braga
./agregate 0 lx_agr Lisboa
./agregate 0 opo_agr Porto

