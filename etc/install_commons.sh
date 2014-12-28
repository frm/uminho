#!/bin/bash

LIB=https://www.dropbox.com/s/8wcnjmh2hx54wvh/commons-codec-1.10-bin.tar.gz?dl=0

PREFIX=$HOME/.java/lib
NAME=commons.tar.gz

mkdir -p $PREFIX
wget -O $PREFIX/$NAME $LIB
tar -xzf $PREFIX/$NAME -C $PREFIX/
rm $PREFIX/$NAME

