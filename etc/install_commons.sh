#!/bin/bash

LIBS=("http://mirrors.fe.up.pt/pub/apache//commons/codec/binaries/commons-codec-1.10-bin.tar.gz"
    "http://mirrors.fe.up.pt/pub/apache//commons/lang/binaries/commons-lang3-3.3.2-bin.tar.gz"
    "http://dev.mysql.com/get/Downloads/Connector-J/mysql-connector-java-5.1.34.tar.gz")

PREFIX=$HOME/.java/lib
NAMES=("commons-codec-1.10.tar.gz" "commons-lang3-3.3.2.tar.gz" "mysql-connector.tar.gz")

mkdir -p $PREFIX

for i in {0..2}
do
    NAME=${NAMES[i]}
    LIB=${LIBS[i]}
    echo $NAME
    wget -O $PREFIX/$NAME $LIB
    tar -xzf $PREFIX/$NAME -C $PREFIX/
    rm $PREFIX/$NAME
done

