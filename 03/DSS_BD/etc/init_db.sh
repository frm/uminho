#!/bin/bash
# NOTE: run this as '. ./init_db.sh'

echo "Setting up environment variables..."
export HBT_USR='habitat'
export HBT_PW='testuser123'
echo "Done!"

function init_osx () {
    echo "### OS X system ###"
    echo "Environment variables are set."
    echo "Please start MySQL Server in System Preferences";
}

function init_linux () {
    if which systemctl > /dev/null
    then
        SYS="Arch"
        SQL_CMD="sudo systemctl start mysqld.service"

    elif [ -f /etc/init.d/mysql ]
    then
        SYS="Debian"
        SQL_CMD="sudo /etc/init.d/mysql start"
    else
        echo "ERROR: Unknown Operating System or MySQL Server not installed!"
    fi

    echo "\n### $SYS based system ###"
    echo "Starting MySQL Server..."
    eval $SQL_CMD
    echo "Done!"

    echo "\nDatabase is running and you now have the correct variable config."

}


if [[ `uname -s` == "Darwin" ]]
then
init_osx
else
init_linux
fi
