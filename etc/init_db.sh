#!/bin/bash
# NOTE: run this as '. ./init_db.sh'

echo "Setting up environment variables..."
export HBT_USR='habitat_admin'
export HBT_PW='testuser123'
echo "Done!"

if [[ `uname -s` == "DARWIN" ]];
then
    echo "### OS X system ###"
    echo "Environment variables are set."
    echo "Please start MySQL Server in System Preferences";
    exit
fi

if which systemctl > /dev/null;
then
    SYS="Arch"
    SQL_CMD="sudo systemctl start mysqld.service"

elif [ -f /etc/init.d/mysql ];
then
    SYS="Debian"
    SQL_CMD="sudo /etc/init.d/mysql start"

else
    echo "ERROR: Unknown Operating System or MySQL Server not installed!"
    exit
fi


echo "\n### $SYS based system ###"
echo "Starting MySQL Server..."
eval $SQL_CMD
echo "Done!"

echo "\nDatabase is running and you now have the correct variable config."
