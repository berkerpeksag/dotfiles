#!/bin/bash

#Cache folder in home directory
cache=~/.killingfloor/Cache
#System folder in Killing Floor folder
sys=~/.local/share/Steam/SteamApps/common/KillingFloor/System

while read line; do

#Get Cache file name from cache.ini
cn=$(echo $line | awk -F"=" '{ print $1 }')
#Get Real file name from cache.ini
rn=$(echo $line | awk -F"=" '{ print $2 }')
#Get file extension
ext=$(echo $rn | awk -F"." '{ print $2 }')

if [ "$ext" != "rom" ]  && [ "$cn" != "[Cache]" ] && [ "$cn" != "" ] ; then
        #Move file with real name to system folder
        mv $cache/$cn.uxx $sys/$rn >/dev/null 2>&1
        #Clear the line
        sed -i "s/$line//" $cache/cache.ini
fi

done < $cache/cache.ini

#Remove blank lines
sed -i "/^$/d" $cache/cache.ini
