#!/bin/bash

# cache folder in home directory
cache="$HOME/.killingfloor/Cache"

# system folder in Killing Floor folder
sys="$HOME/.local/share/Steam/SteamApps/common/KillingFloor/System"

while read line
do
    # get Cache file name from cache.ini
    cn=$(echo "$line" | awk -F"=" '{ print $1 }')

    # get Real file name from cache.ini
    rn=$(echo "$line" | awk -F"=" '{ print $2 }')

    # get file extension
    ext=$(echo "$rn" | awk -F"." '{ print $2 }')

    if [ "$ext" != "rom" ]  && [ "$cn" != "[Cache]" ] && [ "$cn" != "" ] ; then
        # move file with real name to system folder
        mv "$cache"/"$cn".uxx "$sys"/"$rn" >/dev/null 2>&1
        # clear the line
        sed -i "s/$line//" "$cache"/cache.ini
    fi
done < "$cache"/cache.ini

# remove blank lines
sed -i "/^$/d" "$cache"/cache.ini
