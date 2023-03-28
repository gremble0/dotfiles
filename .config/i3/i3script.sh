#!/bin/bash
# Prepends keyboard layout to i3status
i3status --config ~/.config/i3/i3status.conf | while :
do
    read line
    LG=$(xset -q | grep LED | awk '{ print $10 }')
    if [ $LG == "00000000" ]
    then
        LAYOUT="[{ \"full_text\": \"US\"},"
    else
        LAYOUT="[{ \"full_text\": \"NO\"},"
    fi
    echo "${line/[/$LAYOUT}" || exit 1
done
