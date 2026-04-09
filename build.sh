#!/usr/bin/env sh
if [ -z $1 ]; then
    read -p "Enter assignment number to compile and run: " i
else
    i=$1
fi
cobc -x -Wall -o build/A${i} -I include -L lib src/ASSIGNMENT-${i}.COB && /usr/bin/env bash -c "./build/A${i}"
