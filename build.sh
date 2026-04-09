#!/usr/bin/env sh
read -p "Enter assignment number to compile and run: " i
cobc -x -Wall -o build/A${i} -I include -L lib src/ASSIGNMENT-${i}.COB && /usr/bin/env bash -c "./build/A${i}"
