#!/bin/bash

col=0
rec(){
    if [ -d "$1" ]; then
        for name in $(ls "$1"); do
           rec $1/$name
        done
    else
        if [[ $1 == *.c || $1 == *.h ]]; then
      col=$(($col + $(grep -v ^$ $1 | grep -c $ )))
        fi  
    fi
    return $col
}

rec "$1"
echo $col
