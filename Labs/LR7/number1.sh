#!/bin/bash
i=0
>"output.txt"
>"error.txt"
while [ $i -lt 1 ] ; do
    st=$(date +%s)
    $1>>"output.txt" 2>>"error.txt"
    end=$(date +%s)
    duration=$(($end - $st))
    waiting=$(($(("$2" * 60 * ((duration - 1 + "$2" * 60) \
                                 / ("$2" * 60)))) - $duration))
    if [ $waiting -gt 0 ] ; then
        sleep $waiting
    fi
done
