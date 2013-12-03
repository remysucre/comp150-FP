#!/bin/bash

PROG=$1
TIME=$3
REPS=$2

rm -f test.txt

for (( i=1; i<=$REPS; i++ ))
    do
    touch test.txt
    timeout ${TIME} time -f %e ${PROG} > /dev/null 2>> test.txt
    VAL=$?
    if [ "${VAL}" -eq "124" ]
    then echo "-1.0" >> test.txt
    fi
done
