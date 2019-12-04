#!/bin/bash

if [ -f hw4best100.csv ]; then
    rm hw4best100.csv
fi

if [ -d csv ]; then
    if [[ $(ls *.csv 2>/dev/null| wc -l) -ge 1 ]];then
	rm -r csv
	mkdir csv
	mv *.csv csv
    fi
else
    mkdir csv
    mv *.csv csv
fi


sort -k 1 -t',' -m -n csv/*.csv | tail -n +$(ls csv |wc -l) |head -n 101 > hw4best100.csv
