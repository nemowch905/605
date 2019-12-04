#!/bin/bash

if [ -d data ]; then
    rm -r data
    mkdir data
else
    mkdir data
fi

if [ -d data_swap ]; then
    rm -r data_swap
    mkdir data_swap
else
    mkdir data_swap
fi

cd data_swap
cat ../hw4best100.csv | cut -d , -f 2 | tail -n +2 | head -n 10 | sort -k 2 -t'-' |{
    while read n;do
	folder=$(echo $n | cut -d - -f 2)
	if [ ! -d $folder ]; then
	    cd ..
	    rm -r data_swap
	    mkdir data_swap
	    cd data_swap
	    cp /home/groups/STAT605/boss/tgz/$folder.tgz $folder.tgz
	    tar zxvf $folder.tgz 1>/dev/null
	fi
	fit=$(echo $n | cut -d \" -f 2)
	cp $folder/$fit ../data/$fit
    done
}

cd ..
rm -r data_swap
