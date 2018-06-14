#!/bin/bash
for filename in $(ls -1 *.jpg); do
	date=${filename:4:8} 
	y=${date:0:4}
	m=${date:4:2}
	d=${date:6:2}
	folder=$y-$m-$d
	echo mkdir -p $folder
	echo mv $filename $folder
done
