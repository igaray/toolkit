#!/bin/bash

for file in $(ls -1 *.*); do
	exiforigdate="$(exiftool -dateformat "%Y-%m-%d" -DateTimeOriginal $file | awk '{print $4}')"
	date=$exiforigdate
	if [ -z "$date" ] 
	then
		filedate=$(stat -t "%Y-%m-%d" $file | awk '{print $10}')
		date=$filedate
	fi
	#echo $file: $exifmediadate / $exiforigdate / $filedate / \[ $date \]
	mkdir -p $date
	mv -i "$file" $date
done
