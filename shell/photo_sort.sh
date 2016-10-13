#!/bin/bash

for file in *.jpg
do
  dest=$(ls -l --time-style=long-iso "$file" | awk '{print $6}')
  mkdir -p $dest
  mv -i $file $dest
done
