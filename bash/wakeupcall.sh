echo Sleeping for $1
sleep $1
file1=~/Music/unsorted/zen-gong.mp3
file2=~/Music/tagged/CELLO\ ROCK/Apocalyptica/1998\ Inquisition\ Symphony/01\ Harmageddon.mp3
mpg123 $file1 $file1 $file1 $file1 $file1 $file1 $file2 2> /dev/null
