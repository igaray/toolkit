fun() {
    file=$1
    dir=$2
    if [ -z $dir ]
    then
        echo variable not set
    else
        echo variable set $file $dir
        mkdir -p $dir
        cd $dir
    fi
}

fun file
fun file directory
