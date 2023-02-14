echo $1
new_file=$1".new_file.sh"
grep -v "errorStrategy" $1 > $new_file
mv $new_file $1
echo "\n"
