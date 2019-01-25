# Don’t know how to test a bounded while loop otherwise
i=3
p=/x
while test $i -ne 0; do
    i=$(($i-1))
    mkdir $p
    p=$p/x
done
