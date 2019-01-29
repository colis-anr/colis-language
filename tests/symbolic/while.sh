p=/
while test "$p" != "/x/x/x/x/x" ; do
    p=$p/x
    mkdir $p
done
