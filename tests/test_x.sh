
for i in '/bin/ls' '/etc/passwd' ; do
    if test -x "$i" ; then
        echo "$i"' exists and is executable'
    else
        echo "$i"' does not exist or is not executable'
    fi
done
