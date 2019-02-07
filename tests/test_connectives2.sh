
if test ! -e '/bin'; then
    echo "true"
else
    echo "false"
fi

if test -e '/bin' -a -e '/bin/sh'; then
    echo "true"
else
    echo "false"
fi

if test -e '/bin' -o -e '/bin/sh'; then
    echo "true"
else
    echo "false"
fi
