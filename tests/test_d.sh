
if test -d '/bin'; then
    echo '/bin exists and is a directory'
else
    echo '/bin does not exist or is not a directory'
fi

if [ -d '/home' ]; then
    echo '/home exists and is a directory'
else
    echo '/home does not exist or is not a directory'
fi
