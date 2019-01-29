
if test -f '/bin'; then
    echo '/bin exists and is a regular file'
else
    echo '/bin does not exist or is not a regular file'
fi

if [ -f '/home' ]; then
    echo '/home exists and is a regular file'
else
    echo '/home does not exist or is not a regular file'
fi
