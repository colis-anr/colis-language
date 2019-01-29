x=a
echo "1-$x"

x=a$(echo b)
echo "2-$x"

if x=a$(false); then
    echo No
else
    echo "3-$x"
fi

x=a$(false)$(true)
echo "4-$x"

