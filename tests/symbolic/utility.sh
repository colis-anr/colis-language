loop() {
    while 1; do
        echo >/dev/null
    done
}

if mkdir "/x"; then
    echo "Ok"
else
    echo "Error"
fi

if test -e "/x"; then
    echo "Always"
else
    loop
fi
