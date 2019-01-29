loop() {
    while true; do
        echo >/dev/null
    done
}

if mkdir "/x"; then
    echo "Ok"
else
    loop
fi
