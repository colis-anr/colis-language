failure() {
    while true; do
        echo >/dev/null
    done
}

if mkdir x; then
    mkdir x
else
    failure
fi
