
if (exit 5); then
    echo "Not here"
else
    echo "1-Here"
fi

if (return 5); then
    echo "Not here"
else
    echo "2-Here"
fi

(exit 0)
echo "3-Here"

failure () {
    while true; do
        echo > /dev/null
    done
}

echo "4-End"

if (failure); then
    echo "Not here"
else
    echo "Not here"
fi
echo "Not here"
