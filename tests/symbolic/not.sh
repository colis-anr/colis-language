! false
echo "1-Here"

if ! false; then
    echo "2-Here"
else
    echo "Not here"
fi


echo "End"

loop() {
    while true; do
        echo > /dev/null
    done
}
(! loop)
echo "Not here"
