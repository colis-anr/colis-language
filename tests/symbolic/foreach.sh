for x in a$(false; echo x)$(true) b; do
    echo "1-$x"
done
echo "2-$x"

echo "End"

loop() {
    while true; do
        echo > /dev/null
    done
}

for x in a$(loop; echo x)$(true) b; do
    echo "$x"
done
echo "Not here"
