# a failure anyway in the program propagates
failure () {
    while true; do
        echo > /dev/null
    done
}
f () {
  (failure)
}
echo "Not here"
if s="$(f)"; then
  echo "Not here"
else
  echo "Not here"
fi
