f () {
  echo "Hello" "$1!"
}

twice () {
  f $1
  f $2
}

twice 'colis' 'world'
