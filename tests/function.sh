greet () {
  echo "Hello" "$1!"
}

twice () {
  echo Function: $0
  greet $1
  greet $2
}

butfirst () {
    shift
    echo Function: $0
    greet $1
    greet $2
}

echo Program: $0
twice 'colis' 'world'
butfirst 'you' 'colis' 'world'
