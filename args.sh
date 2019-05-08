f () {
  echo 1: $1
  echo "$@"
  echo 2: $2
  echo 3: $3
}

f "$@"
