#!/bin/bash
set -e

COLIS="./concrete_colis"

allgood="yes"

error() {
    file=$1
    shift
    echo "FAILURE $file: $@" >&2
    allgood="no"
}

for file in tests/*.sh tests/*.cls; do
    case "$file" in
        *.sh)
            filetype="shell"
            extension=".sh"
            ;;
        *.cls)
            filetype="colis"
            extension=".cls"
            ;;
        *)
            error "$file" "Unknown filetype"
            continue
            ;;
    esac
    base=$(basename "$file" $extension)

    # Derive oracle file name
    oracle="tests/$base.oracle"
    if ! [ -e "$oracle" ]; then
        error "$file" "Missing oracle $oracle"
        continue
    fi

    outbase=$(mktemp -u -t "$base-XXXXX")

    colisout="$outbase$extension"
    "$COLIS" "-$filetype" "$file" > "$colisout" && true
    colisret=$?

    case $colisret in
        0) colisresult="true" ;;
        1) colisresult="false" ;;
        *) error "$file" "Other colis error ($colisret)"
           continue ;;
    esac

    oracleout="$outbase.oracle"
    oracleresult=$(head -n1 "$oracle"|sed 's/^RESULT: //')
    tail -n+3 "$oracle" | cut -c 3- > "$oracleout"

    if [ "$colisresult" != "$oracleresult" ]; then
        error "$file" "Result mismatch with oracle ($colisresult not $oracleresult)";
        continue
    fi

    if ! diff -q "$colisout" "$oracleout" > /dev/null; then
        error "$file" "Stdout mismatch with oracle";
        continue
    fi

    echo "SUCCESS $file"
done

if [ "$allgood" != "yes" ]; then
    exit 1
fi
