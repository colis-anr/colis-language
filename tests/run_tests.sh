#!/bin/sh
set -e

allgood="yes"

error() {
    file=$1
    shift
    echo "FAILURE $file: $@" >&2
    allgood="no"
}

for file in good/*.sh good/*.cls; do
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
    oracle="good/$base.oracle"
    if ! [ -e "$oracle" ]; then
        error "$file" "Missing oracle $oracle"
        continue
    fi

    outbase=$(mktemp -u -t "$base-XXXXX")

    colisout="$outbase$extension"
    colis "--$filetype" "$file" > "$colisout" && true
    colisret=$?

    oracleout="$outbase.oracle"
    oracleret=$(head -n1 "$oracle"|sed 's/^RETURN: //')
    tail -n+3 "$oracle" | cut -c 3- > "$oracleout"

    if [ "$colisret" -ne "$oracleret" ]; then
        error "$file" "Return code mismatch with oracle ($colisret not $oracleret)";
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
