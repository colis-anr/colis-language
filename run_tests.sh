#!/bin/sh

allgood="yes"
for f in tests/*.cls; do
    if ./concrete_colis -compare -colis "$f" > /dev/null 2>&1; then
        echo "SUCCESS $(basename "$f" .cls)."
    else
        echo "FAILURE $(basename "$f" .cls)." >&2
        allgood="no"
    fi
done

if [ "$allgood" != "yes" ]; then
    exit 1
fi
