

for s in '' 'a'; do
    if test -n "$s" ; then
        echo "string '$s' passes the -n test"
    else
        echo "string '$s' do not pass the -n test"
    fi
    if test -z "$s" ; then
        echo "string '$s' passes the -z test"
    else
        echo "string '$s' do not pass the -z test"
    fi
done
