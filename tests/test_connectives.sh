
for s in '' 'a' ; do
    if test ! -n "$s" ; then
        echo "test ! -n '$s': yes"
    else
        echo "test ! -n '$s': no"
    fi
   for t in '' 'b' ; do
       if test -n "$s" -a -n "$t" ; then
           echo "test -n '$s' -a -n '$t': yes"
       else
           echo "test -n '$s' -a -n '$t': no"
       fi
       if test -n "$s" -o -n "$t" ; then
           echo "test -n '$s' -o -n '$t': yes"
       else
           echo "test -n '$s' -o -n '$t': no"
       fi
   done
done
