

for s in '' 'a' 'bc'; do
   for t in '' 'a' 'bc'; do
       if test "$s" = "$t" ; then
           echo "strings '$s' and '$t' pass the '=' test"
       else
           echo "strings '$s' and '$t' do not pass the '=' test"
       fi
       if test "$s" != "$t" ; then
           echo "strings '$s' and '$t' pass the '!=' test"
       else
           echo "strings '$s' and '$t' do not pass the '!=' test"
       fi
   done
done
