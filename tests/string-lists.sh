ab='a b'
c=' c'
d=d
for s in "$ab$c$d e" 'f g' h i; do
	 echo $s
done
