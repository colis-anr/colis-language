begin
x := 'A';
pipe
  x := x 'B'
into
  x := x 'C'
epip;
echo ['X: 'X x]
end
