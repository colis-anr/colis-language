begin
x := 'A';
pipe
  x := x 'B'
into begin
  echo ['X1: ' x];
  x := x 'C'
end epip;
echo ['X2: ' x]
end
