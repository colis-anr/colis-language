begin
x := 'A';
pipe
  x := x 'B'
into begin
  echo ['X1: ' x];
  x := x 'C'
end endpipe;
echo ['X2: ' x]
end
