begin
  x := 'A';
  nooutput begin x := 'B'; echo ['ignore'] end;
  echo [x]
end
