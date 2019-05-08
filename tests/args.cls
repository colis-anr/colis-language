function f begin
  echo [ '1:'; split arg 1 ] ;
  echo [ '2:'; split arg 2 ] ;
  echo [ '3:'; split arg 3 ]
end
begin
  call f [ 'foo'; 'bar'; 'baz' ]
end
