 
function foo begin
  false ;
  echo ['here']
end

begin
  if call foo then
    echo ['yes']
  else
    echo ['no']
  fi
end