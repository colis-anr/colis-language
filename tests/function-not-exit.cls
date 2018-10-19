function f begin
  not exit success;
  echo ['here']
end

begin
  if call f then
    echo ['yes']
  else
    echo ['no']
  fi
end