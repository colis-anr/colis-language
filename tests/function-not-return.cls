function f begin
  not return success;
  echo ['here']
end

begin
  if call f then
    echo ['yes']
  else
    echo ['no']
  fi
end
