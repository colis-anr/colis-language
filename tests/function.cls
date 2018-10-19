function f begin
  echo ['Hello'; arg 0 '!']
end

function twice begin
  call f [arg 0];
  call f [arg 1]
end

begin
  call twice ['colis'; 'world']
end
