function f echo ['Hello'; arg 0 '!'];
function twice begin
  call f [arg 0];
  call f [arg 1]
end;
call twice ['colis'; 'world']
