function greet begin
  echo ['Hello'; arg 1 '!']
end

function twice begin
  call greet [arg 1];
  call greet [arg 2]
end

function butfirst begin
  shift;
  call greet [arg 1];
  call greet [arg 2]
end

begin
  call twice ['colis'; 'world'];
  call butfirst ['you'; 'colis'; 'world']
end
