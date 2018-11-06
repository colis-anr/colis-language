function greet begin
  echo ['Hello'; arg 1 '!']
end

function twice begin
  echo ['Function:'; arg 0];
  call greet [arg 1];
  call greet [arg 2]
end

function butfirst begin
  shift;
  echo ['Function:'; arg 0];
  call greet [arg 1];
  call greet [arg 2]
end

begin
  (* echo ['Program:'; arg 0]; *)
  call twice ['colis'; 'world'];
  call butfirst ['you'; 'colis'; 'world']
end
