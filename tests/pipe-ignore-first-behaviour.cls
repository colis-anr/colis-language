function f begin
  pipe false into true endpipe;
  pipe exit success into true endpipe;
  pipe exit failure into true endpipe;
  echo ['OK']
end

begin
  call f
end
