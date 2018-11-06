function f begin
  pipe false into true epip;
  pipe exit success into true epip;
  pipe exit failure into true epip;
  echo ['OK']
end

begin
  call f
end
