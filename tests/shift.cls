function f begin
  echo ['This is'; arg 0; '-'; arg 1; '-'; arg 2; '-'; arg 3; '-'; arg 4];
  shift 3;
  echo ['This is'; arg 0; '-'; arg 1; '-'; arg 2; '-'; arg 3; '-'; arg 4]
end

begin
  echo ['Here is'; arg 0; '-'; arg 1; '-'; arg 2];
  shift;
  echo ['Here is'; arg 0; '-'; arg 1; '-'; arg 2];
  s := 'Y Z W';
  call f ['X'; split s];
  call f ['X'; s]
end
