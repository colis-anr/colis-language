begin
  if true then echo ['A'] fi;
  if false then echo ['B'] fi;
  if true then echo ['C'] else echo ['D'] fi;
  if false then echo ['E'] else echo ['F'] fi;
  if exit failure then true fi
end