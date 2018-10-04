begin
echo ['hello'];
pipe
  begin
    echo ['bonjour'] ;
    echo ['vert'] ;
    echo ['bonheur'] ;
    echo ['livre'] ;
    echo ['lovelace'] ;
    echo ['bon'] ;
    echo ['bonsoir']
  end
into
  grep ['bon']
epip
end
