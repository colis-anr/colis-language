begin
  process begin
    echo ['A'];
    echo ['B'];
    echo ['-n'; 'C']
  end;
  process begin
    echo ['D'];
    echo ['-n'; 'E'];
    echo ['F']
  end
end
