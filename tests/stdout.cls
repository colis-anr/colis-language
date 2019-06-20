begin
  process begin
    echo ['A'];
    echo ['B'];
    echo ['-n'; 'C']
  end endprocess;
  process
    echo ['D'];
    echo ['-n'; 'E'];
    echo ['F']
  endprocess
end
