function f begin
  case_var := arg 1 ;
  if
    if
      if false then
        true
      else
        test [ case_var; '='; 'upgrade' ]
      fi
    then
      true
    else
      test [ case_var; '='; 'update' ]
    fi
  then
    echo [ 'foo' ]
  else
    if
      if false then
        true
      else
        test [ case_var; '='; 'clean' ]
      fi
    then
      echo [ 'bar' ]
    else
      echo [ 'baz' ]
    fi
  fi
end

begin
  call f [ 'upgrade' ] ;
  call f [ 'clean' ] ;
  call f [ 'cleana' ] ;
  call f [ 'shproutz' ]
end
