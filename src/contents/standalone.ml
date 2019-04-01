open Contents

let infilename=Sys.argv.(1);;
let outfilename=infilename^".save";;

let t = newtable () in
    begin
      scan infilename t;
      save outfilename t
    end;;

let t = load outfilename in print t;;
