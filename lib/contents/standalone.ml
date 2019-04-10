open Contents

let infilename=Sys.argv.(1)
let outfilename=infilename^".save"
let info m = begin print_string m; print_newline () end;;

let t = newtable () in
    begin
      info "Start parsing contents";
      scan infilename t;
      info "Save contents to file";
      save outfilename t
    end;;

info "Reload Contents from file";;
let t = load outfilename in print t;;
