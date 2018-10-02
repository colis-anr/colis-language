open Format
open Semantics__Context
open Interpreter__Interpreter

let get_parser = function
  | "colis" -> FromColis.parse
  | "shell" -> FromShell.parse
  | str ->
    eprintf "Unknown file type: %s@." str;
    exit 2

let main () =
  if Array.length Sys.argv != 3 then begin
    eprintf "Two arguments please: <filetype> <filename>, where <filename> is 'colis' or 'shell'@.";
    exit 2
  end;
  let filetype, filename = Sys.argv.(1), Sys.argv.(2) in
  let statement = get_parser filetype filename in
  let sta = empty_state () in
  let b, mode =
    try interp_stmt empty_input sta statement, "normal"
    with EExit res -> res, "exit"
  in
  printf "RESULT: %s %b@." mode b;
  printf "STDOUT: @.";
  List.iter (printf "> %s@\n") (List.rev !(sta.stdout))

let () = main ()
