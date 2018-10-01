open Format
open Semantics__Context
open Interpreter__Stdout
open Interpreter__Interpreter

let get_parser = function
  | "colis" -> FromColis.parse
  | "shell" -> FromShell.parse
  | _ ->
    eprintf "Unknown file type: %s@." filetype;
    exit 2

let main () =
  if Array.length Sys.argv != 2 then begin
    eprintf "Two arguments please: <filetype> <filename>, where <filename> is 'colis' or 'shell'";
    exit 2
  end;
  let statement = get_parser filetype filename in
  let out = stdout_new () in
  let (ctx, sta, bool), mode =
    try interp_stmt empty_input out empty_context empty_state statement, "normal"
    with EExit res -> res, "exit"
  in
  printf "%s: %b@." mode b;
  printf "STDOUT: %s@." (string_from_stdout !out)

let () = main ()
