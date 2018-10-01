open Format
open Semantics__Behaviour
open Interpreter__Result
open Interpreter__Interpreter

let print_result fmt (res:Interpreter__Result.result) : unit =
  faiwith "print_result"

let main () =
  if Array.length Sys.argv != 1 then
    exit 2;
  let filename = Array.get Sys.argv 0 in

  let point_pos = String.rindex filename '.' in

  let ext = String.sub filename point_pos (String.length filename) in

  let statement =
    match ext with
    | "cls" ->
      FromColis.parse filename
    | "sh" ->
      FromShell.parse filename
    | _ ->
      Format.eprintf "Unknown extension: %s@." ext;
      exit 2
  in
  let res, success =
    try interp_stmt empty_input empty_context empty_state statement, true
    with EExit res -> res, false
  in
  printf "%s!@." (if success then "Normal" else "Exit");
  printf "%a@." print_result res
