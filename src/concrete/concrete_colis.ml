open Format
open Scanf
module Context = Semantics__Context
module Buffers = Semantics__Buffers
module Interpreter = Interpreter__Interpreter

(** Parse program arguments.

    Replace with Cmdliner or so?
*)
let parse_args () =
  let parse = ref None in
  let filename = ref None in
  let oracle = ref None in
  let acccompanying = ref false in
  let speclist = [
    "-colis", Arg.Unit (fun () -> parse := Some FromColis.parse), "Use the colis parser";
    "-shell", Arg.Unit (fun () -> parse := Some FromShell.parse), "Use the shell parser";
    "-compare-with", Arg.String (fun s -> oracle := Some s), "Compare results with oracle (program exits with code 2 when the results do not correspond with the oracle)";
    "-compare", Arg.Set acccompanying, "Compare results with accompanying oracle (replacing or adding extension .oracle)";
  ] in
  let usage_msg = Sys.argv.(0) ^ " [-compare|-compare-with ORACLE] -colis|-shell FILENAME" in
  let error msg =
    eprintf "Error: %s.@." msg;
    eprintf "Usage: @?";
    Arg.usage speclist usage_msg;
    exit 1
  in
  let anon_fun s =
    if !filename <> None
    then error "Only one filename allowed";
    filename := Some s
  in
  Arg.parse speclist anon_fun usage_msg;
  let filename =
    match !filename with
    | Some filename -> filename
    | None -> error "No file specified"
  in
  let parse =
    match !parse with
    | Some filename -> filename
    | None -> error "No parser specified"
  in
  let oracle =
    match !oracle, !acccompanying with
    | Some oracle, false -> Some oracle
    | Some oracle, true -> error "Should I use the accompanying or the explicit oracle?"
    | None, false -> None
    | None, true -> begin
        try
          let dot_pos = String.rindex filename '.' in
          Some (String.sub filename 0 dot_pos ^ ".oracle")
        with Not_found ->
          Some (filename ^ ".oracle")
      end
  in
  parse, filename, oracle

type outcome = {
  result: bool;
  stdout: Semantics__Buffers.stdout;
}

let outcome_from_state sta = {
  result = !(sta.Interpreter.result);
  stdout = !(sta.Interpreter.stdout);
}

(** Print execution results *)
let print_outcome outcome =
  printf "RESULT: %b@." outcome.result;
  printf "STDOUT:@.";
  List.iter (printf "> %s@\n") (List.rev outcome.stdout)

let error fmt =
  kfprintf (fun fmt -> pp_print_flush std_formatter (); pp_print_flush err_formatter (); exit 3) err_formatter (fmt^^"@.")

(** Read an oracle file **)
let read_oracle filename : outcome =
  try
    let ic = Scanning.open_in filename in (* TODO close*)
    let result = ref None in
    let stdout = ref [] in
    bscanf ic "RESULT: %b\n" (fun b -> result := Some b);
    bscanf ic "STDOUT:\n" ();
    (try
       while true do
         (* TODO how to scan '> ' indicating empty output line *)
         bscanf ic "> %s@\n" (fun s -> stdout := s :: !stdout)
       done
     with End_of_file -> ());
    match !result, !stdout with
    | None, _ ->
      error "ORACLE: No result defined in file %s." filename
    | Some result, stdout ->
      {result; stdout}
  with
  | Sys_error msg ->
    error "ORACLE: Could not read file %s: %s." filename msg
  | Scan_failure msg ->
    error "ORACLE: Error in file %s: %s." filename msg

(** Compare execution results with oracle *)
let compare outcome oracle =
  pp_print_flush err_formatter ();
  pp_print_flush std_formatter ();
  if outcome.result <> oracle.result then begin
    eprintf "ORACLE MISMATCH RESULT: %b@." oracle.result;
    exit 3
  end;
  if outcome.stdout <> oracle.stdout then begin
    eprintf "ORACLE MISMATCH STDOUT:@.";
    List.iter (eprintf "> %s@.") (List.rev oracle.stdout);
    exit 3
  end;
  printf "ORACLE: Success."

let main () =
  let parse, filename, oracle = parse_args () in
  let program = parse filename in
  let outcome =
    let open Interpreter in
    let sta = empty_state () in
    interp_program empty_input sta program;
    outcome_from_state sta
  in
  print_outcome outcome;
  match oracle with
  | Some oracle ->
    let oracle = read_oracle oracle in
    compare outcome oracle
  | None -> ()

let () = main ()
