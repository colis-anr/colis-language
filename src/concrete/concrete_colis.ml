open Format
open Scanf
open Semantics__Context
open Interpreter__Interpreter

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
    "-shell", Arg.Unit (fun () -> parse := Some FromColis.parse), "Use the shell parser";
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

(** Print execution results *)
let print_result behaviour value stdout =
  printf "BEHAVIOUR: %s@." behaviour;
  printf "VALUE: %b@." value;
  printf "STDOUT:@.";
  List.iter (printf "%s@\n") (List.rev stdout)

(** Read an oracle file **)
let read_oracle filename =
  let ic = Scanning.open_in filename in
  let behaviour = ref "" in
  let value = ref true in
  let stdout = ref [] in
  bscanf ic "BEHAVIOUR: %s\n" (fun s -> behaviour := s);
  bscanf ic "VALUE: %b\n" (fun b -> value := b);
  bscanf ic "STDOUT:\n" ();
  begin
    try
      while true do
        bscanf ic "%s\n" (fun s -> stdout := s :: !stdout)
      done
    with
      End_of_file -> ()
  end;
  !behaviour, !value, !stdout

(** Compare execution results with oracle *)
let compare beh value stdout beh' value' stdout' =
  pp_print_flush err_formatter ();
  pp_print_flush std_formatter ();
  let error fmt =
    kfprintf (fun fmt -> pp_print_flush fmt (); exit 3) err_formatter ("ORACLE: "^^fmt)
  in
  let pp_sep fmt () = pp_print_string fmt "\\n" in
  if beh <> beh' then
    error "Behaviour did't match (%s)" beh';
  if value <> value' then
    error "Value did't match (%b)" value';
  if stdout <> stdout' then
    error "Stdout did't match (oracle: \"%a\" (%d lines))"
      (pp_print_list ~pp_sep pp_print_string) (List.rev stdout')
      (List.length stdout');
  printf "ORACLE: Success."

let main () =
  let parse, filename, oracle = parse_args () in
  let statement = parse filename in
  let sta = empty_state () in
  let beh, value =
    try
      "normal", interp_stmt empty_input sta statement
    with EExit res ->
      "exit", res
  in
  print_result beh value !(sta.stdout);
  match oracle with
  | None -> ()
  | Some oracle ->
    let beh1, value1, stdout = read_oracle oracle in
    compare beh value !(sta.stdout) beh1 value1 stdout

let () = main ()
