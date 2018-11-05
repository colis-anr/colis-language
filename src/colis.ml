open Format

module AST = Syntax__Syntax
module ColisParser = ColisParser
module ColisLexer = ColisLexer
module FromShell = FromShell

(* CoLiS *)

exception ParseError of string * Lexing.position
exception ConversionError of string

type colis = AST.program

let colis_from_lexbuf ?(filename="-") lexbuf =
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p
    with Lexing.pos_fname = filename };
  try
    ColisParser.program ColisLexer.token lexbuf
  with
  | ColisLexer.LexerError s ->
     let pos = lexbuf.Lexing.lex_curr_p in
     raise (ParseError (s, pos))
  | ColisParser.Error ->
     let pos = lexbuf.Lexing.lex_curr_p in
     raise (ParseError ("", pos))

let colis_from_channel ?(filename="-") channel =
  let lexbuf = Lexing.from_channel channel in
  colis_from_lexbuf ~filename lexbuf

let colis_from_file filename =
  let ic = open_in filename in
  try
    let colis = colis_from_channel ~filename ic in
    close_in ic;
    colis
  with
    exn -> close_in ic; raise exn

let colis_from_string string =
  let lexbuf = Lexing.from_string string in
  colis_from_lexbuf lexbuf

let pp_print_colis = ToColis.program

let print_colis =
  pp_print_colis (Format.formatter_of_out_channel stdout)

let colis_to_string colis =
  let buf = Buffer.create 8 in
  let fmt = Format.formatter_of_buffer buf in
  pp_print_colis fmt colis;
  Buffer.contents buf

let colis_to_file filename colis =
  let ochan = open_out filename in
  let fmt = Format.formatter_of_out_channel ochan in
  pp_print_colis fmt colis;
  close_out ochan

(* Shell *)

type shell = Morsmall.AST.program

let shell_from_file file =
  try
    Morsmall.parse_file file
  with
    Morsmall.SyntaxError pos ->
    raise (ParseError ("", pos))

let shell_to_colis shell =
  try
    FromShell.program__to__program shell
  with
    FromShell.Unsupported feat ->
    raise (ConversionError ("unsupported feature: " ^ feat))

(* Interpret *)

let run ~argument0 ?(arguments=[]) colis =
  let open Interpreter__Interpreter in
  let input = { empty_input with argument0 } in
  let state = empty_state () in
  state.arguments := arguments;
  interp_program input state colis;
  print_string (!(state.stdout) |> List.rev |> String.concat "\n");
  exit (if !(state.result) then 0 else 1)

let run_symbolic ?(arguments=[]) program =
  let open Format in
  let open Semantics__Context in
  let open SymbolicInterpreter__Definitions in
  let open SymbolicInterpreter__Interpreter in
  let input = { empty_input with arguments=Array.of_list arguments } in
  let state = empty_state in
  let res = interp_program input state program in
  printf "** Exit states: %d@\n" (BatSet.cardinal res.normal);
  printf "** Normal states: %d@\n" (BatSet.cardinal res.exit)

let run_symbexec ?(arguments=[]) program =
  let open Format in
  let open Semantics__Context in
  let open Symbexec__Definitions in
  let open Symbexec__Interpreter in
  let open Symbexec_printers in
  let input = { empty_input with arguments=Array.of_list arguments } in
  let state = empty_state "root" in
  let res = interp_program input state program in
  printf "** Exit states@\n";
  List.iter (printf "@\n- @[%a@]@\n" print_state) (BatSet.to_list res.exit);
  printf "@\n** Normal states@\n";
  List.iter (printf "@\n- @[%a@]@\n" print_state) (BatSet.to_list res.normal)
