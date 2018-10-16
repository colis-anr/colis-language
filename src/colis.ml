open Format

module AST = Syntax__Syntax

let print_position fmt p =
  fprintf fmt "File '%s', line %d, character %d"
    p.Lexing.pos_fname p.Lexing.pos_lnum (p.Lexing.pos_cnum - p.Lexing.pos_bol)

(* CoLiS *)

type colis = AST.program

let parse_colis filename =
  let ic = open_in filename in (* TODO close *)
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p
    with Lexing.pos_fname = filename };
  try
    ColisParser.program ColisLexer.token lexbuf
  with
  | ColisLexer.LexerError s ->
     printf "Lexing error: %s@." s;
     exit 2
  | ColisParser.Error ->
     printf "Parsing error: %a@."
       print_position lexbuf.Lexing.lex_start_p;
     exit 2

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

let parse_shell filename =
  try
    Morsmall.parse_file filename
  with
  | Morsmall.SyntaxError _pos ->
     printf "Syntax error";
     exit 2

let shell_to_colis shell =
  try
    FromShell.program__to__program shell
  with
    FromShell.Unsupported feat ->
    printf "Unsupported feature: %s" feat;
    exit 3

         (* Interpret *)

let run ?(arguments=[]) colis =
  let open Interpreter__Interpreter in
  let state = empty_state () in
  let input = { empty_input with arguments = Array.of_list arguments } in
  interp_program input state colis;
  print_string (!(state.stdout) |> List.rev |> String.concat "\n");
  exit (if !(state.result) then 0 else 1)
