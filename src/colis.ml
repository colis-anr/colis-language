open Format

module AST = Syntax__Syntax
module ColisParser = ColisParser
module ColisLexer = ColisLexer
module FromShell = FromShell

(* CoLiS *)

type colis = AST.program

let colis_from_file filename =
  let ic = open_in filename in (* TODO close when exception *)
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p
    with Lexing.pos_fname = filename };
  let colis = ColisParser.program ColisLexer.token lexbuf in
  close_in ic;
  colis

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

let shell_from_file = Morsmall.parse_file

let shell_to_colis = FromShell.program__to__program

(* Interpret *)

let run ?(arguments=[]) colis =
  let open Interpreter__Interpreter in
  let state = empty_state () in
  let input = { empty_input with arguments = Array.of_list arguments } in
  interp_program input state colis;
  print_string (!(state.stdout) |> List.rev |> String.concat "\n");
  exit (if !(state.result) then 0 else 1)
