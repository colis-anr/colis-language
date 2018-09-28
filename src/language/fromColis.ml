open Format
open Syntax__Syntax

let print_position fmt p =
  fprintf fmt "File '%s', line %d, character %d"
    p.Lexing.pos_fname p.Lexing.pos_lnum (p.Lexing.pos_cnum - p.Lexing.pos_bol)

let parse filename : statement =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p
                                with Lexing.pos_fname = filename };
  try
    ColisParser.statement ColisLexer.token lexbuf
  with
  | ColisLexer.LexerError s ->
    printf "Lexing error: %s@." s;
    exit 2
  | ColisParser.Error ->
    printf "Parsing error: %a@."
      print_position lexbuf.Lexing.lex_start_p;
    exit 2
