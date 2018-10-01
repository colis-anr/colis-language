(* File colis_lexer.mll *)
{
open ColisParser        (* The type token is defined in colis_parser.mli *)
exception LexerError of string
}
let lalpha = ['a'-'z' '_']
let ualpha = ['A'-'Z']
let alpha = lalpha | ualpha
let digit = ['0'-'9']

rule token = parse
  | "(*"                                { comment 1 lexbuf }
  | "*)"                                { raise (LexerError ("mismatched *)")) }
  | ":="                                { ASSTRING }
  | "begin"                             { BEGIN }
  | "do"                                { DO }
  | "done"                              { DONE }
  | "else"                              { ELSE }
  | "embed"                             { EMBED }
  | "end"                               { END }
  | "epip"                              { EPIP }
  | "exit"                              { EXIT }
  | "failure"                           { FAILURE }
  | "fi"                                { FI }
  | "for"                               { FOR }
  | "if"                                { IF }
  | "in"                                { IN }
  | "into"                              { INTO }
  | "not"                               { NOT }
  | "nop"                               { NOP }
  | "pipe"                              { PIPE }
  | "previous"                          { PREVIOUS }
  | "process"                           { PROCESS }
  | "success"                           { SUCCESS }
  | "then"                              { THEN }
  | "while"                             { WHILE }
  | '{'                                 { LACCOL }
  | '('                                 { LPAREN }
  | ')'                                 { RPAREN }
  | ';'                                 { PTVIRG }
  | '['                                 { LCROCH }
  | '\''                                { let b = Buffer.create 10 in string b lexbuf }
  | '\n'                                { Lexing.new_line lexbuf; token lexbuf }
  | ']'                                 { RCROCH }
  | '}'                                 { RACCOL }
  | (alpha (alpha | digit | '_')* as v) { VAR_NAME (v) }
  | ['\t' ' ']                          { token lexbuf }     (* skip tab and blank*)
  | _ as c                              { raise (LexerError ("unknown character '" ^ String.make 1 c ^ "'")) }

and string b = parse
  | '\''                                { LITERAL (Buffer.contents b) }
  | [^'\\''\''] as c                    { Buffer.add_char b c ; string b lexbuf }
  | '\\' (_ as c)                       { Buffer.add_char b c ; string b lexbuf }

and comment n = parse
  | "(*"                                { comment (n+1) lexbuf }
  | "*)"                                { if n=1 then token lexbuf else comment (n-1) lexbuf }
  | _                                   { comment n lexbuf }
