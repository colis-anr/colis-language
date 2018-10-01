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
  | "(*"                                        { comment 1 lexbuf }
  | "*)"                                        { raise (LexerError ("mismatched *)")) }
  | '\''                                        { let b = Buffer.create 10 in
                                                  string b lexbuf }
  | ['\t' ' ']                                  { token lexbuf }     (* skip tab and blank*)
  | '\n'                                        { Lexing.new_line lexbuf; token lexbuf }
  | "var"                                       { VAR }
  | "success"                                   { SUCCESS }
  | "failure"                                   { FAILURE }
  | "previous"                                  { PREVIOUS }
  | "exit"                                      { EXIT }
  | "not"                                       { NOT }
  | "if"                                        { IF }
  | "then"                                      { THEN }
  | "else"                                      { ELSE }
  | "fi"                                        { FI }
  | "for"                                       { FOR }
  | "in"                                        { IN }
  | "do"                                        { DO }
  | "done"                                      { DONE }
  | "while"                                     { WHILE }
  | "begin"                                     { BEGIN }
  | "end"                                       { END }
  | "call"                                      { CALL }
  | "process"                                   { PROCESS }
  | "pipe"                                      { PIPE }
  | "into"                                      { INTO }
  | "epip"                                      { EPIP }
  | "embed"                                     { EMBED }
  | (alpha (alpha | digit | '_')* as v)         { VAR_NAME (v) }
  | ":="                                        { ASSTRING }
  | '('                                         { LPAREN }
  | ')'                                         { RPAREN }
  | '{'                                         { LACCOL }
  | '}'                                         { RACCOL }
  | '['                                         { LCROCH }
  | ']'                                         { RCROCH }
  | ';'                                         { PTVIRG }
  | _ as c                                      { raise (LexerError ("unknown character '" ^ String.make 1 c ^ "'")) }

and string b = parse
  | '\''                                        { LITERAL (Buffer.contents b) }
  | [^'\\''\''] as c                            { Buffer.add_char b c ; string b lexbuf }
  | '\\' (_ as c)                               { Buffer.add_char b c ; string b lexbuf }

and comment n = parse
  | "(*"                                        { comment (n+1) lexbuf }
  | "*)"                                        { if n=1 then token lexbuf else comment (n-1) lexbuf }
  | eof                                         { raise (LexerError ("comment not terminated")) }
  | _                                           { comment n lexbuf }
