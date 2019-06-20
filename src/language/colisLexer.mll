(* File colis_lexer.mll *)
{
open ColisParser        (* The type token is defined in colis_parser.mli *)
exception LexerError of string

let reserved_words =
  [ "arg",         ARG ;
    "begin",       BEGIN ;
    "call",        CALL ;
    "do",          DO ;
    "done",        DONE ;
    "else",        ELSE ;
    "embed",       EMBED ;
    "end",         END ;
    "endnooutput", ENDNOOUTPUT ;
    "endpipe",     ENDPIPE ;
    "endprocess",  ENDPROCESS ;
    "exit",        EXIT ;
    "export",      EXPORT ;
    "failure",     FAILURE ;
    "function",    FUNCTION ;
    "fi",          FI ;
    "for",         FOR ;
    "if",          IF ;
    "in",          IN ;
    "into",        INTO ;
    "not",         NOT ;
    "nooutput",    NOOUTPUT ;
    "pipe",        PIPE ;
    "previous",    PREVIOUS ;
    "process",     PROCESS ;
    "return",      RETURN ;
    "shift",       SHIFT ;
    "split",       SPLIT ;
    "success",     SUCCESS ;
    "then",        THEN ;
    "while",       WHILE ]

let promote_reserved_words word =
  try List.assoc word reserved_words
  with Not_found -> IDENTIFIER word
}

let lalpha = ['a'-'z' '_']
let ualpha = ['A'-'Z']
let alpha = lalpha | ualpha
let digit = ['0'-'9']

rule token = parse
  | eof                                 { EOF }
  | "(*"                                { comment 1 lexbuf }
  | "*)"                                { raise (LexerError ("mismatched *)")) }
  | ":="                                { ASSTRING }
  | "&&"                                { AND }
  | "||"                                { OR }
  | '{'                                 { LBRACE }
  | '}'                                 { RBRACE }
  | '('                                 { LPAREN }
  | ')'                                 { RPAREN }
  | ';'                                 { SEMICOLON }
  | '['                                 { LBRACKET }
  | ']'                                 { RBRACKET }
  | '\''                                { let b = Buffer.create 10 in string b lexbuf }
  | '\n'                                { Lexing.new_line lexbuf; token lexbuf }
  | (alpha (alpha | digit | '_')* as s) { promote_reserved_words s }
  | (digit+ as s)                       { NAT (Z.of_string s) }
  | ['\t' ' ']                          { token lexbuf }     (* skip tab and blank*)
  | _ as c                              { raise (LexerError ("unknown character '" ^ String.make 1 c ^ "'")) }

and string b = parse
  | eof                                 { raise (LexerError "Unterminated string") }
  | '\''                                { LITERAL (Buffer.contents b) }
  | [^'\\''\''] as c                    { Buffer.add_char b c ; string b lexbuf }
  | '\\' (_ as c)                       { Buffer.add_char b c ; string b lexbuf }

and comment n = parse
  | eof                                 { raise (LexerError "Unterminated comment") }
  | "(*"                                { comment (n+1) lexbuf }
  | "*)"                                { if n=1 then token lexbuf else comment (n-1) lexbuf }
  | _                                   { comment n lexbuf }
