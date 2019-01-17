/* File colis_parser.mly */

%{

open Syntax__Syntax

let rec concat = function
  | [] -> assert false (* parsed as a non-empty list *)
  | [se] -> se
  | se :: ses -> SConcat (se, concat ses)

%}


%token SPLIT SUCCESS FAILURE PREVIOUS EXIT NOT IF THEN ELSE FI FOR IN FUNCTION CALL
%token DO DONE WHILE BEGIN END PROCESS PIPE INTO EPIP NOOUTPUT ASSTRING ARG SHIFT
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET EMBED SEMICOLON EOF RETURN
%token<string> LITERAL
%token<string> IDENTIFIER
%token<Z.t> NAT
%start program
%type <Syntax__Syntax.program> program
%%
program:
  list(function_definition)
  BEGIN seq END EOF                                           { {function_definitions=$1; instruction=$3} }
;
function_definition:
  FUNCTION IDENTIFIER BEGIN seq END                           { $2, $4 }
;
instruction:
  | EXIT exit_code                                            { IExit($2) }
  | RETURN exit_code                                          { IReturn($2) }
  | SHIFT option(NAT)                                         { IShift($2) }
  | IF instruction THEN instruction ELSE instruction FI       { IIf ($2, $4, $6) }
  | IF instruction THEN instruction FI                        { IIf ($2, $4, ICallUtility("true", [])) }
  | NOT instruction                                           { INot ($2) }
  | FOR IDENTIFIER IN lexpr DO instruction DONE               { IForeach ($2, $4, $6) }
  | WHILE instruction DO instruction DONE                     { IWhile ($2, $4) }
  | BEGIN seq END                                             { $2 }
  | PROCESS instruction                                       { ISubshell ($2) }
  | PIPE pipe EPIP                                            { $2 }
  | NOOUTPUT instruction                                      { INoOutput ($2) }
  | IDENTIFIER                                                { ICallUtility ($1, []) }
  | IDENTIFIER lexpr                                          { ICallUtility ($1, $2) }
  | CALL IDENTIFIER                                           { ICallFunction ($2, []) }
  | CALL IDENTIFIER lexpr                                     { ICallFunction ($2, $3) }
  | IDENTIFIER ASSTRING sexpr                                 { IAssignment ($1, $3) }
  | LPAREN instruction RPAREN                                 { $2 }
;
exit_code:
  | SUCCESS                                                   { RSuccess }
  | FAILURE                                                   { RFailure }
  | PREVIOUS                                                  { RPrevious }
;
pipe:
  | instruction INTO pipe                                     { IPipe($1,$3) }
  | instruction                                               { $1 }
;
seq:
  | instruction SEMICOLON seq                                 { ISequence($1,$3) }
  | instruction                                               { $1 }
;
sfrag:
  | LITERAL                                                   { SLiteral($1) }
  | IDENTIFIER                                                { SVariable($1) }
  | EMBED delimited(LBRACE, instruction, RBRACE)              { SSubshell($2) }
  | ARG NAT                                                   { SArgument($2) }
;
sexpr:
  | nonempty_list(sfrag)                                      { concat $1 }
;
lfrag:
  | SPLIT sexpr                                               { $2, Split }
  | sexpr                                                     { $1, DontSplit}
;
lexpr:
  | delimited (LBRACKET, separated_list(SEMICOLON, lfrag), RBRACKET) { $1 }
;
