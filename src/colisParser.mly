/* File colis_parser.mly */

%{

open Syntax__Syntax

let rec concat = function
  | [] -> assert false (* parsed as a non-empty list *)
  | [se] -> se
  | se :: ses -> SConcat (se, concat ses)

%}


%token SPLIT SUCCESS FAILURE PREVIOUS EXIT NOT IF THEN ELSE FI FOR IN
%token DO DONE WHILE BEGIN END PROCESS PIPE INTO EPIP NOOUTPUT ASSTRING
%token LPAREN RPAREN LACCOL RACCOL LCROCH RCROCH EMBED PTVIRG EOF
%token<string> LITERAL
%token<string> IDENTIFIER
%start program
%type <Syntax__Syntax.instruction> program
%%
program:
  list(function_definition)
  instruction EOF                                             { {function_definitions=$1; instruction=$2} }
;
function_definition:
  FUNCTION IDENTIFIER instruction END                         { $2, $3 }
;
instruction:
  | EXIT exit_code                                            { IExit($2) }
  | IF instruction THEN instruction ELSE instruction FI       { IIf ($2, $4, $6) }
  | IF instruction THEN instruction FI                        { IIf ($2, $4, ICallBuiltin("true", [])) }
  | NOT instruction                                           { INot ($2) }
  | FOR IDENTIFIER IN lexpr DO instruction DONE               { IForeach ($2, $4, $6) }
  | WHILE instruction DO instruction DONE                     { IWhile ($2, $4) }
  | BEGIN seq END                                             { $2 }
  | PROCESS instruction                                       { ISubshell ($2) }
  | PIPE pipe EPIP                                            { $2 }
  | NOOUTPUT instruction                                      { INoOutput ($2) }
  | IDENTIFIER                                                { ICallBuiltin ($1, []) }
  | IDENTIFIER lexpr                                          { ICallBuiltin ($1, $2) }
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
  | instruction PTVIRG seq                                    { ISequence($1,$3) }
  | instruction                                               { $1 }
;
sfrag:
  | LITERAL                                                   { SLiteral($1) }
  | IDENTIFIER                                                { SVariable($1) }
  | EMBED delimited(LACCOL, instruction, RACCOL)              { SSubshell($2) }
;
sexpr:
  | nonempty_list(sfrag)                                      { concat $1 }
;
lfrag:
  | SPLIT sexpr                                               { $2, Split }
  | sexpr                                                     { $1, DontSplit}
;
lexpr:
  | delimited (LCROCH, separated_list(PTVIRG, lfrag), RCROCH) { $1 }
;
