/* File colis_parser.mly */

%{

open Syntax__Syntax

let rec concat = function
  | [] -> assert false (* parsed as a non-empty list *)
  | [se] -> se
  | se :: ses -> EConcat (se, concat ses)

%}


%token SUCCESS FAILURE PREVIOUS EXIT NOT IF THEN ELSE FI FOR IN
%token DO DONE WHILE BEGIN END PROCESS PIPE INTO EPIP ASSTRING
%token LPAREN RPAREN LACCOL RACCOL LCROCH RCROCH EMBED PTVIRG NOP
%token<string> LITERAL
%token<string> VAR_NAME
%start statement
%type <Syntax__Syntax.statement> statement
%%
statement:
  | NOP                                              { SNop }
  | EXIT exit_code                                   { SExit($2) }
  | IF statement THEN statement ELSE statement FI    { SIf ($2, $4, $6) }
  | IF statement THEN statement FI                   { SIf ($2, $4, SNop) }
  | NOT statement                                    { SNot ($2) }
  | FOR VAR_NAME IN lexpr DO statement DONE          { SForeach ($2, $4, $6) }
  | WHILE statement DO statement DONE                { SWhile ($2, $4) }
  | BEGIN seq END                                    { $2 }
  | PROCESS statement                                { SSubshell ($2) }
  | PIPE pipe EPIP                                   { $2 }
  | VAR_NAME lexpr                                   { SCall ($1, $2) }
  | VAR_NAME ASSTRING sexpr                          { SAssignment ($1, $3) }
  | LPAREN statement RPAREN                          { $2 }
;
exit_code:
  | SUCCESS                                          { CSuccess }
  | FAILURE                                          { CFailure }
  | PREVIOUS                                         { CPrevious }
;
pipe:
  | statement INTO pipe                              { SPipe($1,$3) }
  | statement                                        { $1 }
;
seq:
  | statement PTVIRG seq                             { SSequence($1,$3) }
  | statement                                        { $1 }
;
sfrag:
  | LITERAL                                          { ELiteral($1) }
  | VAR_NAME                                         { EVariable($1) }
  | EMBED statement                                  { ESubshell($2) }
;
sexpr:
  | delimited (LPAREN, nonempty_list(sfrag), RPAREN) { concat $1 }
;
lfrag:
  | sexpr                                            { $1, Split false }
  | delimited (LACCOL, sexpr, RACCOL)                { $1, Split true}
;
lexpr:
  | delimited (LCROCH, list(lfrag), RCROCH)          { $1 }
;
