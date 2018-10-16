open Format

let rec print_list_pre sep print fmt = function
  | [] -> ()
  | x :: r -> sep fmt (); print fmt x; print_list_pre sep print fmt r

let print_list sep print fmt = function
  | [] -> ()
  | [x] -> print fmt x
  | x :: r -> print fmt x; print_list_pre sep print fmt r

let comma fmt () = fprintf fmt ",@ "

open Syntax__Syntax

let rec string_expression (fmt:formatter) (e:string_expression) : unit =
  match e with
  | SLiteral s ->
     fprintf fmt "'%s'" s
  | SVariable s ->
     fprintf fmt "%s" s
  | SSubshell i ->
     fprintf fmt "@[embed {@ %a }@]" instruction i
  | SConcat(e1,e2) ->
     fprintf fmt "@[%a@ %a@]" string_expression e1 string_expression e2

and expr_split (fmt:formatter) (e,s) : unit =
  match s with
  | Split -> fprintf fmt "split %a" string_expression e
  | DontSplit -> fprintf fmt "%a" string_expression e

and instruction (fmt:formatter) (i:instruction) : unit =
  match i with
  | IAssignment(s,e) ->
     fprintf fmt "@[<hv 2>%s :=@ %a@]" s string_expression e
  | ISequence(i1,i2) ->
     fprintf fmt "@[<v 0>%a ;@ %a@]" instruction i1 instruction i2
  | ISubshell i ->
     fprintf fmt "@[process {@ %a }@]" instruction i
  | IIf(c,i1,i2) ->
     fprintf fmt "@[<hv 2>if %a@ then %a@ else %a fi@]"
             instruction c instruction i1 instruction i2
  | INot _ ->
     failwith "Not implemented: ToColis.statement INot"
  | IPipe _ ->
     failwith "Not implemented: ToColis.statement IPipe"
  | IWhile _ ->
     failwith "Not implemented: ToColis.statement IWhile"
  | INoOutput _ ->
     failwith "Not implemented: ToColis.statement INoOutput"
  | IForeach _ ->
     failwith "Not implemented: ToColis.statement IForeach"
  | ICall(s,args) ->
     fprintf fmt "@[%s@ [@ %a ]@]" s (print_list comma expr_split) args

  | IExit _ ->
     failwith "Not implemented: ToColis.statement IExit"

let program fmt p = instruction fmt p
