open Format

let rec print_list_pre sep print fmt = function
  | [] -> ()
  | x :: r -> sep fmt (); print fmt x; print_list_pre sep print fmt r

let print_list sep print fmt = function
  | [] -> ()
  | [x] -> print fmt x
  | x :: r -> print fmt x; print_list_pre sep print fmt r

(* let comma fmt () = fprintf fmt ",@ " *)

let semi fmt () = fprintf fmt ";@ "

open Syntax__Syntax

let rec string_expression (fmt:formatter) (e:string_expression) : unit =
  match e with
  | SLiteral s ->
     fprintf fmt "'%s'" s
  | SVariable s ->
     fprintf fmt "%s" s
  | SSubshell i ->
     fprintf fmt "@[embed {@ %a }@]" instruction i
  | SArgument n ->
     fprintf fmt "arg %a" Z.pp_print n
  | SConcat(e1,e2) ->
     fprintf fmt "@[%a@ %a@]" string_expression e1 string_expression e2

and expr_split (fmt:formatter) (e,s) : unit =
  match s with
  | Split -> fprintf fmt "split %a" string_expression e
  | DontSplit -> fprintf fmt "%a" string_expression e

and lexpr (fmt:formatter) (l: (string_expression * split) list) =
  fprintf fmt "@[[ %a ]@]" (print_list semi expr_split) l

and instruction (fmt:formatter) (i:instruction) : unit =
  match i with
  | IAssignment(s,e) ->
     fprintf fmt "@[<hv 2>%s :=@ %a@]" s string_expression e
  | ISequence(i1,i2) ->
     fprintf fmt "@[<v 2>begin@ %a ;@ %a@]@ end" sequence i1 sequence i2
  | ISubshell i ->
     fprintf fmt "@[process@ %a@]" instruction i
  | IIf(c,i1,ICallUtility("true", [])) ->
     fprintf fmt "@[@[<hv 2>if %a@]@ @[<hv 2>then %a@]@ fi@]"
             instruction c instruction i1
  | IIf(c,i1,i2) ->
     fprintf fmt "@[@[<hv 2>if %a@]@ @[<hv 2>then %a@]@ @[<hv 2>else %a@]@ fi@]"
             instruction c instruction i1 instruction i2
  | INot i1 ->
     fprintf fmt "@[<hv 2>not %a@]" instruction i1
  | IPipe(i1,i2) ->
     fprintf fmt "@[@[<v 0>pipe@ %a@ into %a@]@ epip@]" instruction i1 pipe i2
  | IWhile(i1,i2) ->
     fprintf fmt "@[@[<hv 2>while %a@ do %a@]@ done@]"
             instruction i1 instruction i2
  | INoOutput i1 ->
     fprintf fmt "@[@[<hv 2>nooutput %a@]@]" instruction i1
  | IForeach(id,le,i1) ->
     fprintf fmt "@[@[<hv 2>for %s@ in %a@ do %a@]@ done@]"
             id lexpr le instruction i1
  | ICallUtility(s,[]) ->
     fprintf fmt "@[<h>%s@]" s
  | ICallUtility(s,args) ->
     fprintf fmt "@[<h>%s@ %a@]" s lexpr args
  | ICallFunction(s,[]) ->
     fprintf fmt "@[<h>call %s@]" s
  | ICallFunction(s,args) ->
     fprintf fmt "@[<h>call %s %a@]" s lexpr args
  | IExit c ->
     fprintf fmt "@[<h>exit %a@]" exitcode c
  | IReturn c ->
     fprintf fmt "@[return@ %a@]" exitcode c
  | IShift bn ->
     let n = match bn with None -> "" | Some n -> " "^Z.to_string n in
     fprintf fmt "@[shift%s@]" n
  | IExport id ->
     fprintf fmt "@[export %s@]" id

and exitcode (fmt:formatter) (c:return_code) =
  match c with
  | RSuccess -> fprintf fmt "success"
  | RFailure -> fprintf fmt "failure"
  | RPrevious -> fprintf fmt "previous"

and sequence (fmt:formatter) (i:instruction) : unit =
  match i with
  | ISequence(i1,i2) ->
     fprintf fmt "@[<v 0>%a ;@ %a@]" sequence i1 sequence i2
  | _ -> instruction fmt i

and pipe (fmt:formatter) (i:instruction) : unit =
  match i with
  | IPipe(i1,i2) ->
     fprintf fmt "@[<v 0>%a into@ %a@]" instruction i1 pipe i2
  | _ -> instruction fmt i

and function_definition fmt (n, i) =
  fprintf fmt "@[function %s %a@]@\n" n instruction i

and program fmt p =
  List.iter (function_definition fmt) p.function_definitions;
  fprintf fmt "@[@[<v 2>begin@ %a@]@ end@]" sequence p.instruction
