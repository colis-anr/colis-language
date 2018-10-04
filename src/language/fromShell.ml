
(* Open Morsmall AST. Import the 'on_located' function. *)

open Morsmall.AST
let on_located = Morsmall.Location.on_located

(* Put CoLiS' syntax inside a 'C' module so that it doesn't clash with
   Morsmall. Add a constructor for concatenation of lists. *)

module C = struct
  include Syntax__Syntax

  let econcat_l = function
    | [] -> failwith "econcat_l"
    | h :: t -> List.fold_left (fun se se' -> EConcat (se, se')) h t
end

(* Define constants coming from Shell or hypothesis we make on input
   Shell scripts. *)

let special_builtins = [
    (* Important note: cd is not in that list because it is
       technically not a special built-in! *)
    "break"; ":"; "continue"; "."; "eval"; "exec";
    "exit"; "export"; "readonly"; "return"; "set";
    "shift"; "times"; "trap"; "unset" ]

let ifs = [' '; '\n'; '\t']

(* Add an exception raised when there is a feature in the input Shell
   script that is not supported. *)

exception Unsupported of string (*FIXME: position*)


(* Define split requirements. This will be usefull in the conversion
   of expressions. They are in fact in a lattice:

     DoesntCare
      /      \
   Split    NoSplit
      \      /
     Impossible

   unify_split_requirements looks for the least upper bound in the
   lattice. *)

type split_requirement =
  Split | NoSplit | DoesntCare | Impossible

let unify_split_requirements a b =
  match a, b with
  | DoesntCare, _ -> b
  | _, DoesntCare -> a
  | _ when a = b -> a
  | _ -> Impossible

let unify_split_requirement_list =
  List.fold_left unify_split_requirements DoesntCare

(* ========================================================================== *)
(* All the following functions here are named X__to__Y where X is a
   type in Morsmall.AST and Y is a type in Syntax__Syntax. The
   functions X'__to__Y take a located type in Morsmall.C. *)

let rec word__to__name = function
  | [Literal s] | [Name s] -> s
  | _ -> raise (Unsupported "(word_to_name)")

and word'__to__name word' =
  on_located word__to__name word'

(* ============================ [ Expressions ] ============================= *)
(* We flag each string_expression with its requirements in term of
   splitting (either Split, NoSplit or DoesntCare). When
   concatenating, we ensure that the requirements are compatible so
   that each string_expression has a unique requirement. This is what
   then gives the 'split' flag in CoLiS. *)

and word_component__to__string_expression_split_requirement = function
  | Literal s when List.exists (String.contains s) ifs ->
     (C.ELiteral s, NoSplit)
  | Literal s | Name s ->
     (C.ELiteral s, DoesntCare)
  | Variable (name, NoAttribute) ->
     (C.EVariable name, Split)
  | Subshell program ->
     (C.ESubshell (program__to__statement program), Split)
  | DoubleQuoted word ->
     word_DoubleQuoted__to__string_expression_split_requirement word
  | _ ->
     raise (Unsupported "(word_component)")

and word_component_DoubleQuoted__to__string_expression = function
  | Literal s | Name s -> C.ELiteral s
  | Variable (name, NoAttribute) -> C.EVariable name
  | Subshell program -> C.ESubshell (program__to__statement program)
  | _ -> raise (Unsupported "(word_component_DoubleQuoted)")

and word__to__string_expression_split_requirement word : (C.string_expression * split_requirement) =
  (* Note: the type annotation here is required because otherwise,
     OCaml gets lost in type unification. *)
  let string_expression_list, split_requirement_list =
    List.map word_component__to__string_expression_split_requirement word
    |> List.split
  in
  (C.econcat_l string_expression_list,
   unify_split_requirement_list split_requirement_list)

and word_DoubleQuoted__to__string_expression_split_requirement word =
  List.map word_component_DoubleQuoted__to__string_expression word
  |> C.econcat_l
  |> fun string_expression -> (string_expression, NoSplit)

(* Now, the real functions. *)

and word__to__string_expression word =
  (* In that case, we don't care about the splitting. *)
  fst (word__to__string_expression_split_requirement word)

and word_list__to__list_expression word_list =
  List.map word__to__string_expression_split_requirement word_list
  |> List.map
       (fun (string_expression, split_requirement) ->
         (string_expression,
          match split_requirement with
          | Impossible -> raise (Unsupported "mixed words")
          | DoesntCare | NoSplit -> C.Split false
          | Split -> C.Split true))

and word'_list__to__list_expression word'_list =
  List.map (fun word' -> word'.Morsmall.Location.value) word'_list
  |> word_list__to__list_expression

and assignment__to__assign (name, word) =
  C.SAssignment (name, word__to__string_expression word)

and assignment'__to__assign assignment' =
  on_located assignment__to__assign assignment'

(* ============================= [ Statements ] ============================= *)

and command__to__statement = function

  | Simple ([], []) ->
     assert false

  | Simple (assignment' :: assignment'_list, []) ->
     List.fold_left
       (fun statement assignment' ->
         let assign = assignment'__to__assign assignment' in
         C.SSequence (statement, assign))
       (assignment'__to__assign assignment')
       assignment'_list

  | Simple (assignment'_list, word' :: word'_list) ->
     let name = word'__to__name word' in
     let args = word'_list__to__list_expression word'_list in
     if assignment'_list <> [] then
       raise (Unsupported "assignment prefixing simple command");
     (
       match name, args with

       (* Special builtins *)

       | ".", [C.ELiteral s, _] when s.[0] = '/' ->
          raise (Unsupported "absolute source")

       | "exit", [] | "exit", [C.EVariable "?", _] ->
          C.(SExit CPrevious)
       | "exit", [C.ELiteral n, _] when int_of_string_opt n = Some 0 ->
          C.(SExit CSuccess)
       | "exit", [C.ELiteral n, _] when int_of_string_opt n <> None ->
          C.(SExit CFailure)

       | "set", [C.ELiteral "-e", _] ->
          (* FIXME *)
          C.SCall ("true", [])

       (* All the other special builtins: unsupported *)

       | _ when List.mem name special_builtins ->
          raise (Unsupported ("special builtin: " ^ name))

       (* cd: not a special builtin, but still deserves a special
          treatement *)

       | "cd", _ ->
          raise (Unsupported "cd")

       (* FIXME: functions *)

       (* all the other commands *)

       | _ ->
          let command =
            List.fold_right
              (fun assignment' statement ->
                let assign = assignment'__to__assign assignment' in
                (* FIXME: export *)
                C.SSequence (assign, statement))
              assignment'_list
              (C.SCall (name, args))
          in
          C.SSubshell command
     )

  | Async _ ->
     raise (Unsupported "asynchronous separator &")

  | Seq (first', second') ->
     let first = command'__to__statement first' in
     let second = command'__to__statement second' in
     C.SSequence (first, second)

  | And (first', second') ->
     let first = command'__to__statement first' in
     let second = command'__to__statement second' in
     C.SIf (first, second, C.SNot (C.SCall ("false", [])))

  | Or (first', second') ->
     let first = command'__to__statement first' in
     let second = command'__to__statement second' in
     C.SIf (first, C.SCall ("true", []), second)

  | Not command' ->
     let statement = command'__to__statement command' in
     C.SNot statement

  | Pipe (first', second') ->
     let first = command'__to__statement first' in
     let second = command'__to__statement second' in
     C.SPipe (first, second)

  | Subshell command' ->
     let statement = command'__to__statement command' in
     C.SSubshell statement

  | For (_, None, _) ->
     raise (Unsupported "for with no list")

  | For (name, Some word_list, command') ->
     let statement = command'__to__statement command' in
     C.SForeach (name, word_list__to__list_expression word_list, statement)

  | Case _ ->
     raise (Unsupported "case")

  | If (test', body', None) ->
     let test = command'__to__statement test' in
     let body = command'__to__statement body' in
     C.SIf (test, body, C.SCall ("true", []))

  | If (test', body', Some rest') ->
     let test = command'__to__statement test' in
     let body = command'__to__statement body' in
     let rest = command'__to__statement rest' in
     C.SIf (test, body, rest)

  | While (cond', body') ->
     C.SWhile (command'__to__statement cond',
               command'__to__statement body')

  | Until (_cond, _body) ->
     raise (Unsupported "until")

  | Function _ ->
     raise (Unsupported ("function"))

  | Redirection _ as command ->
     redirection__to__statement command

  | HereDocument _ ->
     raise (Unsupported ("here document"))

and command'__to__statement command' =
  on_located command__to__statement command'

and redirection__to__statement = function
  (* >=2 redirected to /dev/null. Since they don't have any impact on
     the semantics of the program, we don't care. *)
  | Redirection (command', descr, Output, [Literal "/dev/null"])
       when descr >= 2 ->
     command'__to__statement command'

  (* 1 redirected to >=2, this means the output will never ever have
     an impact on the semantics again ==> ignore *)
  | Redirection (command', 1, OutputDuplicate, [Literal i])
       when (try int_of_string i >= 2 with Failure _ ->  false) ->
     C.SNoOutput (command'__to__statement command')

  (* 1 redirected to /dev/null. This means that the output will never
     have an impact on the semantics again ==> Ignore. In fact, we can
     even be a bit better an accept all subsequent redirections of >=2
     to 1. *)
  | Redirection (command', 1, Output, [Literal "/dev/null"]) ->
     (
       let rec flush_redirections_to_1 = function
         | Redirection (command', descr, OutputDuplicate, [Literal "1"])
              when descr >= 2 ->
            flush_redirections'_to_1 command'
         | _ as command -> command
       and flush_redirections'_to_1 redirection' =
         on_located flush_redirections_to_1 redirection'
       in
       C.SNoOutput (command__to__statement (flush_redirections'_to_1 command'))
     )

  | _ -> raise (Unsupported ("other redirections"))

and program__to__statement = function
  | [] -> raise (Unsupported "empty program")
  | first' :: rest' ->
     List.fold_left
       (fun statement command' ->
         let statement' = command'__to__statement command' in
         C.SSequence (statement, statement'))
       (command'__to__statement first')
       rest'

(* ============================ [ Entry point ] ============================= *)

let parse filename : C.statement =
  try
    Morsmall.parse_file filename
    |> program__to__statement
  with
  | Morsmall.SyntaxError _pos ->
     Format.printf "Syntax error";
     exit 2
  | Unsupported feat ->
     Format.printf "Unsupported feature: %s" feat;
     exit 3
