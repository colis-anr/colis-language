open Colis_internals

(* Open Morsmall AST. Import the 'on_located' function. *)

open Morsmall.AST

let on_located f v = f ~pos:v.Morsmall.Location.position v.Morsmall.Location.value

let error ~pos msg =
  raise (Colis_internals.Errors.ConversionError (pos, msg))

let unsupported ~pos feature =
  error ~pos ("unsupported feature: " ^ feature)

(* Put CoLiS' syntax inside a 'C' module so that it doesn't clash with
   Morsmall. Add a constructor for concatenation of lists. *)

module C = struct
  include Syntax__Syntax

  let sconcat_l = function
    | [] -> SLiteral ""
    | s :: ss -> List.fold_left (fun s1 s2 -> SConcat (s1, s2)) s ss

  let isequence_l = function
    | [] -> failwith "isequence_l"
    | i :: is -> List.fold_left (fun i1 i2 -> ISequence (i1, i2)) i is

  let icolon = ICallUtility (":", [])
  let itrue = ICallUtility ("true", [])
  let ifalse = ICallUtility ("false", [])

  let ior (i1, i2) = IIf (i1, icolon, i2)
  let iand  (i1, i2) = IIf (i1, i2, INot icolon)

  let rec sexpr_unfold_concat = function
    | SConcat (s1, s2) ->
      sexpr_unfold_concat s1 @ sexpr_unfold_concat s2
    | sexpr -> [sexpr]

  let rec sexprs_merge_literals = function
    | SLiteral a :: SLiteral b :: sexprs ->
      sexprs_merge_literals (SLiteral (a^b) :: sexprs)
    | [] ->
      []
    | sexpr :: sexprs ->
      sexpr :: sexprs_merge_literals sexprs
end

let list_fold_map (f : 'a -> 'b -> ('a * 'c)) (x : 'a) (l : 'b list) : ('a * 'c list) =
  List.fold_left
    (fun (x, l') e ->
      let (x', e') = f x e in
      (x', e' :: l'))
    (x, []) l
  |> fun (x, l) -> (x, List.rev l)

(* Define the conversion environment. *)

module E = struct
  module SSet = Set.Make(String)
  module SMap = struct
    include Map.Make(String)

    let to_list m =
      fold (fun k v l -> (k, v) :: l) m []
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

  type t =
    { at_toplevel : bool ;
      inside_function_body : bool ;
      cmd_line_arguments : string list ;
      names_called : SSet.t ;
      functions : C.instruction SMap.t }

  let empty cmd_line_arguments =
    { at_toplevel = true ;
      inside_function_body = false ;
      cmd_line_arguments ;
      names_called = SSet.empty ;
      functions = SMap.empty }

  let check_legal_function_name ~pos e n =
    if List.mem n special_builtins then
      unsupported ~pos "function definition shadowing a special builtin";
    if SMap.mem n e.functions then
      unsupported ~pos "function definition shadowing an other function";
    if SSet.mem n e.names_called then
      unsupported ~pos "function definition after a use of the same name"

  let add_called e n =
    { e with names_called = SSet.add n e.names_called }

  let add_function e n i =
    { e with functions = SMap.add n i e.functions }

  let replace_function e n i =
    if not (SMap.mem n e.functions) then
      failwith "E.replace_function";
    { e with functions = SMap.add n i e.functions }

  let is_function e n =
    SMap.mem n e.functions

  let get_functions e =
    e.functions |> SMap.to_list

  let with_deeper e f =
    let (e', x) = f { e with at_toplevel = false } in
    ({ e' with at_toplevel = e.at_toplevel }, x)

  let is_inside_function_body e =
    e.inside_function_body

  let with_inside_function_body e f =
    let (e', x) = f { e with inside_function_body = true } in
    ({ e' with inside_function_body = e.inside_function_body }, x)
end

let on_located_with_env (f : pos:Morsmall.Location.position -> E.t -> 'a -> (E.t * 'b)) (e : E.t) : 'a Morsmall.Location.located -> (E.t * 'b) =
  on_located (f e)

(* Define split requirements. This will be useful in the conversion
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

let rec word__to__name ~pos e = function
  | [WLiteral s] -> (e, s)
  | _ -> unsupported ~pos "name that was not a literal"

and word'__to__name e word' =
  on_located_with_env word__to__name e word'

(* ============================ [ Expressions ] ============================= *)
(* We flag each string_expression with its requirements in term of
   splitting (either Split, NoSplit or DoesntCare). When
   concatenating, we ensure that the requirements are compatible so
   that each string_expression has a unique requirement. This is what
   then gives the 'split' flag in CoLiS. *)

and word_component__to__string_expression_split_requirement ~pos e = function
  | WLiteral s when List.exists (String.contains s) E.ifs ->
     (e, (C.SLiteral s, NoSplit))
  | WLiteral s ->
    (e, (C.SLiteral s, DoesntCare))
  | WVariable (name, NoAttribute) when name = "*" || name = "#" ->
    unsupported ~pos "$* or $#"
  | WVariable (name, NoAttribute) when name = "@" ->
    unsupported ~pos "$@ without quotes"
  | WVariable (name, NoAttribute) when int_of_string_opt name <> None ->
    (e, (C.SArgument (Z.of_int (int_of_string name)), Split))
  | WVariable (name, NoAttribute) ->
    (e, (C.SVariable name, Split))
  | WVariable _ ->
    unsupported ~pos "variable with attribute"
  | WSubshell c's ->
     E.with_deeper e @@ fun e ->
     let (e, i) = command'_list__to__instruction e c's in
     (e, (C.SSubshell i, Split))
  | WDoubleQuoted word ->
     E.with_deeper e @@ fun e ->
     word_DoubleQuoted__to__string_expression_split_requirement ~pos e word
  | WTildePrefix _ -> unsupported ~pos "tilde prefix"
  | WGlobAll -> unsupported ~pos "glob '*'"
  | WGlobAny -> unsupported ~pos "glob '?'"
  | WBracketExpression _ -> unsupported ~pos "bracket expression"

and word_component_DoubleQuoted__to__string_expression ~pos e = function
  | WLiteral s ->
    (e, C.SLiteral s)
  | WVariable (name, NoAttribute) when name = "*" || name = "#" ->
    unsupported ~pos "$* or $#"
  | WVariable (name, NoAttribute) when name = "@" ->
    unsupported ~pos "$@ with quotes (yet)" (* FIXME *)
  | WVariable (name, NoAttribute) when int_of_string_opt name <> None ->
     (e, C.SArgument (Z.of_int (int_of_string name)))
  | WVariable (name, NoAttribute) ->
    (e, C.SVariable name)
  | WVariable _ ->
    unsupported ~pos "variable with attribute"
  | WSubshell c's ->
     E.with_deeper e @@ fun e ->
     let (e, i) = command'_list__to__instruction e c's in
     (e, C.SSubshell i)
  | WTildePrefix _ -> assert false
  | WDoubleQuoted _ -> assert false
  | WGlobAll -> unsupported ~pos "glob '*'"
  | WGlobAny -> unsupported ~pos "glob '?'"
  | WBracketExpression _ -> unsupported ~pos "bracket expression"

and word__to__string_expression_split_requirement ~pos e w : (E.t * (C.string_expression * split_requirement)) =
  (* Note: the type annotation here is required because otherwise,
     OCaml gets lost in type unification for some reason. *)
  let (e, expr_and_req) =
    list_fold_map (word_component__to__string_expression_split_requirement ~pos) e w
  in
  let string_expression_list, split_requirement_list =
    List.split expr_and_req
  in
  (e, (C.sconcat_l string_expression_list, unify_split_requirement_list split_requirement_list))

and word_DoubleQuoted__to__string_expression_split_requirement ~pos e word =
  let (e, exprs) =
    list_fold_map (word_component_DoubleQuoted__to__string_expression ~pos) e word
  in
  (e, (C.sconcat_l exprs, NoSplit))

(* Now, the real functions. *)

and word__to__string_expression ~pos e w =
  (* In that case, we don't care about the splitting. *)
  let (e, (expr, _)) = (word__to__string_expression_split_requirement ~pos) e w in
  (e, expr)

and word'__to__string_expression e w' =
  on_located_with_env word__to__string_expression e w'

and word'_list__to__list_expression e word'_list =
  list_fold_map
    (fun e w' ->
       let pos = w'.Morsmall.Location.position in
       let w = w'.Morsmall.Location.value in
       match w with
       | [WDoubleQuoted [WVariable ("@", NoAttribute)]] -> (* "$@" *)
         if E.is_inside_function_body e then
           unsupported ~pos "\"$@\" inside function body"
         else
           (e, List.map (fun arg -> (C.SLiteral arg, C.DontSplit)) e.E.cmd_line_arguments)
       | _ ->
         let (e, (se, sr)) = word__to__string_expression_split_requirement ~pos e w in
         (e, [(se,
              match sr with
              | Impossible -> unsupported ~pos "mixed words"
              | DoesntCare | NoSplit -> C.DontSplit
              | Split -> C.Split)]))
    e
    word'_list
  |> fun (e, l) -> (e, List.flatten l)

and assignment__to__assign ~pos e (n, w) =
  let (e, s1) = word__to__string_expression ~pos e w in
  (e, C.IAssignment (n, s1))

and assignment'__to__assign e assignment' =
  on_located_with_env assignment__to__assign e assignment'

(* ============================ [ Instructions ] ============================ *)

and command__to__instruction ~pos (e : E.t) : command -> E.t * C.instruction =
  function

  | Simple ([], []) ->
     assert false

  | Simple ((_ :: _) as a's, []) ->
     let (e, as_) = list_fold_map assignment'__to__assign e a's in
     (e, C.isequence_l as_)

  | Simple ([], word' :: word'_list) ->
     let (e, name) = word'__to__name e word' in
     let (e, args) = word'_list__to__list_expression e word'_list in

     (
       match name, args with

       (* Special builtins *)

       | ".", [C.SLiteral s, _] when s.[0] = '/' ->
          (
            match !Options.external_sources with
            | "" -> unsupported ~pos "absolute source without external sources"
            | prefix ->
               try parse_file_in_env e (Filename.concat prefix s)
               with Errors.FileError _ ->
                 error ~pos "absolute source where external source could not be read"
          )

       (* Note: the special case [exec 2>&1] is handled later, with redirections. *)

       | "exit", [] | "exit", [C.SVariable "?", _] ->
          (e, C.(IExit RPrevious))
       | "exit", [C.SLiteral n, _] when int_of_string_opt n = Some 0 ->
          (e, C.(IExit RSuccess))
       | "exit", [C.SLiteral n, _] when int_of_string_opt n <> None ->
          (e, C.(IExit RFailure))

       | "export", [sexpr, DontSplit] ->
         (
           match sexpr |> C.sexpr_unfold_concat |> C.sexprs_merge_literals with
           | [C.SLiteral name] when String.index_opt name '=' = None ->
             (e, C.IExport name)
           | C.SLiteral name :: sexprs when String.index_opt name '=' <> None ->
             let name, literal =
               let i = String.index name '=' in
               String.(sub name 0 i, sub name (i+1) (length name - i-1))
             in
             (e, C.(ISequence (
                  IAssignment (name, C.sconcat_l (SLiteral literal :: sexprs)),
                  IExport name)))
           | _ ->
             unsupported ~pos "export with one bad argument"
         )
       | "export", _ ->
         unsupported ~pos "export with no, several or splitable arguments"

       | "return", [] | "return", [C.SVariable "?", _] ->
          (e, C.(IReturn RPrevious))
       | "return", [C.SLiteral n, _] when int_of_string_opt n = Some 0 ->
          (e, C.(IReturn RSuccess))
       | "return", [C.SLiteral n, _] when int_of_string_opt n <> None ->
          (e, C.(IReturn RFailure))

       | "set", [C.SLiteral "-e", _] ->
          (* FIXME *)
          (e, C.itrue)

       | "shift", [] ->
          (e, C.IShift None)

       (* All the other special builtins: unsupported *)

       | _ when List.mem name E.special_builtins ->
          unsupported ~pos ("special builtin: " ^ name)

       | _ when E.is_function e name ->
          (e, C.ICallFunction (name, args))

       (* cd: not a special builtin (so it is handled after functions,
          but still deserves a special treatement *)

       | "cd", [arg, C.DontSplit] ->
          (e, C.ICd arg)
       | "cd", [C.SLiteral "-", _] ->
          unsupported ~pos "cd -"
       | "cd", _ ->
          unsupported ~pos "cd with multiple or zero arguments"

       (* FIXME: functions *)

       (* all the other commands *)

       | _ ->
          (e, C.ICallUtility (name, args))
     )
     |> fun (e, i) -> (E.add_called e name, i)

  | Simple (_::_, _::_) ->
     unsupported ~pos "prefix assignments"

  | Async _ ->
     unsupported ~pos "asynchronous separator &"

  | Seq (c1', c2') ->
     (* Warning: no E.with_deeper here. *)
     let (e1, i1) = command'__to__instruction e  c1' in
     let (e2, i2) = command'__to__instruction e1 c2' in
     (e2, C.ISequence (i1, i2))

  | And (c1', c2') ->
     E.with_deeper e @@ fun e ->
     let (e1, i1) = command'__to__instruction e  c1' in
     let (e2, i2) = command'__to__instruction e1 c2' in
     (e2, C.iand (i1, i2))

  | Or (c1', c2') ->
     E.with_deeper e @@ fun e ->
     let (e1, i1) = command'__to__instruction e  c1' in
     let (e2, i2) = command'__to__instruction e1 c2' in
     (e2, C.ior (i1, i2))

  | Not c1' ->
     E.with_deeper e @@ fun e ->
     let (e1, i1) = command'__to__instruction e c1' in
     (e1, C.INot i1)

  | Pipe (c1', c2') ->
     E.with_deeper e @@ fun e ->
     let (e1, i1) = command'__to__instruction e  c1' in
     let (e2, i2) = command'__to__instruction e1 c2' in
     (e2, C.IPipe (i1, i2))

  | Subshell c1' ->
     E.with_deeper e @@ fun e ->
     let (e1, i1) = command'__to__instruction e c1' in
     (e1, C.ISubshell i1)

  | For (_, None, _) ->
     unsupported ~pos "for with no list"

  | For (x, Some word_list, c1') ->
     E.with_deeper e @@ fun e ->
     let (e0, expr) = word'_list__to__list_expression e word_list in
     let (e1, i1) = command'__to__instruction e0 c1' in
     (e1, C.IForeach (x, expr, i1))
  (* FIXME: with only functions and topevel, it's alright. If we put
     more, we have to be carefull because c1' also happens after itself. *)

  | Case (word', case_item'_list) ->
     let fresh_var = "case_" ^ (string_of_int (Random.int 10000000)) in
     let (e0, sexpr) = word'__to__string_expression e word' in
     let (e1, instruction) = case_item'_list__to__if_sequence fresh_var e0 case_item'_list in
     (e1, C.(ISequence (IAssignment (fresh_var, sexpr), instruction)))

  | If (c1', c2', None) ->
     E.with_deeper e @@ fun e ->
     let (e1, i1) = command'__to__instruction e  c1' in
     let (e2, i2) = command'__to__instruction e1 c2' in
     (e2, C.IIf (i1, i2, C.icolon))

  | If (c1', c2', Some c3') ->
     E.with_deeper e @@ fun e ->
     let (e1, i1) = command'__to__instruction e  c1' in
     let (e2, i2) = command'__to__instruction e1 c2' in
     let (_ , i3) = command'__to__instruction e1 c3' in (* Warning: it's e1! *)
     (e2, C.IIf (i1, i2, i3))
  (* FIXME: with only functions and topevel, it's alright. If we put
     more, we have to be careful about merging e2 and e3. *)

  | While (c1', c2') ->
     E.with_deeper e @@ fun e ->
     let (e1, i1) = command'__to__instruction e  c1' in
     let (e2, i2) = command'__to__instruction e1 c2' in
     (e2, C.IWhile (i1, i2))
  (* FIXME: with only functions and topevel, it's alright. If we put
     more, we have to be careful because c1' also happens after c2',
     etc. *)

  | Until (c1', c2') ->
     command__to__instruction ~pos e (While (Morsmall.Location.copy_location c1' (Not c1'), c2'))

  | Function (n, c1') ->
     E.check_legal_function_name ~pos e n;
     E.with_deeper e @@ fun e ->
     E.with_inside_function_body e @@ fun e ->
     let (e, i) = command'__to__instruction (E.add_function e n C.itrue) c1' in
     (E.replace_function e n i, C.itrue)

  | Redirection ({value=Simple ([], [word']);_}, _, OutputDuplicate, {value=[WLiteral _];_})
    when e.at_toplevel && (try snd (word'__to__name e word') = "exec" with _ -> false) ->
    (* Special case of [exec 2>&1] that has no impact at toplevel. *)
    (e, C.itrue)

  | Redirection _ as command ->
     E.with_deeper e @@ fun e ->
     (e, redirection__to__instruction ~pos e command)

  | HereDocument _ ->
     unsupported ~pos "here document"

and command'__to__instruction env command' =
  on_located_with_env command__to__instruction env command'

and case_item'_list__to__if_sequence fresh_var env = function
  | [] -> assert false

  | [{value=({value=[[WGlobAll]];_}, command'_option);_}] ->
     (* When the last case_item is "* )", it's the "else" part of our
        if sequence. *)
     command'_option__to__instruction env command'_option

  | [case_item'] ->
     let (pattern', command'_option) = case_item'.value in
     let (env0, instruction0) = pattern'__to__instruction fresh_var env pattern' in
     let (env1, instruction1) = command'_option__to__instruction env0 command'_option in
     (env1, C.IIf (instruction0, instruction1, C.icolon))

  | case_item' :: case_item'_list ->
     let (pattern', command'_option) = case_item'.value in
     let (env0, instruction0) = pattern'__to__instruction fresh_var env pattern' in
     let (env1, instruction1) = command'_option__to__instruction env0 command'_option in
     let (_   , instruction2) = case_item'_list__to__if_sequence fresh_var env0 case_item'_list in  (* Warning: it's env0! *)
     (env1, C.IIf (instruction0, instruction1, instruction2))
(* FIXME: with only functions and topevel, it's alright. If we put
     more, we have to be careful about merging env1 and env2. *)

and command'_option__to__instruction env = function
  | None -> (env, C.itrue)
  | Some command' -> command'__to__instruction env command'

and pattern__to__instruction ~pos fresh_var = function
  | [] -> unsupported ~pos "empty pattern"
  | pat0 :: pats ->
    let test_eq_from_pat = function
      | [WLiteral word] ->
        C.ICallUtility ("test", [(SVariable fresh_var, DontSplit); (SLiteral "=", DontSplit); (SLiteral word, DontSplit)])
      | _ ->
        unsupported ~pos "case when non-literal patterns"
    in
    List.fold_left
      (fun instruction pat ->
         C.ior (instruction, test_eq_from_pat pat))
      (test_eq_from_pat pat0)
      pats

and pattern'__to__instruction fresh_var env pattern' =
  (env, on_located (pattern__to__instruction fresh_var) pattern')

and redirection__to__instruction ~pos e = function
  (* Anything redirected anywhere when at toplevel => we don't care. Indeed, at
     toplevel, we don't care about the content of 1. So if something is being
     added to 1 or removed from 1, who cares? *)
  | Redirection (command', _, OutputDuplicate, {value=[WLiteral _];_})
      when e.at_toplevel ->
    snd (command'__to__instruction e command')

  (* >=2 redirected to /dev/null. Since they don't have any impact on
     the semantics of the program, we don't care. *)
  | Redirection (command', descr, Output, {value=[WLiteral "/dev/null"];_})
       when descr >= 2 ->
     snd (command'__to__instruction e command')

  (* 1 redirected to >=2, this means the output will never ever have
     an impact on the semantics again ==> ignore *)
  | Redirection (command', 1, OutputDuplicate, {value=[WLiteral i];_})
       when (try int_of_string i >= 2 with Failure _ ->  false) ->
     C.INoOutput (snd (command'__to__instruction e command'))

  (* 1 redirected to /dev/null. This means that the output will never
     have an impact on the semantics again ==> Ignore. In fact, we can
     even be a bit better an accept all subsequent redirections of >=2
     to 1. *)
  | Redirection (command', 1, Output, {value=[WLiteral "/dev/null"];_}) ->
     (
       let rec flush_redirections_to_1 ~pos = ignore pos; function
         | Redirection (command', descr, OutputDuplicate, {value=[WLiteral "1"];_})
              when descr >= 2 ->
            flush_redirections'_to_1 command'
         | _ as command -> command
       and flush_redirections'_to_1 redirection' =
         on_located flush_redirections_to_1 redirection'
       in
       C.INoOutput (snd (command__to__instruction ~pos e (flush_redirections'_to_1 command')))
     )

  | _ -> unsupported ~pos "other redirections"

and command'_list__to__instruction env = function
  | [] ->
     (env, C.itrue)
  | command'_list ->
     let (env, instruction_list) = list_fold_map command'__to__instruction env command'_list in
     (env, C.isequence_l instruction_list)

and parse_file_in_env env file =
  try
    Morsmall.parse_file file
    |> command'_list__to__instruction env
  with
  | Sys_error msg -> raise (Errors.FileError msg)
  | Morsmall.SyntaxError pos -> raise (Errors.ParseError ("", pos))

let env_instruction__to__program (env, instruction) =
  { C.function_definitions = E.get_functions env ; instruction }

let program__to__program ~cmd_line_arguments (command'_list : program) : C.program =
  command'_list__to__instruction (E.empty cmd_line_arguments) command'_list
  |> env_instruction__to__program

let parse_file ~cmd_line_arguments file =
  parse_file_in_env (E.empty cmd_line_arguments) file
  |> env_instruction__to__program
