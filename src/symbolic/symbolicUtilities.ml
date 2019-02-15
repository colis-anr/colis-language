open Format
open Constraints
open Clause
open UtilitiesSpecification
open Semantics__Buffers
open SymbolicInterpreter__State

type env = (string * string) list
type args = string list

(** Get the name of the last path component, if any, or of the hint
   root variable otherwise. The result is useful as a hint for
   creating variables for resolving the path. *)
let last_comp_as_hint: root:Var.t -> Path.t -> string option =
  fun ~root path ->
    match Path.split_last path with
    | Some (_, Down f) ->
      Some (Feat.to_string f)
    | None -> (* Empty parent path => root *)
      Some (Var.hint root)
    | Some (_, (Here|Up)) ->
      None (* We can’t know (if last component in parent path is a symbolic link) *)

(** Error utility with optional message *)
let error ?msg () : utility =
  fun sta ->
    let sta' =
      match msg with
      | Some msg ->
        let str = "[ERR] "^msg in
        let stdout = Stdout.(output str sta.stdout |> newline) in
        {sta with stdout}
      | None -> sta
    in
    [ sta', false ]

(** Wrapper around [error] in case of unknown utility. *)
let unknown_utility ?(msg="Unknown utility") ~name () =
  if !Options.fail_on_unknown_utilities then
    raise (Errors.UnsupportedUtility (name, msg))
  else
    error ~msg:(msg ^ ": " ^ name) ()

(** Wrapper around [error] in case of unknown argument. *)
let unknown_argument ?(msg="Unknown argument") ~name ~arg () =
  if !Options.fail_on_unknown_utilities then
    raise (Errors.UnsupportedArgument (name, msg, arg))
  else
    error ~msg:(msg ^ ": " ^ arg) ()

(******************************************************************************)
(*                                  true/false                                *)
(******************************************************************************)

let return result : utility =
  fun sta -> [sta, result]

let interp_true : env -> args -> utility =
  fun _ _ ->
    return true

let interp_false : env -> args -> utility =
  fun _ _ ->
    return false

(******************************************************************************)
(*                                     echo                                   *)
(******************************************************************************)

let interp_echo : env -> args -> utility =
  fun _ args sta ->
    let open Semantics__Buffers in
    let open SymbolicInterpreter__State in
    let str = String.concat " " args in
    let stdout = Stdout.(output str sta.stdout |> newline) in
    [ {sta with stdout}, true ]

(******************************************************************************)
(*                                     touch                                  *)
(******************************************************************************)

let interp_touch1 path_str : utility =
  under_specifications @@ fun ~cwd ~root ~root' ->
  let p = Path.from_string path_str in
  match Path.split_last p with
  | None -> (* `touch ''` *)
    failure ~error_message:"cannot touch '': No such file or directory" ()
  | Some (q, comp) ->
    let common_cases =
      let hint = last_comp_as_hint ~root p in [
        success_case
          ~descr:(asprintf "touch %a: path resolves" Path.pp p)
          begin
            exists ?hint @@ fun y ->
            resolve root cwd p y &
            eq root root'
          end;
        error_case
          ~descr:(asprintf "touch %a: parent path does not resolve" Path.pp p)
          begin
            noresolve root cwd q &
            eq root root';
          end
      ]
    in
    let feature_cases f =
      let hintx = last_comp_as_hint ~root q in
      let hinty = Feat.to_string f in [
        success_case
          ~descr:(asprintf "touch %a: create file" Path.pp p)
          begin
            exists3 ?hint1:hintx ?hint2:hintx ~hint3:hinty @@ fun x x' y' ->
            resolve root cwd q x &
            dir x &
            abs x f &
            similar root root' cwd q x x' &
            sim x (Feat.Set.singleton f) x' &
            dir x' &
            feat x' f y' &
            reg y'
          end;
        error_case
          ~descr:(asprintf "touch %a: parent path is not directory" Path.pp p)
          begin
            exists ?hint:hintx @@ fun x ->
            resolve root cwd q x &
            ndir x &
            eq root root'
          end
      ]
    in
    match comp with
    | Up | Here ->
      common_cases
    | Down f ->
      common_cases @ feature_cases f

let interp_touch : env -> args -> utility =
  fun _ -> function
  | [arg] -> interp_touch1 arg
  | _ -> unknown_argument ~msg:"multiple arguments"  ~name:"touch" ~arg:"" ()

(******************************************************************************)
(*                                     mkdir                                  *)
(******************************************************************************)

let interp_mkdir1 path_str : utility =
  under_specifications @@ fun ~cwd ~root ~root' ->
  let p = Path.from_string path_str in
  match Path.split_last p with
  | None ->
    failure ~error_message:"mkdir: cannot create directory ''" ()
  | Some (_q, (Here|Up)) ->
    failure ~error_message:"mkdir: file exists" () (* CHECK *)
  | Some (q, Down f) ->
    let hintx = last_comp_as_hint ~root q in
    let hinty = Feat.to_string f in [
      success_case
        ~descr:(asprintf "mkdir %a: create directory" Path.pp p)
        begin
          exists2 ?hint1:hintx ?hint2:hintx @@ fun x x' ->
          exists ~hint:hinty @@ fun y ->
          resolve root cwd q x &
          dir x &
          abs x f &
          similar root root' cwd q x x' &
          sim x (Feat.Set.singleton f) x' &
          dir x' &
          feat x' f y &
          dir y &
          fen y Feat.Set.empty
        end;
      error_case
        ~descr:(asprintf "mkdir %a: target already exists" Path.pp p)
        begin
          exists ?hint:hintx @@ fun x ->
          resolve root cwd q x &
          dir x &
          nabs x f &
          eq root root'
        end;
      error_case
        ~descr:(asprintf "mkdir %a: parent path is file" Path.pp p)
        begin
          exists ?hint:hintx @@ fun x ->
          resolve root cwd q x &
          ndir x &
          eq root root'
        end;
      error_case
        ~descr:(asprintf "mkdir %a: parent path does not resolve" Path.pp p)
        begin
          noresolve root cwd q &
          eq root root'
        end;
    ]

let interp_mkdir : env -> args -> utility =
  fun _ -> function
  | [] -> error ~msg:"mkdir: missing operand" ()
  | [arg] -> interp_mkdir1 arg
  | _ -> unknown_argument ~msg:"multiple arguments" ~name:"mkdir" ~arg:"" ()

(******************************************************************************)
(*                                     test                                   *)
(******************************************************************************)

let interp_test_parse_error args : utility =
  under_specifications @@ fun ~cwd:_ ~root ~root' ->
  [
    let descr = "test: parse error in `" ^ (String.concat " " args) ^ "`" in
    error_case
      ~descr
      begin
        eq root root'
      end;
  ]

let interp_test_empty () : utility =
  under_specifications @@ fun ~cwd:_ ~root ~root' ->
  [
    let descr = "test: empty expression" in
    error_case
      ~descr
      begin
        eq root root'
      end;
  ]

let interp_test_e path_str : utility =
  under_specifications @@ fun ~cwd ~root ~root' ->
  let p = Path.from_string path_str in
  let hintx = last_comp_as_hint ~root p in [
    success_case
      ~descr:(asprintf "test -e %a: path resolves" Path.pp p)
      begin
        exists ?hint:hintx @@ fun x ->
        resolve root cwd p x &
        eq root root'
      end;
    error_case
      ~descr:(asprintf "test -e %a: path does not resolve" Path.pp p)
      begin
        noresolve root cwd p &
        eq root root'
      end;
  ]

let interp_test_d path_str : utility =
  under_specifications @@ fun ~cwd ~root ~root' ->
  let p = Path.from_string path_str in
  let hintx = last_comp_as_hint ~root p in [
    success_case
      ~descr:(asprintf "test -d %a: path resolves to a dir" Path.pp p)
      begin
        exists ?hint:hintx @@ fun x ->
        resolve root cwd p x & dir x &
        eq root root'
      end;
    error_case
      ~descr:(asprintf "test -d %a: path does not resolve" Path.pp p)
      begin
        noresolve root cwd p &
        eq root root'
      end;
    error_case
      ~descr:(asprintf "test -d %a: path resolves but not to a dir" Path.pp p)
      begin
        exists ?hint:hintx @@ fun x ->
        resolve root cwd p x & ndir x &
        eq root root'
      end;
  ]

let interp_test_f path_str : utility =
  under_specifications @@ fun ~cwd ~root ~root' ->
  let p = Path.from_string path_str in
  let hintx = last_comp_as_hint ~root p in [
    success_case
      ~descr:(asprintf "test -f %a: path resolves to a regular file" Path.pp p)
      begin
        exists ?hint:hintx @@ fun x ->
        resolve root cwd p x & reg x &
        eq root root'
      end;
    error_case
      ~descr:(asprintf "test -f %a: path does not resolve" Path.pp p)
      begin
        noresolve root cwd p &
        eq root root'
      end;
    error_case
      ~descr:(asprintf "test -f %a: path resolves but not to a regular file" Path.pp p)
      begin
        exists ?hint:hintx @@ fun x ->
        resolve root cwd p x & nreg x &
        eq root root'
      end;
  ]

let interp_test_x ?(name="test -x") path_str : utility =
  under_specifications @@ fun ~cwd ~root ~root' ->
  let p = Path.from_string path_str in
  let hintx = last_comp_as_hint ~root p in [
    success_case
      ~descr:(asprintf "%s '%a': path resolves to an executable (overapprox to -e)" name Path.pp p)
      begin
        exists ?hint:hintx @@ fun x ->
        resolve root cwd p x & (* no way to constraint "x" mode *)
        eq root root'
      end;
    error_case
      ~descr:(asprintf "%s '%a': path does not resolve" name Path.pp p)
      begin
        noresolve root cwd p &
        eq root root'
      end;
    error_case
      ~descr:(asprintf "%s '%a': path resolves but not to an executable (overapprox to -e)" name Path.pp p)
      begin
        exists ?hint:hintx @@ fun x ->
        resolve root cwd p x &  (* no way to constraint no "x" mode *)
        eq root root'
      end;
  ]

let interp_test_n str : utility =
  under_specifications @@ fun ~cwd:_cwd ~root ~root' ->
  if str = "" then
    [
      error_case
        ~descr:(asprintf "test -n '%s': string is empty" str)
      begin
        eq root root'
      end
    ]
  else
    [
      success_case
        ~descr:(asprintf "test -n '%s': string is non-empty" str)
      begin
        eq root root'
      end
    ]

let interp_test_z str : utility =
  under_specifications @@ fun ~cwd:_cwd ~root ~root' ->
  if str = "" then
    [
      success_case
        ~descr:(asprintf "test -z '%s': string is empty" str)
      begin
        eq root root'
      end
    ]
  else
    [
      error_case
        ~descr:(asprintf "test -z '%s': string is non-empty" str)
      begin
        eq root root'
      end
    ]

let interp_test_string_equal s1 s2 : utility =
  under_specifications @@ fun ~cwd:_cwd ~root ~root' ->
  if s1 = s2 then
    [
      success_case
        ~descr:(asprintf "test '%s' = '%s': strings are equal" s1 s2)
      begin
        eq root root'
      end
    ]
  else
    [
      error_case
        ~descr:(asprintf "test '%s' = '%s': string are not equal" s1 s2)
      begin
        eq root root'
      end
    ]

let interp_test_string_notequal s1 s2 : utility =
  under_specifications @@ fun ~cwd ~root ~root' ->
  if s1 <> s2 then
    [
      success_case
        ~descr:(asprintf "test '%s' != '%s': strings are not equal" s1 s2)
      begin
        eq root root'
      end
    ]
  else
    [
      error_case
        ~descr:(asprintf "test '%s' != '%s': string are equal" s1 s2)
      begin
        eq root root'
      end
    ]

let interp_test_neg (u:utility) : utility = fun st ->
  List.map (fun (s,b) -> (s, not b)) (u st)

let interp_test_and (u1:utility) (u2:utility) : utility = fun st ->
  List.flatten
    (List.map
       (fun (s1,b1) ->
         List.map (fun (s2,b2) -> (s2, b1 && b2)) (u2 s1))
       (u1 st))

let interp_test_or (u1:utility) (u2:utility) : utility = fun st ->
  List.flatten
    (List.map
       (fun (s1,b1) ->
         List.map (fun (s2,b2) -> (s2, b1 || b2)) (u2 s1))
       (u1 st))

let rec interp_test_expr e : utility =
  let name = "test" in
  let msg what = "unsupported " ^ what in
  Morsmall_utilities.TestParser.(
  match e with
  | Unary("-e",arg) -> interp_test_e arg
  | Unary("-d",arg) -> interp_test_d arg
  | Unary("-f",arg) -> interp_test_f arg
  | Unary("-x",arg) -> interp_test_x arg
  | Unary("-n",arg) -> interp_test_n arg
  | Unary("-z",arg) -> interp_test_z arg
  | Binary ("=",a1,a2) -> interp_test_string_equal a1 a2
  | Binary ("!=",a1,a2) -> interp_test_string_notequal a1 a2
  | Unary(op,_) ->
     let msg = msg "unary operator" in
     unknown_argument ~msg ~name ~arg:op ()
  | And(e1,e2) ->
     interp_test_and (interp_test_expr e1) (interp_test_expr e2)
  | Or(e1,e2) ->
     interp_test_or (interp_test_expr e1) (interp_test_expr e2)
  | Not(e1) -> interp_test_neg (interp_test_expr e1)
  | Binary (op,_e1,_e2) ->
     let msg = msg "binary operator" in
     unknown_argument ~msg ~name ~arg:op ()
  | Single arg ->
     let msg = msg "single argument" in
     unknown_argument ~msg ~name ~arg ()
  )

let interp_test ~bracket _env (args : string list) : utility =
  match Morsmall_utilities.TestParser.parse ~bracket args with
  | None -> interp_test_empty ()
  | Some e -> interp_test_expr e
  | exception Morsmall_utilities.TestParser.Parse_error ->
     interp_test_parse_error args

(******************************************************************************)
(*                                which                                       *)
(******************************************************************************)

let _interp_which_naive (args: string list) : utility =
  match args with
  | [] ->
     under_specifications @@ fun ~cwd ~root ~root' ->
       [
         error_case
           ~descr:(asprintf "which without argument (returns 1)")
           begin
             eq root root'
           end
       ]
  | [p] ->
     under_specifications @@ fun ~cwd ~root ~root' ->
       [
         success_case
           ~descr:(asprintf "which '%s': assuming command is found" p)
           begin
             eq root root'
           end
       ;
         error_case
           ~descr:(asprintf "which '%s': assuming command is not found" p)
           begin
             eq root root'
           end
       ]
  | p :: _ ->
     unknown_argument ~msg:"more than one argument" ~name:"which" ~arg:p ()

let interp_test_regular_and_x path_str : utility =
  under_specifications @@ fun ~cwd ~root ~root' ->
  let p = Path.from_string path_str in
  let hintx = last_comp_as_hint ~root p in [
    success_case
      ~descr:(asprintf "which '%a': path resolves to a regular executable (overapprox to -f)" Path.pp p)
      ~stdout:Stdout.(empty |> output (asprintf "%a" Path.pp p) |> newline)
      begin
        exists ?hint:hintx @@ fun x ->
        resolve root cwd p x & reg x & (* no way to constraint "x" mode *)
        eq root root'
      end;
    error_case
      ~descr:(asprintf "which '%a': path does not resolve" Path.pp p)
      begin
        noresolve root cwd p &
        eq root root'
      end;
    error_case
      ~descr:(asprintf "which '%a': path resolves but not to regular executable)" Path.pp p)
      begin
        exists ?hint:hintx @@ fun x ->
        resolve root cwd p x & (* no way to constraint no "x" mode *)
        eq root root'
      end;
  ]


let rec search_as_which_in_path (path:string list) arg : utility =
  match path with
  | [] ->
     under_specifications @@ fun ~cwd:_ ~root ~root' ->
       [
         error_case
           ~descr:(asprintf "which: `%s` not found in PATH" arg)
           begin
             eq root root'
           end
       ]
  | p :: rem ->
     let u1 = interp_test_regular_and_x (p ^ "/" ^ arg) in
     let u2 = search_as_which_in_path rem arg in
     fun st ->
     List.flatten
       (List.map
          (function (s1,b1) as x -> if b1 then [x] else u2 s1)
          (u1 st))

let search_as_which (path:string list) arg : utility =
  match Path.from_string arg with
  | Abs _ -> interp_test_regular_and_x arg
  | Rel [] -> assert false
  | Rel [_] -> search_as_which_in_path path arg
  | Rel r ->
     fun st ->
     let a = Path.concat st.filesystem.cwd r in
     interp_test_regular_and_x (Path.to_string a) st


let interp_which_full _env (* envPATH:string *) (args:string list) : utility =
  match args with
  | [] ->
     under_specifications @@ fun ~cwd ~root ~root' ->
       [
         error_case
           ~descr:(asprintf "which without argument (returns 1)")
           begin
             eq root root'
           end
       ]
  | ["-a"] ->
     unknown_argument ~msg:"option `-a`" ~name:"which" ~arg:"-a" ()
  | _ ->
     (* FIXME     let path = String.split_on_char ':' envPATH in *)
     let path = [ "/usr/sbin" ; "/usr/bin" ; "/sbin" ; "/bin" (* ; "/usr/games" *) ] in
     let rec aux args =
       match args with
       | [] -> assert false
       | [a] -> search_as_which path a
       | a :: rem ->
          interp_test_and (search_as_which path a) (aux rem)
     in
     aux args

(**************************************************************************)
(*                     update-alternatives                                *)
(**************************************************************************)

let interp_update_alternatives _env args =
  let name = "update-alternatives" in
  let rec aux = function
    | [] ->
       unknown_argument ~msg:"update-alternatives: no command found" ~name ~arg:"(none)" ()
    | "--quiet" :: rem->
       fun st -> aux rem (print_utility_trace (name ^ ": ignored option --quiet") st)
    | arg :: _ ->
       unknown_argument ~msg:"update-alternatives: unsupported argument" ~name ~arg ()
  in
  aux args

(******************************************************************************)
(*                      Dispatch interpretation of utilities                  *)
(******************************************************************************)

let interp (name: string) : env -> args -> utility =
  match name with
  | "true" -> interp_true
  | "false" -> interp_false
  | "echo" -> interp_echo
  | "test" -> interp_test ~bracket:false
  | "[" -> interp_test ~bracket:true
  | "touch" -> interp_touch
  | "mkdir" -> interp_mkdir
  | "which" -> interp_which_full
  | "update-alternatives" -> interp_update_alternatives
  | _ -> fun _env _args -> unknown_utility ~name ()
