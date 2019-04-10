open Format
open Constraints
open Clause
open UtilitiesSpecification
open Semantics__Buffers

module IdMap = Env.IdMap

type env = string IdMap.t

type args = string list

module type SYMBOLIC_UTILITY = sig
  val interprete : env -> args -> utility
end

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
    let open SymbolicInterpreter__Semantics in
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

let interp_test_h str : utility =
  assert false (* FIXME : we need test -h *)
  
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
  | Unary("-h",arg) -> interp_test_h arg
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
(*                                rm                                          *)
(******************************************************************************)
let interp_rm1 arg : utility =
  under_specifications @@ fun ~cwd ~root ~root' ->
  let q = Path.from_string arg in
  match Path.split_last q with
  (* FIXME: Here, I reuse the same programming scheme as in mkdir. *)
  (* FIXME: Shouldn't we factorize it in a combinator?             *)
  | None ->
     failure ~error_message:"rm: invalid path ''" ()
  | Some (_q, (Here|Up)) ->
     failure ~error_message:"rm: cannot remove .. or ." ()
  | Some (q, Down f) ->
     let hintx = last_comp_as_hint ~root q in
     let hinty = Feat.to_string f in [
         success_case
           ~descr:(asprintf "rm %a: remove file" Path.pp q)
           begin
             exists2 ?hint1:hintx ?hint2:hintx @@ fun x x' ->
             exists ~hint:hinty @@ fun y ->
             resolve root cwd q y & ndir y
             & similar root root' cwd q x x'
             & sim x (Feat.Set.singleton f) x'
             & dir x' & fen x' (Feat.Set.singleton f)
           end;
         error_case
           ~descr:(asprintf "rm %a: target is a directory" Path.pp q)
           begin
             exists ~hint:hinty @@ fun y ->
             resolve root cwd q y & dir y
             & eq root root'
           end;
         error_case
           ~descr:(asprintf "rm %a: target does not exist" Path.pp q)
           begin
             exists ~hint:hinty @@ fun y ->
             noresolve root cwd q & eq root root'
           end;
       ]

let interp_rm1_r arg : utility =
  under_specifications @@ fun ~cwd ~root ~root' ->
  let q = Path.from_string arg in
  match Path.split_last q with
  (* FIXME: Here, I reuse the same programming scheme as in mkdir. *)
  (* FIXME: Shouldn't we factorize it in a combinator?             *)
  | None ->
     failure ~error_message:"rm: invalid path ''" ()
  | Some (_q, (Here|Up)) ->
     failure ~error_message:"rm: cannot remove .. or ." ()
  | Some (q, Down f) ->
     let hintx = last_comp_as_hint ~root q in
     let hinty = Feat.to_string f in [
         success_case
           ~descr:(asprintf "rm -r %a: remove file or directory" Path.pp q)
           begin
             exists2 ?hint1:hintx ?hint2:hintx @@ fun x x' ->
             exists ~hint:hinty @@ fun y ->
             resolve root cwd q y & ndir y
             & similar root root' cwd q x x'
             & sim x (Feat.Set.singleton f) x'
             & dir x' & fen x' (Feat.Set.singleton f)
           end;
         error_case
           ~descr:(asprintf "rm -r %a: target does not exist" Path.pp q)
           begin
             exists ~hint:hinty @@ fun y ->
             noresolve root cwd q & eq root root'
           end;
       ]

let interp_rm_r : env -> args -> utility =
  fun _ -> function
  | [] -> error ~msg:"rm: missing operand" ()
  | [arg] -> interp_rm1_r arg
  | _ -> unknown_argument ~msg:"multiple arguments" ~name:"rm -r/R" ~arg:"" ()

let interp_rm : env -> args -> utility =
  fun env -> function
  | [] -> error ~msg:"rm: missing operand" ()
  | ("-r" | "-R") :: args -> interp_rm_r env args
  | [arg] -> interp_rm1 arg
  | _ -> unknown_argument ~msg:"multiple arguments" ~name:"rm" ~arg:"" ()

(******************************************************************************)
(*                                  mv                                        *)
(******************************************************************************)

(* FIXME: this should go into a separate file *)

module Mv:SYMBOLIC_UTILITY = struct
  let interprete (env:env) (args:args) =
    unknown_utility ~name:"mv" ()
end                                              

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
       (* TODO: return error state *)
       unknown_argument ~msg:"no sub-command found" ~name ~arg:"(none)" ()
    | "--quiet" :: rem->
       fun st -> aux rem (print_utility_trace (name ^ ": ignored option --quiet") st)
    | arg :: _ ->
       unknown_argument ~msg:"unsupported argument" ~name ~arg ()
  in
  aux args

(**************************************************************************)
(*                                    dpkg                                *)
(**************************************************************************)

let interp_dpkg _env args =
  let name = "dpkg" in
  let aux = function
    | ["-L"; pkg_name] ->
       unknown_argument ~msg:"support for -L not yet implemented" ~name ~arg:pkg_name ()
    | "-L" :: _ ->
       unknown_argument ~msg:"option -L expects exactly one argument" ~name ~arg:"(any)" ()
    | ["--compare-versions"; v1; v2] ->
       unknown_argument ~msg:"support for --compare-versions not yet implemented" ~name ~arg:(v1 ^ " " ^v2) ()
    | "--compare-versions" :: _ ->
       unknown_argument ~msg:"option --compare-versions expects exactly two arguments" ~name ~arg:"(any)" ()
    | [] ->
       (* TODO: return error state *)
       unknown_argument ~msg:"no argument found" ~name ~arg:"(none)" ()
    | arg :: _ ->
       unknown_argument ~msg:"unsupported argument" ~name ~arg ()
  in
  aux args

(******************************************************************************)
(*                       dpkg_maintscript_helper                              *)
(******************************************************************************)

(* FIXME: this should go into a separate file *)

module DpkgMaintScriptHelper:SYMBOLIC_UTILITY = struct
  let supports _env args =
    match args with
    |["rm_conffile"]|["mv_conffile"]|["symlink_to_dir"]|["dir_to_symlink"]
     -> return true
    | _ -> return false

  exception NoDashDash
  let split_at_dashdash l =
    (* split a list into the part before "--" and the part after *)
    let rec split_aux acc = function
      | "--"::rest -> (List.rev acc, rest)
      | head::rest -> split_aux (head::acc) rest
      | [] -> raise NoDashDash
    in split_aux [] l

  let (||>>) = compose_non_strict
  let starts_on_slash s = String.length s > 0 && s.[0]='/'
  let ends_on_slash s = let n = String.length s in n > 0 && s.[n-1]='/'
  let empty_string s = (String.length s = 0)
  let dpkg_compare_versions_le_nl s1 s2 = assert false (* FIXME *)
  let dpkg_validate_version s = true (* FIXME *)
  let validate_optional_version s =
    empty_string s || dpkg_validate_version s
  let ensure_package_owns_file package file = true (* FIXME *)
  let conffiles package = [] (* FIXME *)
  let contents package = [] (* FIXME *)
  let is_pathprefix p1 p2 =
    (* check whether [p1^'/'] is a prefix of [p2] *)
    let rec forall_from_to lower upper pred =
      (* check [(pred lower) && .... && (pred upper)] *)
      if lower > upper then true
      else pred lower && forall_from_to (lower+1) upper pred
    in
    let n1 = String.length p1
    and n2 = String.length p2
    in
    if n1+1 >= n2
    then
      false
    else
      p2.[n1]='/' && forall_from_to 0 (n1-1) (function i -> p1.[i]=p2.[i])

  let interp_test_fence pathname arity = return true (* FIXME *)
                                  
  exception Error of string
  exception NumberOfArguments
  exception MaintainerScriptArguments

  let prepare_rm_conffile env conffile package =
    if ensure_package_owns_file package conffile
    then choice
           (Mv.interprete env [
                "-f";
                conffile;
                conffile^".dpkg-backup"])
           (Mv.interprete env [
                "-f";
                conffile;
                conffile^".dpkg-remove"])
    else return true

  let finish_rm_conffile env conffile =
    (if_then_else
       (interp_test_e (conffile^".dpkg-backup"))
       (* TODO: echo .... in positive case *)
       (Mv.interprete env
          ["-f"; conffile^".dpkg-backup"; conffile^".dpkg-bak"])
       (return true))
  ||>>
    (if_then_else
       (interp_test_e (conffile^".dpkg-remove"))
       (* TODO echo ... in positive case *)
       (interp_rm env
          ["-f"; conffile^".dpkg-remove"])
       (return true)
    )
    
  let abort_rm_conffile env conffile package =
    if ensure_package_owns_file package conffile
    then
      (if_then_else
         (interp_test_e (conffile^".dpkg-remove"))
         (* TODO echo ... in positive case *)
         (Mv.interprete env [conffile^".dpkg-remove"; conffile])
         (return true))
      ||>>
        (if_then_else
           (interp_test_e (conffile^".dpkg-backup"))
           (* TODO echo ... in positive case *)
           (Mv.interprete env [conffile^".dpkg-backup"; conffile])
           (return true))
    else
      return true
    
  let rm_conffile env cmdargs scriptarg1 scriptarg2 =
    let dpkg_package =
      try IdMap.find "DPKG_MAINTSCRIPT_PACKAGE" env
      with Not_found ->
        raise (Error
                 "environment variable DPKG_MAINTSCRIPT_PACKAGE is required")
    in
    let default_package =
      try
        let dpkg_arch = IdMap.find "DPKG_MAINTSCRIPT_ARCH" env
        in dpkg_package^":"^dpkg_arch
      with Not_found -> dpkg_package
    in
    let dpkg_maintscript_name =
      try IdMap.find "DPKG_MAINTSCRIPT_NAME" env
      with
      | Not_found ->
         raise (Error
                  "environment variable DPKG_MAINTSCRIPT_PACKAGE is required")
    in
    let (conffile,lastversion,package) =
      match cmdargs with
      | [x;y;z] -> (x,y,if empty_string z then default_package else z)
      | [x;y] -> (x,y,default_package)
      | [x] -> (x,"",default_package)
      | _ -> raise NumberOfArguments
    in
    if empty_string package then
      raise (Error "couldn't identify the package");
    (* checking scriptarg1 done by [interprete] *)
    (* checking DPKG_MAINTSCRIPTNAME done above *)
    (* checking DPKG_MAINTSCRIPT_PACKAGE done above *)
    if not (starts_on_slash conffile) then
      raise (Error "conffile '$CONFFILE' is not an absolute path");
    if not (validate_optional_version lastversion) then
      raise (Error ("wrong version "^lastversion));
    match dpkg_maintscript_name with
    | "preinst" ->
       if (scriptarg1 = "install" || scriptarg1 = "upgrade")
          && not (empty_string scriptarg2)
          && dpkg_compare_versions_le_nl scriptarg2 lastversion
       then prepare_rm_conffile env conffile package
       else return true
    | "postinst" ->
       if scriptarg1 = "configure"
          && not (empty_string scriptarg2)
          && dpkg_compare_versions_le_nl scriptarg2 lastversion
       then finish_rm_conffile env conffile
       else return true
    | "postrm" ->
       if scriptarg1 = "purge"
       then interp_rm env
              [ "-f";
                conffile^".dpkg-bak";
                conffile^".dpkg-remove"; 
                conffile^".dpkg-backup"]
       else
         if (scriptarg1 = "abort-install" || scriptarg1 = "abort-upgrade")
            && not (empty_string scriptarg2)
            && dpkg_compare_versions_le_nl scriptarg2 lastversion
         then abort_rm_conffile env conffile package
         else return true
    | _ -> return true

  let prepare_mv_conffile env conffile package =
    if_then_else
      (interp_test_e conffile)
      (if ensure_package_owns_file package conffile
       then choice
              (Mv.interprete env ["-f"; conffile; conffile^".dpkg-remove"])
              (return true)
       else return true)
      (return true)
    
  let finish_mv_conffile env oldconffile newconffile package =
    (interp_rm env ["-f"; oldconffile^".dpkg-remove"])
    ||>>
      (if_then_else
         (interp_test_e oldconffile)
         (if ensure_package_owns_file package oldconffile
          then
            (* TODO echo bla bla *)
            compose_non_strict
              (if_then_else
                 (interp_test_e newconffile)
                 (Mv.interprete env
                    ["-f";newconffile; newconffile^".dpkg-new"])
                 (return true))
              (Mv.interprete env ["-f"; oldconffile; newconffile])
          else return true
         )
         (return true)
      )
    
  let abort_mv_conffile env conffile package =
    if ensure_package_owns_file package conffile
    then
      if_then_else
        (interp_test_e (conffile^".dpkg-remove"))
        (* TODO echo bla bla *)
        (Mv.interprete env [conffile^".dpkg-remove"; conffile])
        (return true)
    else return true
    
  let mv_conffile env cmdargs scriptarg1 scriptarg2 =
    let dpkg_package =
      try IdMap.find "DPKG_MAINTSCRIPT_PACKAGE" env
      with Not_found ->
        raise (Error
                 "environment variable DPKG_MAINTSCRIPT_PACKAGE is required")
    in
    let default_package =
      try
        let dpkg_arch = IdMap.find "DPKG_MAINTSCRIPT_ARCH" env
        in dpkg_package^":"^dpkg_arch
      with Not_found -> dpkg_package
    in
    let dpkg_maintscript_name =
      try IdMap.find "DPKG_MAINTSCRIPT_NAME" env
      with
      | Not_found ->
         raise (Error
                  "environment variable DPKG_MAINTSCRIPT_PACKAGE is required")
    in
    let (oldconffile,newconffile,lastversion,package) =
      match cmdargs with
      | [w;x;y;z] -> (w,x,y,if empty_string z then default_package else z)
      | [w;x;y] -> (w,x,y,default_package)
      | [w;x] -> (w,x,"",default_package)
      | _ -> raise NumberOfArguments
    in
    if empty_string package then
      raise (Error "couldn't identify the package");
    (* checking scriptarg1 done by [interprete] *)
    (* checking DPKG_MAINTSCRIPT_NAME done above *)
    (* checking DPKG_MAINTSCRIPT_PACKAGE done above *)
    if not (starts_on_slash oldconffile) then
      raise (Error "conffile '$OLDCONFFILE' is not an absolute path");
    if not (starts_on_slash newconffile) then
      raise (Error "conffile '$NEWCONFFILE' is not an absolute path");
    if not (validate_optional_version lastversion) then
      raise (Error ("wrong version "^lastversion));
    match dpkg_maintscript_name with
    | "preinst" ->
       if (scriptarg1 = "install" || scriptarg1 = "upgrade")
          && not (empty_string scriptarg2)
          && dpkg_compare_versions_le_nl scriptarg2 lastversion
       then prepare_mv_conffile env oldconffile package
       else return true
    | "postinst" ->
       if scriptarg1 = "configure"
          && not (empty_string scriptarg2)
          && dpkg_compare_versions_le_nl scriptarg2 lastversion
       then finish_mv_conffile env oldconffile newconffile package
       else return true
    | "postrm" ->
         if (scriptarg1 = "abort-install" || scriptarg1 = "abort-upgrade")
            && not (empty_string scriptarg2)
            && dpkg_compare_versions_le_nl scriptarg2 lastversion
         then abort_mv_conffile env oldconffile package
         else return true
    | _ -> return true

  let symlink_match link target = assert false (* FIXME *)
    
  let symlink_to_dir env cmdargs scriptarg1 scriptarg2 =
    let dpkg_package =
      try IdMap.find "DPKG_MAINTSCRIPT_PACKAGE" env
      with Not_found ->
        raise (Error
                 "environment variable DPKG_MAINTSCRIPT_PACKAGE is required")
    in
    let default_package =
      try
        let dpkg_arch = IdMap.find "DPKG_MAINTSCRIPT_ARCH" env
        in dpkg_package^":"^dpkg_arch
      with Not_found -> dpkg_package
    in
    let dpkg_maintscript_name =
      try IdMap.find "DPKG_MAINTSCRIPT_NAME" env
      with
      | Not_found ->
         raise (Error
                  "environment variable DPKG_MAINTSCRIPT_PACKAGE is required")
    in
    let (symlink,symlink_target,lastversion,package) =
      match cmdargs with
      | [w;x;y;z] -> (w,x,y,if empty_string z then default_package else z)
      | [w;x;y] -> (w,x,y,default_package)
      | [w;x] -> (w,x,"",default_package)
      | _ -> raise NumberOfArguments
    in
    if empty_string package then
      raise (Error "couldn't identify the package");
    if empty_string symlink then
      raise (Error "symlink parameter is missing");
    if not (starts_on_slash symlink) then
      raise (Error "symlink pathname is not an absolute path");
    if ends_on_slash symlink then
      raise (Error "symlink pathname ends with a slash");
    if empty_string symlink_target then
      raise (Error "original symlink target is missing");
    (* checking scriptarg1 done by [interprete] *)
    (* checking DPKG_MAINTSCRIPT_NAME done above *)
    (* checking DPKG_MAINTSCRIPT_PACKAGE done above *)
    if not (validate_optional_version lastversion) then
      raise (Error ("wrong version "^lastversion));
    match dpkg_maintscript_name with
    | "preinst" ->
       if (scriptarg1 = "install" || scriptarg1 = "upgrade" )
          && not (empty_string scriptarg2)
          && dpkg_compare_versions_le_nl scriptarg2 lastversion
       then
         if_then_else
           (interp_test_h symlink)
           (if_then_else
              (symlink_match symlink symlink_target)
              (Mv.interprete env [symlink; (symlink^".dpkg-backup")])
              (return true))
           (return true)
       else return true
    | "postinst" ->
       if scriptarg1 = "configure"
       then
         if_then_else
           (interp_test_h (symlink^".dpkg-backup"))
           (if_then_else
              (symlink_match (symlink^".dpkg-backup") symlink_target)
              (interp_rm env ["-f"; symlink^".dpkg-backup"])
              (return true))
           (return true)
       else return true
    | "postrm" ->
       (if scriptarg1 = "purge"
        then
          if_then_else
            (interp_test_h (symlink^".dpkg-backup"))
            (interp_rm env ["-f"; symlink^".dpkg-backup"])
            (return true)
        else return true)
       ||>>
         (if (scriptarg1 = "abort-install" || scriptarg1 = "abort-upgrade")
             && not (empty_string scriptarg2)
             && dpkg_compare_versions_le_nl scriptarg2 lastversion
          then
            if_then_else
              (interp_test_e symlink)
              (return true)
              (if_then_else
                 (interp_test_h (symlink^".dpkg-backup"))
                 (if_then_else
                    (symlink_match (symlink^".dpkg-backup") symlink_target)
                    (* FIXME echo Restoring ... *)
                    (Mv.interprete env [symlink^".dpkg-backup"; symlink])
                    (return true))
                 (return true))
          else return true)
    | _ -> return true
         
  let prepare_dir_to_symlink env package pathname =
    (if List.exists
          (function filename -> is_pathprefix pathname filename)
          (conffiles package)
     then raise (Error
                   ("directory '"^pathname^
                      "' contains conffiles, cannot swithc to directory"))
     else return true)
    ||>>
      (if_then_else
         (interp_test_fence
            pathname
            (List.filter
               (function filename -> is_pathprefix pathname filename)
               (* FIXME we also need to remove the pathprefix *) 
               (contents package)))
         (raise (Error
                   ("directory '" ^ pathname
                    ^ "' contains files not owned by '" ^ package
                    ^ "', cannot switch to symlink")))
         (return true))
    ||>>
      (Mv.interprete env ["-f"; pathname; pathname^".dpkg-staging-dir"])
    ||>>
      (interp_mkdir env [pathname])
    ||>>
      (interp_touch env [pathname^"/.dpkg-staging-dir"])

  let finish_dir_to_symlink env pathname symlink_target = assert false

  let abort_dir_to_symlink env pathname symlink_target = assert false
                                                       
  let dir_to_symlink env cmdargs scriptarg1 scriptarg2 =
    let dpkg_package =
      try IdMap.find "DPKG_MAINTSCRIPT_PACKAGE" env
      with Not_found ->
        raise (Error
                 "environment variable DPKG_MAINTSCRIPT_PACKAGE is required")
    in
    let default_package =
      try
        let dpkg_arch = IdMap.find "DPKG_MAINTSCRIPT_ARCH" env
        in dpkg_package^":"^dpkg_arch
      with Not_found -> dpkg_package
    in
    let dpkg_maintscript_name =
      try IdMap.find "DPKG_MAINTSCRIPT_NAME" env
      with
      | Not_found ->
         raise (Error
                  "environment variable DPKG_MAINTSCRIPT_PACKAGE is required")
    in
    let (pathname,symlink_target,lastversion,package) =
      match cmdargs with
      | [w;x;y;z] -> (w,x,y,if empty_string z then default_package else z)
      | [w;x;y] -> (w,x,y,default_package)
      | [w;x] -> (w,x,"",default_package)
      | _ -> raise NumberOfArguments
    in
    (* checking DPKG_MAINTSCRIPT_NAME done above *)
    (* checking DPKG_MAINTSCRIPT_PACKAGE done above *)
    if empty_string package then
      raise (Error "cannot identify the package");
    if empty_string pathname then
      raise (Error "directory parameter is missing");
    if not (starts_on_slash pathname) then
      raise (Error "directory parameter is not an absolute path");
    if empty_string symlink_target then
      raise (Error "new symlink target is missing");
    (* checking scriptarg1 done by [interprete] *)
    if not (validate_optional_version lastversion) then
      raise (Error ("wrong version "^lastversion));
    match dpkg_maintscript_name with
    | "preinst" ->
       if (scriptarg1 = "install" || scriptarg1 = "upgrade" )
          && not (empty_string scriptarg2)
          && dpkg_compare_versions_le_nl scriptarg2 lastversion then
         if_then_else
           (interp_test_h pathname)
           (if_then_else
              (interp_test_d pathname)
              (prepare_dir_to_symlink env package pathname)
              (return true))
           (return true)
       else return true
    | "postinst" ->
       if scriptarg1 = "configure" then
         if_then_else
           (interp_test_d (pathname^".dpkg-backup"))
           (if_then_else
              (interp_test_h pathname)
              (return true)
              (if_then_else
                 (interp_test_d pathname)
                 (if_then_else
                    (interp_test_f (pathname^".dpkg-staging-dir"))
                    (finish_dir_to_symlink env pathname symlink_target)
                    (return true)
                 )
                 (return true)
              )
           )
           (return true)
       else return true
    | "postrm" ->
       (if scriptarg1 = "purge"
        then if_then_else
               (interp_test_d (pathname^".dpkg-backup"))
               (interp_rm env ["-rf"; pathname^".dpkg-backup"])
               (return true)
        else return true
       )
       ||>>
         (if (scriptarg1 = "abort-install" || scriptarg1 = "abort-upgrade")
             && not (empty_string scriptarg2)
             && dpkg_compare_versions_le_nl scriptarg2 lastversion then
            (if_then_else
               (interp_test_d (pathname^".dpkg-backup"))
               (if_then_else
                  (interp_test_h pathname)
                  (if_then_else
                     (symlink_match pathname symlink_target)
                     (abort_dir_to_symlink env pathname)
                     (return true))
                  (if_then_else
                     (interp_test env ~bracket:false
                        ["-d"; pathname;"-a";"-f";pathname^".dpkg-staging-dir"])
                     (abort_dir_to_symlink env pathname)
                     (return true)))
               (return true))
          else return true)
    | _ -> return true

  let interprete (env:env) (args:args) =
    match args with
    | subcmd::restargs ->
       begin
         if subcmd = "supports" then 
           supports env restargs
         else
           try
             let (cmdargs,scriptargs) = split_at_dashdash args in
             let (scriptarg1,scriptarg2) =
               match scriptargs with
               | [x;y] -> (x,y)
               | [x] -> (x,"")
               | _ -> raise MaintainerScriptArguments
             in
             match subcmd with
             | "rm_conffile"->
                rm_conffile env cmdargs scriptarg1 scriptarg2
             | "mv_conffile" ->
                mv_conffile env cmdargs scriptarg1 scriptarg2
             | "symlink_to_dir" ->
                symlink_to_dir env cmdargs scriptarg1 scriptarg2
             | "dir_to_symlink" ->
                dir_to_symlink env cmdargs scriptarg1 scriptarg2
             | _ -> unknown_argument
                      ~msg:"unknown subcommand"
                      ~name:"dpkg_maintscript_helper"
                      ~arg:subcmd
                      ()
           with
           | NoDashDash ->
              unknown_argument
                ~msg:"missing -- separator"
                ~name:("dpkg-maintscript-helper "^subcmd)
                ~arg:"" (* FIXME *)
                ()
           | MaintainerScriptArguments ->
              unknown_argument
                ~msg:"maintainer script arguments are missing"
                ~name:("dpkg-maintscript-helper "^subcmd)
                ~arg: "" (* FIXME *)
                ()
           | Error s ->
              error ~msg:s ()
       end
    | [] -> unknown_argument
              ~msg:"no arguments"
              ~name: "dpkg_maintscript_helper"
              ~arg:"" (* FIXME *)
              ()
end


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
  | "rm" -> interp_rm
  | "update-alternatives" -> interp_update_alternatives
  | "dpkg" -> interp_dpkg
  | "dpkg-maintscript-helper" -> DpkgMaintScriptHelper.interprete
  | "mv" -> Mv.interprete
  | _ -> fun _env _args -> unknown_utility ~name ()
