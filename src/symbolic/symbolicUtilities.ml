open Format
open Constraints
open Clause
open SymbolicUtility
open Semantics__Buffers

module IdMap = Env.IdMap

(******************************************************************************)
(*                                  true/false                                *)
(******************************************************************************)

let interp_true : context -> utility =
  fun _ ->
    return true

let interp_false : context -> utility =
  fun _ ->
    return false

(******************************************************************************)
(*                                     echo                                   *)
(******************************************************************************)

let interp_echo : context -> utility =
  fun ctx sta ->
    let open Semantics__Buffers in
    let open SymbolicInterpreter__Semantics in
    let str = String.concat " " ctx.args in
    let stdout = Stdout.(output str sta.stdout |> newline) in
    [ {sta with stdout}, true ]

(******************************************************************************)
(*                                     touch                                  *)
(******************************************************************************)

let interp_touch1 cwd path_str : utility =
  under_specifications @@ fun ~root ~root' ->
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

let interp_touch ctx : utility =
  match ctx.args with
  | [arg] -> interp_touch1 ctx.cwd arg
  | _ -> unknown_argument ~msg:"multiple arguments"  ~name:"touch" ~arg:"" ()


(******************************************************************************)
(*                                     mkdir                                  *)
(******************************************************************************)

let interp_mkdir1 cwd path_str : utility =
  under_specifications @@ fun ~root ~root' ->
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

let interp_mkdir ctx : utility =
  match ctx.args with
  | [] -> error ~msg:"mkdir: missing operand" ()
  | [arg] -> interp_mkdir1 ctx.cwd arg
  | _ -> unknown_argument ~msg:"multiple arguments" ~name:"mkdir" ~arg:"" ()

(******************************************************************************)
(*                                     test                                   *)
(******************************************************************************)

let interp_test_parse_error args : utility =
  under_specifications @@ fun ~root ~root' ->
  [
    let descr = "test: parse error in `" ^ (String.concat " " args) ^ "`" in
    error_case
      ~descr
      begin
        eq root root'
      end;
  ]

let interp_test_empty () : utility =
  under_specifications @@ fun ~root ~root' ->
  [
    let descr = "test: empty expression" in
    error_case
      ~descr
      begin
        eq root root'
      end;
  ]

let interp_test_e cwd path_str : utility =
  under_specifications @@ fun ~root ~root' ->
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

let interp_test_d cwd path_str : utility =
  under_specifications @@ fun ~root ~root' ->
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

let interp_test_f cwd path_str : utility =
  under_specifications @@ fun ~root ~root' ->
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

let interp_test_attribute ~attr cwd path_str : utility =
  under_specifications @@ fun ~root ~root' ->
  let p = Path.from_string path_str in
  let hintx = last_comp_as_hint ~root p in [
    success_case
      ~descr:(asprintf "test '%a': path resolves, attribute -%s OK (overapprox to -e)"
                Path.pp p attr)
      begin
        exists ?hint:hintx @@ fun x ->
        resolve root cwd p x &
        eq root root'
      end;
    error_case
      ~descr:(asprintf "test '%a': path does not resolve" Path.pp p)
      begin
        noresolve root cwd p &
        eq root root'
      end;
    error_case
      ~descr:(asprintf "test '%a': path resolves, attribute -%s not OK (overapprox to -e)"
                Path.pp p attr)
      begin
        exists ?hint:hintx @@ fun x ->
        resolve root cwd p x &
        eq root root'
      end;
  ]

let interp_test_n str : utility =
  under_specifications @@ fun ~root ~root' ->
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
  under_specifications @@ fun ~root ~root' ->
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

let interp_test_h _cwd _path_str : utility =
  assert false (* FIXME : we need test -h *)

let interp_test_string_equal s1 s2 : utility =
  under_specifications @@ fun ~root ~root' ->
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
  under_specifications @@ fun ~root ~root' ->
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

let rec interp_test_expr cwd e : utility =
  let name = "test" in
  let msg what = "unsupported " ^ what in
  Morsmall_utilities.TestParser.(
  match e with
  | Unary("-e",arg) -> interp_test_e cwd arg
  | Unary("-d",arg) -> interp_test_d cwd arg
  | Unary("-f",arg) -> interp_test_f cwd arg
  | Unary("-h",arg) -> interp_test_h cwd arg
  | Unary("-G",arg) -> interp_test_attribute ~attr:"G" cwd arg
  | Unary("-O",arg) -> interp_test_attribute ~attr:"O" cwd arg
  | Unary("-g",arg) -> interp_test_attribute ~attr:"g" cwd arg
  | Unary("-k",arg) -> interp_test_attribute ~attr:"k" cwd arg
  | Unary("-r",arg) -> interp_test_attribute ~attr:"r" cwd arg
  | Unary("-s",arg) -> interp_test_attribute ~attr:"s" cwd arg
  | Unary("-u",arg) -> interp_test_attribute ~attr:"u" cwd arg
  | Unary("-w",arg) -> interp_test_attribute ~attr:"w" cwd arg
  | Unary("-x",arg) -> interp_test_attribute ~attr:"x" cwd arg
  | Unary("-n",arg) -> interp_test_n arg
  | Unary("-z",arg) -> interp_test_z arg
  | Binary ("=",a1,a2) -> interp_test_string_equal a1 a2
  | Binary ("!=",a1,a2) -> interp_test_string_notequal a1 a2
  | Unary(op,_) ->
     let msg = msg "unary operator" in
     unknown_argument ~msg ~name ~arg:op ()
  | And(e1,e2) ->
     interp_test_and (interp_test_expr cwd e1) (interp_test_expr cwd e2)
  | Or(e1,e2) ->
     interp_test_or (interp_test_expr cwd e1) (interp_test_expr cwd e2)
  | Not(e1) -> interp_test_neg (interp_test_expr cwd e1)
  | Binary (op,_e1,_e2) ->
     let msg = msg "binary operator" in
     unknown_argument ~msg ~name ~arg:op ()
  | Single arg ->
     let msg = msg "single argument" in
     unknown_argument ~msg ~name ~arg ()
  )

let interp_test ~bracket ctx : utility =
  match Morsmall_utilities.TestParser.parse ~bracket ctx.args with
  | None -> interp_test_empty ()
  | Some e -> interp_test_expr ctx.cwd e
  | exception Morsmall_utilities.TestParser.Parse_error ->
     interp_test_parse_error ctx.args

(******************************************************************************)
(*                                rm                                          *)
(******************************************************************************)
let interp_rm1 cwd arg : utility =
  under_specifications @@ fun ~root ~root' ->
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

let interp_rm1_r cwd arg : utility =
  under_specifications @@ fun ~root ~root' ->
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

let interp_rm_r cwd args : utility =
  match args with
  | [] -> error ~msg:"rm: missing operand" ()
  | [arg] -> interp_rm1_r cwd arg
  | _ -> unknown_argument ~msg:"multiple arguments" ~name:"rm -r/R" ~arg:"" ()

let interp_rm ctx : utility =
  match ctx.args with
  | [] -> error ~msg:"rm: missing operand" ()
  | ("-r" | "-R") :: args -> interp_rm_r ctx.cwd args
  | [arg] -> interp_rm1 ctx.cwd arg
  | _ -> unknown_argument ~msg:"multiple arguments" ~name:"rm" ~arg:"" ()




(******************************************************************************)
(*                                which                                       *)
(******************************************************************************)

let _interp_which_naive ctx : utility =
  match ctx.args with
  | [] ->
     under_specifications @@ fun ~root ~root' ->
       [
         error_case
           ~descr:(asprintf "which without argument (returns 1)")
           begin
             eq root root'
           end
       ]
  | [p] ->
     under_specifications @@ fun ~root ~root' ->
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

let interp_test_regular_and_x cwd path_str : utility =
  under_specifications @@ fun ~root ~root' ->
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


let rec search_as_which_in_path cwd (path:string list) arg : utility =
  match path with
  | [] ->
     under_specifications @@ fun ~root ~root' ->
       [
         error_case
           ~descr:(asprintf "which: `%s` not found in PATH" arg)
           begin
             eq root root'
           end
       ]
  | p :: rem ->
     let u1 = interp_test_regular_and_x cwd (p ^ "/" ^ arg) in
     let u2 = search_as_which_in_path cwd rem arg in
     fun st ->
     List.flatten
       (List.map
          (function (s1,b1) as x -> if b1 then [x] else u2 s1)
          (u1 st))

let search_as_which cwd (path:string list) arg : utility =
  match Path.from_string arg with
  | Abs _ -> interp_test_regular_and_x cwd arg
  | Rel [] -> assert false
  | Rel [_] -> search_as_which_in_path cwd path arg
  | Rel r ->
     fun st ->
     let a = Path.concat cwd (Rel r) in
     interp_test_regular_and_x cwd ("/" ^ Path.rel_to_string a) st

let interp_which_full ctx : utility =
  match ctx.args with
  | [] ->
     under_specifications @@ fun ~root ~root' ->
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
    (* FIXME     let path = String.split_on_char ':' (IdMap.find "PATH" ctx.env) in *)
     let path = [ "/usr/sbin" ; "/usr/bin" ; "/sbin" ; "/bin" (* ; "/usr/games" *) ] in
     let rec aux args =
       match args with
       | [] -> assert false
       | [a] -> search_as_which ctx.cwd path a
       | a :: rem ->
          interp_test_and (search_as_which ctx.cwd path a) (aux rem)
     in
     aux ctx.args

(**************************************************************************)
(*                     update-alternatives                                *)
(**************************************************************************)

let interp_update_alternatives ctx =
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
  aux ctx.args

(**************************************************************************)
(*                                    dpkg                                *)
(**************************************************************************)

let interp_dpkg ctx =
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
  aux ctx.args


let register () =
  (* These calls can be moved to the modules that implement the utilities *)
  let register' (name, f) = register (module struct let name = name let interprete = f end) in
  List.iter register' [
    "true", interp_true;
    "false", interp_false;
    "echo", interp_echo;
    "test", interp_test ~bracket:false;
    "[", interp_test ~bracket:true;
    "touch", interp_touch;
    "mkdir", interp_mkdir;
    "which", interp_which_full;
    "rm", interp_rm;
    "update-alternatives", interp_update_alternatives;
    "dpkg", interp_dpkg;
  ];
  List.iter register [
      (module Mv);
      (module DpkgMaintscriptHelper)
  ]
