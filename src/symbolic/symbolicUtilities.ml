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
(*                                rm                                          *)
(******************************************************************************)
let interp_rm1 cwd arg : utility =
  under_specifications @@ fun ~root ~root' ->
  let oq = Path.from_string arg in
  match Path.split_last oq with
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
           ~descr:(asprintf "rm %a: remove file" Path.pp oq)
           begin
             exists2 ?hint1:hintx ?hint2:hintx @@ fun x x' ->
             exists ~hint:hinty @@ fun y ->
             resolve root cwd q y & ndir y
             & similar root root' cwd q x x'
             & sim x (Feat.Set.singleton f) x'
             & dir x' & fen x' (Feat.Set.singleton f)
           end;
         error_case
           ~descr:(asprintf "rm %a: target is a directory" Path.pp oq)
           begin
             exists ~hint:hinty @@ fun y ->
             resolve root cwd q y & dir y
             & eq root root'
           end;
         error_case
           ~descr:(asprintf "rm %a: target does not exist" Path.pp oq)
           begin
             exists ~hint:hinty @@ fun y ->
             noresolve root cwd q & eq root root'
           end;
       ]

let interp_rm1_r cwd arg : utility =
  under_specifications @@ fun ~root ~root' ->
  let oq = Path.from_string arg in
  match Path.split_last oq with
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
           ~descr:(asprintf "rm -r %a: remove file or directory" Path.pp oq)
           begin
             exists2 ?hint1:hintx ?hint2:hintx @@ fun x x' ->
             exists ~hint:hinty @@ fun y ->
             resolve root cwd q y & ndir y
             & similar root root' cwd q x x'
             & sim x (Feat.Set.singleton f) x'
             & dir x' & fen x' (Feat.Set.singleton f)
           end;
         error_case
           ~descr:(asprintf "rm -r %a: target does not exist" Path.pp oq)
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

let interp_silent_which ctx : utility =
  match ctx.args with
  | [] ->
     under_specifications @@ fun ~root ~root' ->
       [
         error_case
           ~descr:(asprintf "silent-which without argument (returns 1)")
           begin
             eq root root'
           end
       ]
  | [p] ->
     under_specifications @@ fun ~root ~root' ->
       [
         success_case
           ~descr:(asprintf "silent-which '%s': assuming command is found" p)
           begin
             eq root root'
           end
       ;
         error_case
           ~descr:(asprintf "silent-which '%s': assuming command is not found" p)
           begin
             eq root root'
           end
       ]
  | p :: _ ->
     unknown_argument ~msg:"more than one argument" ~name:"silent-which" ~arg:p ()

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
          uand (search_as_which ctx.cwd path a) (aux rem)
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

(* Symbolic utilties are now registered in module Colis.Symbolic (file src/colis.ml) --
   this file will ideally disappear. *)
