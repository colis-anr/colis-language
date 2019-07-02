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
  let str = String.concat " " ctx.args in
  let sta = print_stdout ~newline:true str sta in
  [sta, true]

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
  | _ -> unsupported ~utility:"touch" "multiple arguments"


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
     unsupported ~utility:"silent-which" "more than one argument"

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
     unsupported ~utility:"which" "option `-a`"
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
      unsupported ~utility:name "no sub-command found"
    | "--quiet" :: rem->
      fun st -> aux rem (print_utility_trace (name ^ ": ignored option --quiet") st)
    | arg :: _ ->
      unsupported ~utility:name ("unsupported argument: " ^ arg)
  in
  aux ctx.args

(**************************************************************************)
(*                                    dpkg                                *)
(**************************************************************************)

let interp_dpkg ctx =
  let utility = "dpkg" in
  let aux = function
    | ["-L"; _pkg_name] ->
      unsupported ~utility "support for -L not yet implemented"
    | "-L" :: _ ->
      unsupported ~utility "option -L expects exactly one argument"
    | ["--compare-versions"; _v1; _v2] ->
      unsupported ~utility "support for --compare-versions not yet implemented"
    | "--compare-versions" :: _ ->
      unsupported ~utility "option --compare-versions expects exactly two arguments"
    | [] ->
      (* TODO: return error state *)
      unsupported ~utility "no argument found"
    | arg :: _ ->
      unsupported ~utility ("unsupported argument: " ^ arg)
  in
  aux ctx.args


let register () =
  (* These calls can be moved to the modules that implement the utilities *)
  let register' (name, f) = register (module struct let name = name let interprete = f end) in
  List.iter register' [
    "true", interp_true;
    "false", interp_false;
    "echo", interp_echo;
    "test", Test_utility.interpret ~bracket:false;
    "[", Test_utility.interpret ~bracket:true;
    "touch", interp_touch;
    "which", interp_which_full;
    "silent-which", interp_silent_which;
    "rm", Rm.interp;
    "update-alternatives", interp_update_alternatives;
    "dpkg", interp_dpkg;
  ];
  List.iter register [
      (module Cp);
      (module Mv);
      (module Mkdir);
      (module DpkgMaintscriptHelper)
  ]
