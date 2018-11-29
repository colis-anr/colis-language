open Format
open Constraints
open Clause
open UtilitiesSpecification
open Semantics__Buffers
open SymbolicInterpreter__State

(** Get the name of the last path component, if any, or of the hint root variable
    otherwise. The result is useful as a hint for creating variables for resolving the
    path. *)
let last_comp_as_hint: root:Var.t -> Path.t -> string option =
  fun ~root path ->
    match Path.split_last path with
    | Some (_, Down f) ->
      Some (Feat.to_string f)
    | None -> (* Empty parent path => root *)
      Some (Var.hint root)
    | Some (_, (Here|Up)) ->
      None (* We can’t know (if last component in parent path is a symbolic link) *)

(** Create a singleton error case with optional message *)
let error ?msg () : state -> (state * bool) list =
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

(*********************************************************************************)
(*                                     true/false                                *)
(*********************************************************************************)

let return result : state -> (state * bool) list =
  fun sta -> [sta, result]

let interp_true =
  fun _args ->
    return true

let interp_false =
  fun _args ->
    return false

(*********************************************************************************)
(*                                        echo                                   *)
(*********************************************************************************)

let interp_echo : utility =
  fun args sta ->
    let open Semantics__Buffers in
    let open SymbolicInterpreter__State in
    let str = String.concat " " args in
    let stdout = Stdout.(output str sta.stdout |> newline) in
    [ {sta with stdout}, true ]

(*********************************************************************************)
(*                                        touch                                  *)
(*********************************************************************************)

let interp_touch1 path_str =
  let p = Path.from_string path_str in
  match Path.split_last p with
  | None -> (* `touch ''` *)
    error ~msg:"cannot touch '': No such file or directory" ()
  | Some (q, comp) ->
    under_specifications @@ fun cwd root root' ->
    let noop_cases =
      let hint = last_comp_as_hint ~root p in [
        { descr = asprintf "touch %a: path resolves" Path.pp p;
          outcome = Success ;
          spec =
            exists ?hint @@ fun y ->
            resolve root cwd p y &
            eq root root' };
        { descr = asprintf "touch %a: parent path does not resolve" Path.pp p;
          outcome = Error;
          spec =
            noresolve root cwd q &
            eq root root' };
      ]
    in
    let create_file_case f =
      let hintx = last_comp_as_hint ~root q in
      let hinty = Feat.to_string f in
      { descr = asprintf "touch %a: create file" Path.pp p;
        outcome = Success;
        spec =
          exists3 ?hint1:hintx ?hint2:hintx ~hint3:hinty @@ fun x x' y' ->
          resolve root cwd q x &
          dir x &
          abs x f &
          similar root root' cwd q x x' &
          sim x (Feat.Set.singleton f) x' &
          dir x' &
          feat x' f y' &
          reg y' }
    in
    match comp with
    | Up | Here ->
      noop_cases
    | Down f ->
      create_file_case f :: noop_cases

let interp_touch : utility =
  function
  | [arg] -> interp_touch1 arg
  | _ ->
    failwith "not yet implemented: interp_touch with multiple arguments"

(*********************************************************************************)
(*                                        mkdir                                  *)
(*********************************************************************************)

let interp_mkdir1 path_str =
  let p = Path.from_string path_str in
  match Path.split_last p with
  | None ->
    error ()
  | Some (q, (Here|Up)) ->
    error ~msg:"mkdir: file exists" () (* CHECK *)
  | Some (q, Down f) ->
    under_specifications @@ fun cwd root root' ->
    let hintx = last_comp_as_hint ~root q in
    let hinty = Feat.to_string f in [
      { descr = asprintf "mkdir %a: create directory" Path.pp p;
        outcome = Success;
        spec =
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
          fen y Feat.Set.empty };
      { descr = asprintf "mkdir %a: target already exists" Path.pp p;
        outcome = Error;
        spec =
          exists ?hint:hintx @@ fun x ->
          resolve root cwd q x &
          dir x &
          nabs x f &
          eq root root' };
      (* The path "q" resolves to a file *)
      { descr = asprintf "mkdir %a: parent path is file" Path.pp p;
        outcome = Error;
        spec =
          exists ?hint:hintx @@ fun x ->
          resolve root cwd q x &
          ndir x &
          eq root root' };
      (* The dir "p" does not exist. *)
      { descr = asprintf "mkdir %a: parent path does not resolve" Path.pp p;
        outcome = Error;
        spec =
          noresolve root cwd q &
          eq root root'};
    ]

let interp_mkdir : utility =
  function
  | [] -> error ~msg:"mkdir: missing operand" ()
  | [arg] -> interp_mkdir1 arg
  | _ -> failwith "not implemented: mkdir with multiple arguments"

(*********************************************************************************)
(*                                        test                                   *)
(*********************************************************************************)

let interp_test_e path_str =
  let p = Path.from_string path_str in
  under_specifications @@ fun cwd root root' ->
  let hintx = last_comp_as_hint ~root p in [
    { descr = asprintf "test -e %a: path resolves" Path.pp p;
      outcome = Success;
      spec =
        exists ?hint:hintx @@ fun x ->
        resolve root cwd p x &
        eq root root' };
    { descr = asprintf "test -e %a: path does not resolve" Path.pp p;
      outcome = Error;
      spec =
        noresolve root cwd p &
        eq root root' };
  ]

let interp_test : utility = function
  | [] -> return false (* CHECK *)
  | ["-e"] -> return true
  | ["-e"; arg] -> interp_test_e arg
  | "-e" :: _ -> error ~msg:"test: too many arguments" ()
  | [_] -> return true (* CHECK *)
  | flag :: _ -> error ~msg:("test: unknown condition: "^flag) ()


(*********************************************************************************)
(*                         Dispatch interpretation of utilities                  *)
(*********************************************************************************)

let interp (name: string) : utility =
  match name with
  | "true" -> interp_true
  | "false" -> interp_false
  | "echo" -> interp_echo
  | "test" -> interp_test
  | "touch" -> interp_touch
  | "mkdir" -> interp_mkdir
  | _ ->
    fun _args ->
      error ~msg:("Unknown utility: "^name) ()
