open Format
open Constraints
open Clause
open UtilitiesSpecification
open Semantics__Buffers
open SymbolicInterpreter__State



type args = string list

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
    raise (Errors.UnsupportedUtility name)
  else
    error ~msg:(msg ^ ": " ^ name) ()

(** Wrapper around [error] in case of unknown argument. *)
let unknown_argument ?(msg="Unknown argument") ~name ~arg () =
  if !Options.fail_on_unknown_utilities then
    raise (Errors.UnsupportedArgument (name, arg))
  else
    error ~msg:(msg ^ ": " ^ arg) ()

(*********************************************************************************)
(*                                     true/false                                *)
(*********************************************************************************)

let return result : utility =
  fun sta -> [sta, result]

let interp_true: args -> utility =
  fun _ ->
    return true

let interp_false: args -> utility =
  fun _ ->
    return false

(*********************************************************************************)
(*                                        echo                                   *)
(*********************************************************************************)

let interp_echo: args -> utility =
  fun args sta ->
    let open Semantics__Buffers in
    let open SymbolicInterpreter__State in
    let str = String.concat " " args in
    let stdout = Stdout.(output str sta.stdout |> newline) in
    [ {sta with stdout}, true ]

(*********************************************************************************)
(*                                        touch                                  *)
(*********************************************************************************)

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

let interp_touch: args -> utility =
  function
  | [arg] -> interp_touch1 arg
  | _ ->
    failwith "not yet implemented: interp_touch with multiple arguments"

(*********************************************************************************)
(*                                        mkdir                                  *)
(*********************************************************************************)

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

let interp_mkdir: args -> utility =
  function
  | [] -> error ~msg:"mkdir: missing operand" ()
  | [arg] -> interp_mkdir1 arg
  | _ -> failwith "not implemented: mkdir with multiple arguments"

(*********************************************************************************)
(*                                        test                                   *)
(*********************************************************************************)

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

let interp_test ~bracket (args : string list) : utility =
  Morsmall_utilities.TestParser.(
    let name = "test" in
    let msg what =
      "unsupported " ^ what ^ " in `" ^ (String.concat " " args) ^"`"
    in
    match parse ~bracket args with
    | None -> interp_test_empty ()
    | Some e ->
       begin
       match e with
       | Unary("-e",arg) -> interp_test_e arg
       | Unary("-d",arg) -> interp_test_d arg
       | Unary("-f",arg) -> interp_test_f arg
       | Unary(op,_) ->
          let msg = msg "unary operator" in
          unknown_argument ~msg ~name ~arg:op ()
       | And(_e1,_e2) ->
          let msg = msg "conjunction operator" in
          unknown_argument ~msg ~name ~arg:"-a" ()
       | Or(_e1,_e2) ->
          let msg = msg "disjunction operator" in
          unknown_argument ~msg ~name ~arg:"-o" ()
       | Not(_e1) ->
          let msg = msg "negation operator" in
          unknown_argument ~msg ~name ~arg:"!" ()
       | Binary (op,_e1,_e2) ->
          let msg = msg "binary operator" in
          unknown_argument ~msg ~name ~arg:op ()
       | Single arg ->
          let msg = msg "single argument" in
          unknown_argument ~msg ~name ~arg ()
       end
    | exception Parse_error ->
       interp_test_parse_error args
  )


(*********************************************************************************)
(*                         Dispatch interpretation of utilities                  *)
(*********************************************************************************)

let interp (name: string) : args -> utility =
  match name with
  | "true" -> interp_true
  | "false" -> interp_false
  | "echo" -> interp_echo
  | "test" -> interp_test ~bracket:false
  | "[" -> interp_test ~bracket:true
  | "touch" -> interp_touch
  | "mkdir" -> interp_mkdir
  | _ -> fun _args -> unknown_utility ~name ()
