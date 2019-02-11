open Format
open Constraints
open Clause
open UtilitiesSpecification
open SymbolicInterpreter__State

type env = (string * string) list
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

(** Wrapper around [error] in case of unknown utility. *)
let unknown_utility name =
  under_specifications @@ fun ~cwd:_cwd ~root ~root' -> [
    let descr = sprintf "Utility %s not found" name in
    error_case ~descr Clause.(eq root root')
  ]

(** Wrapper around [error] in case of unknown argument. *)
let unknown_argument name arg =
  under_specifications @@ fun ~cwd:_cwd ~root ~root' -> [
    let descr = sprintf "Invalid argument for utility %s: %s" name arg in
    error_case ~descr Clause.(eq root root')
  ]

(** Wrapper around [error] in case of known but unimplemented utility. *)
let unimplemented_utility name =
  under_specifications @@ fun ~cwd:_cwd ~root ~root' -> [
    let descr = sprintf "Utility %s not implemented" name in
    failure_case ~descr Clause.(eq root root')
  ]

(** Wrapper around [error] in case of known but unimplemented argument. *)
let unimplemented_argument name arg =
  under_specifications @@ fun ~cwd:_cwd ~root ~root' -> [
    let descr = sprintf "Argument for utility %s not implemented: %s" name arg in
    failure_case ~descr Clause.(eq root root')
  ]

let error_utility ~descr ?stdout () =
  under_specifications @@ fun ~cwd:_cwd ~root ~root' -> [
    error_case ~descr ?stdout Clause.(eq root root')
  ]

(*********************************************************************************)
(*                                     true/false                                *)
(*********************************************************************************)

let return result : utility =
  fun sta -> [sta, result]

let interp_true : env -> args -> utility =
  fun _ _ ->
    return (Result true)

let interp_false : env -> args -> utility =
  fun _ _ ->
    return (Result false)

(*********************************************************************************)
(*                                        echo                                   *)
(*********************************************************************************)

let interp_echo : env -> args -> utility =
  fun _ args sta ->
    let open Semantics__Buffers in
    let open SymbolicInterpreter__State in
    let str = String.concat " " args in
    let stdout = Stdout.(output str sta.stdout |> newline) in
    [ {sta with stdout}, Result true ]

(*********************************************************************************)
(*                                        touch                                  *)
(*********************************************************************************)

let interp_touch1 path_str : utility =
  under_specifications @@ fun ~cwd ~root ~root' ->
  let p = Path.from_string path_str in
  match Path.split_last p with
  | None -> (* `touch ''` *)
    [error_case ~descr:"cannot touch '': No such file or directory" Clause.(eq root root')]
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
  | _ -> unimplemented_utility "touch with multiple arguments"

(*********************************************************************************)
(*                                        mkdir                                  *)
(*********************************************************************************)

let interp_mkdir1 path_str : utility =
  under_specifications @@ fun ~cwd ~root ~root' ->
  let p = Path.from_string path_str in
  match Path.split_last p with
  | None ->
    [error_case ~descr:"mkdir: cannot create directory ''" Clause.(eq root root')]
  | Some (_q, (Here|Up)) ->
    [error_case ~descr:"mkdir: file exists" Clause.(eq root root') (* CHECK *)]
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
  | [] -> error_utility ~descr:"mkdir: missing operand" ()
  | [arg] -> interp_mkdir1 arg
  | _ -> unimplemented_utility "mkdir with multiple arguments"

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

let interp_test_x path_str : utility =
  under_specifications @@ fun ~cwd ~root ~root' ->
  let p = Path.from_string path_str in
  let hintx = last_comp_as_hint ~root p in [
    success_case
      ~descr:(asprintf "test -x %a: path resolves to an executable (overapprox to -e)" Path.pp p)
      begin
        exists ?hint:hintx @@ fun x ->
        resolve root cwd p x & (* no way to constraint "x" mode *)
        eq root root'
      end;
    error_case
      ~descr:(asprintf "test -x %a: path does not resolve" Path.pp p)
      begin
        noresolve root cwd p &
        eq root root'
      end;
    error_case
      ~descr:(asprintf "test -x %a: path resolves but not to an executable (overapprox to -e)" Path.pp p)
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
  List.map
    (function
      | (s, Result b) -> (s, Result (not b))
      | (s,Incomplete) -> (s,Incomplete))
    (u st)

let interp_test_and (u1:utility) (u2:utility) : utility = fun st ->
  List.flatten
    (List.map
       (function
         | (s1,Result b1) ->
           List.map
             (function
               | (s2, Result b2) -> s2, Result (b1 && b2)
               | (s2, Incomplete) -> s2, Incomplete)
             (u2 s1)
         | (s1, Incomplete) -> [s1, Incomplete])
       (u1 st))

let interp_test_or (u1:utility) (u2:utility) : utility = fun st ->
  List.flatten
    (List.map
       (function
         | (s1, Result b1) ->
           List.map
             (function
               | (s2, Result b2) -> s2, Result (b1 || b2)
               | (s2, Incomplete) -> s2, Incomplete)
             (u2 s1)
         | (s1, Incomplete) -> [s1, Incomplete])
       (u1 st))

let test_known_unary_operators = ["-b"; "-c"; "-d"; "-e"; "-f"; "-g"; "-h"; "-L"; "-n"; "-p"; "-r"; "-S"; "-s"; "-t"; "-u"; "-w"; "-x"; "-z"]
let test_known_binary_operators = ["=="; "!="; "-e"; "-n"; "-g"; "-g"; "-l"; "-l"]

let rec interp_test_expr e : utility =
  let name = "test" in
  Morsmall_utilities.TestParser.(
  match e with
  | And(e1,e2) ->
    interp_test_and (interp_test_expr e1) (interp_test_expr e2)
  | Or(e1,e2) ->
    interp_test_or (interp_test_expr e1) (interp_test_expr e2)
  | Not(e1) -> interp_test_neg (interp_test_expr e1)
  | Unary("-e",arg) -> interp_test_e arg
  | Unary("-d",arg) -> interp_test_d arg
  | Unary("-f",arg) -> interp_test_f arg
  | Unary("-x",arg) -> interp_test_x arg
  | Unary("-n",arg) -> interp_test_n arg
  | Unary("-z",arg) -> interp_test_z arg
  | Unary(op, _) ->
    if List.mem op test_known_unary_operators then
      unimplemented_argument "test (unary operator)" op
    else
      unknown_argument "test (unary operator)" op
  | Binary ("=",a1,a2) -> interp_test_string_equal a1 a2
  | Binary ("!=",a1,a2) -> interp_test_string_notequal a1 a2
  | Binary (op,_e1,_e2) ->
    if  List.mem op test_known_binary_operators then
      unimplemented_argument name op
    else
      unknown_argument name op
  | Single arg ->
     unimplemented_argument "test (single argument)" ""
  )

let interp_test ~bracket _env (args : string list) : utility =
  match Morsmall_utilities.TestParser.parse ~bracket args with
  | None -> interp_test_empty ()
  | Some e -> interp_test_expr e
  | exception Morsmall_utilities.TestParser.Parse_error ->
     interp_test_parse_error args

(*********************************************************************************)
(*                         Dispatch interpretation of utilities                  *)
(*********************************************************************************)

let interp (name: string) : env -> args -> utility =
  match name with
  | "true" -> interp_true
  | "false" -> interp_false
  | "echo" -> interp_echo
  | "test" -> interp_test ~bracket:false
  | "[" -> interp_test ~bracket:true
  | "touch" -> interp_touch
  | "mkdir" -> interp_mkdir
  | _ ->
    fun _ _ ->
      if List.mem name Utilities.known_utilities then
        unimplemented_utility name
      else
        unknown_utility name

