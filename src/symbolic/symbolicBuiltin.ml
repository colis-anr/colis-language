open Batteries
open Semantics__Buffers
open SymbolicInterpreter__Definitions

let (>>=) disj f = List.(map f disj |> flatten)

let dir : Variable.t -> Clause.t -> Clause.disj = assert false
let reg : Variable.t -> Clause.t -> Clause.disj = assert false

let exists : ?hint:string -> (Variable.t -> Clause.disj) -> Clause.disj =
  fun ?hint k ->
    let v = Variable.fresh ?hint () in
    k v |> List.map (Clause.exists [v])

let rec resolve' disj x xs path k =
  match path with
  | [] -> k disj x xs
  | s :: path' ->
    let f = Feature.of_string s in
    exists ~hint:s @@ fun y ->
    let disj' = disj >>= dir x >>= Clause.feat x f y in
    resolve' disj' y (x::xs) path' k

let resolve : filesystem -> string -> (Clause.disj -> Variable.t -> Clause.disj) -> Clause.disj =
  fun fs path k ->
    match String.split_on_char '/' path with
    | "" :: path ->
      (* an absolute path *)
      resolve' [fs.clause] fs.root [] path (fun disj x _ -> k disj x)
    | path ->
      (* a relative path *)
      let cwd = List.map Feature.to_string fs.cwd in
      resolve' [fs.clause] fs.root [] cwd @@ fun disj x xs ->
      resolve' disj x xs path @@ fun disj y _ ->
      k disj y

let rec noresolve' disj x xs path =
  match path with
  | [] -> []
  | s :: path' ->
    let f = Feature.of_string s in
    (disj >>= reg x) @
    (disj >>= dir x >>= Clause.abs x f) @
    exists ~hint:s @@ fun y ->
    noresolve' (disj >>= Clause.feat x f y) y (x::xs) path'

let noresolve : filesystem -> string -> Clause.disj =
  fun fs path ->
    match String.split_on_char '/' path with
    | "" :: path ->
      (* an absolute path *)
      noresolve' [fs.clause] fs.root [] path
    | path ->
      (* a relative path *)
      let cwd = List.map Feature.to_string fs.cwd in
      resolve' [fs.clause] fs.root [] cwd @@ fun disj x xs ->
      noresolve' disj x xs path

let print_line str sta =
  let stdout = output str sta.stdout |> newline in
  {sta with stdout}

let print_err str sta =
  print_line ("[ERR] "^str) sta

let print_dbg str sta =
  print_line ("[DBG] "^str) sta

let interp_echo sta args =
  let str = String.concat " " args in
  [print_line str {sta with result = true}]

let with_clause sta clause =
  let filesystem = {sta.filesystem with clause} in
  [{sta with filesystem}]

let last_path_component path =
  match List.rev (String.split_on_char '/' path) with
  | f :: p -> Some (String.join "/" (List.rev p), f)
  | _ -> None

(* Symbolic interpretation of command touch *)
let interp_touch sta args =
  match args with
  | [path] -> begin
      match last_path_component path with
      | Some (p, s) -> (* [path = p/f] *)
        let oks =
          let f = Feature.of_string s in
          let for_x disj x =
            let st1_ok = (* p/f exists, nop *)
              disj >>=
              Clause.nabs x f
            in
            let st2_ok = (* p exists, p/f doesn't *)
              exists @@ fun x' ->
              exists @@ fun y' ->
              disj >>=
              dir x >>=
              Clause.abs x f >>=
              Clause.sim x (Feature.Set.singleton f) x' >>=
              Clause.feat x' f y'
            in
            st1_ok @ st2_ok
          in
          resolve sta.filesystem p for_x >>=
          with_clause {sta with result = true}
        in
        let errs =
          noresolve sta.filesystem p >>=
          with_clause {sta with result = false}
        in
        oks @ errs
      | None ->
        [print_err "touch: empty path" {sta with result = false}]
    end
  | _ -> [print_err "touch: not exactly one argument" {sta with result = false}]


let interp_test_e sta args =
  match args with
  | [path] ->
    let oks =
      resolve sta.filesystem path (fun d _ -> d) >>=
      with_clause {sta with result = true}
    in
    let fails =
      noresolve sta.filesystem path >>=
      with_clause {sta with result = true}
    in
    oks @ fails
  | _ -> [print_err "test-e: Not exactly one argument" {sta with result = false}]

let interp (sta: state) (cmd: string) (args:string list) : state list =
  match cmd with
  | "echo" -> interp_echo sta args
  | "test-e" -> interp_test_e sta args
  | _ -> [print_err ("Unknown builtin: "^cmd) {sta with result = false}]
