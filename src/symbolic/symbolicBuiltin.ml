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
    let disj' =
      disj >>=
      dir x >>=
      Clause.feat x f y
    in
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

let error str sta =
  print_err str {sta with result = false}

let interp_echo sta args =
  let str = String.concat " " args in
  [print_line str {sta with result = true}]

let with_clause sta clause =
  let filesystem = {sta.filesystem with clause} in
  [{sta with filesystem}]

let last_path_component path =
  match List.rev (String.split_on_char '/' path) with
  | f :: p -> String.join "/" (List.rev p), f
  | _ -> failwith "last_path_component"

(* Symbolic interpretation of command touch *)
let interp_touch sta args =
  match args with
  | [path] -> begin
      let p, s = last_path_component path in (* [path = p/f] *)
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
    end
  | _ ->
    [error "touch: not exactly one argument" sta]

let interp_mkdir sta args =
  match args with
  | [path] ->
    let p, s = last_path_component path in (* [path = p/f] *)
    let f = Feature.of_string s in
    let oks = (* p/f can be created *)
      let for_x disj x = (* p resolves to x *)
        exists ~hint:s @@ fun x' -> (* x' will be similar to x  *)
        exists ~hint:s @@ fun y' -> (* y' will be the new child of x' *)
        disj >>=
        dir x >>=
        Clause.abs x f >>= (* p/f does not yet exist *)
        Clause.sim x (Feature.Set.singleton f) x' >>=
        Clause.feat x' f y' >>=
        dir y' >>=
        Clause.fen y' Feature.Set.empty
      in
      resolve sta.filesystem path for_x >>=
      with_clause {sta with result = true}
    in
    let errs1 = (* p/f exists *)
      let for_x disj x =
        disj >>=
        Clause.nabs x f
      in
      resolve sta.filesystem path for_x >>=
      with_clause {sta with result = false}
    in
    let errs2 = (* p does not resolve *)
      noresolve sta.filesystem path >>=
      with_clause {sta with result = false}
    in
    oks @ errs1 @ errs2
  | _ ->
    [error "mkdir: not exactly one argument" sta]

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
  | _ -> [error "test-e: Not exactly one argument" sta]

let interp (sta: state) (cmd: string) (args:string list) : state list =
  match cmd with
  | "true" -> [{sta with result = true}]
  | "false" -> [{sta with result = false}]
  | "echo" -> interp_echo sta args
  | "test-e" -> interp_test_e sta args
  | "touch" -> interp_touch sta args
  | "mkdir" -> interp_mkdir sta args
  | _ ->
    [error ("Unknown builtin: "^cmd) sta]
