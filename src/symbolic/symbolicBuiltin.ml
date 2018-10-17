open Batteries
open Semantics__Buffers
open SymbolicInterpreter__Definitions

let (>>=) disj f = List.(map f disj |> flatten)

let exists_d : ?hint:string -> (Variable.t -> (bool * Clause.t) list) -> (bool * Clause.t) list =
  fun ?hint k ->
    let v = Variable.fresh ?hint () in
    k v >>= fun (b, d) ->
    [b, Clause.exists [v] d]

let as_success c = [true, c]
let as_failure c = [false, c]

let with_filesystem_clauses sta f =
  f sta.filesystem >>= fun (b, clause) ->
  let filesystem = {sta.filesystem with clause} in
  [{sta with filesystem; result = b}]

let dir : Variable.t -> Clause.t -> Clause.disj = assert false
let reg : Variable.t -> Clause.t -> Clause.disj = assert false

(** [resolve st p] resolves path [p] (as string) in state [st], corresponding to
    {%resolve(\Sigma,x,np,p)%} in the document {em Specification of UNIX commands} **)
let resolve path k fs =
  let rec aux disj x xs path errs k =
    match path with
    | [] -> (* done *)
      k x disj (xs, errs)
    | "." :: path'
    | "" :: path'-> (* ignore dots in path *)
      aux disj x xs path' errs k
    | ".." :: path' ->
      let x, xs =
        match xs with
        | x :: xs -> x, xs
        | [] -> x, xs
      in
      aux disj x xs path' errs k
    | s :: path' -> (* down *)
      exists_d ~hint:s @@ fun y ->
      let f = Feature.of_string s in
      let disj' =
        disj >>= dir x >>= Clause.feat x f y
      in
      let errs' =
        (disj >>= reg x) @
        (disj >>= dir x >>= Clause.abs x f)
      in
      aux disj' y (x::xs) path' (errs' @ errs : Clause.t list) k
  in
  match String.split_on_char '/' path with
  | "" :: path -> (* absolute path *)
    aux [fs.clause] fs.root [] path [] k
  | path -> (* relative path *)
    let cwd = List.map Feature.to_string fs.cwd in
    aux [fs.clause] fs.root [] cwd [] @@ fun x disj (xs, errs) ->
    aux disj x xs path [] @@ fun y disj (ys,  errs') ->
    k y disj (ys, errs' @ errs)

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
        let f = Feature.of_string s in
        with_filesystem_clauses sta @@
        resolve p @@ fun x disj (_, (errs : Clause.t list)) ->
        let st1_ok : (bool * Clause.t) list = (* p/f exists, nop *)
          disj >>=
          Clause.nabs x f >>=
          as_success
        in
        let st2_ok : (bool * Clause.t) list = (* p exists, p/f doesn't *)
          exists_d @@ fun x' ->
          exists_d @@ fun y ->
          disj >>=
          dir x >>= Clause.abs x f >>=
          Clause.sim x (Feature.Set.singleton f) x' >>= fun cls ->
          Clause.feat x' f y cls >>=
          as_success
        in
        let sts_err : (bool * Clause.t) list =
          (* p does not exist *)
          errs >>= fun cls -> [false, cls]
        in
        st1_ok @ st2_ok @ sts_err
      | None ->
        [print_err "touch: empty path" {sta with result = false}]
    end
  | _ -> [print_err "touch: not exactly one argument" {sta with result = false}]


let interp_test_e sta args =
  match args with
  | [path] ->
    with_filesystem_clauses sta @@
    resolve path @@ fun x disj (_, errs) ->
    let sts_ok = disj >>= as_success in
    let sts_err = errs >>= as_failure in
    sts_ok @ sts_err
  | _ -> [print_err "test-e: Not exactly one argument" {sta with result = false}]

let interp (sta: state) (cmd: string) (args:string list) : state list =
  match cmd with
  | "echo" -> interp_echo sta args
  | "test-e" -> interp_test_e sta args
  | _ -> [print_err ("Unknown builtin: "^cmd) {sta with result = false}]
