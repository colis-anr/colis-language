open Batteries
open Format
open Symbexec__Definitions
open Semantics__Buffers
open Symbexec_printers

let (&) = Set.add

(** Collect the set of variables in a constraint *)
let constraint_vars = function
  | Bottom -> Set.empty
  | Eq (x, y)
  | Feature (x, _, y)
  | Similar (x, _, y) -> x & Set.singleton y
  | Present (x, _) | Absent (x, _)
  | Dir x | Reg x | Fence (x, _) ->
    Set.singleton x

(** Collect the set of variables in a constraint set *)
let constraints_vars cs : var Set.t =
  Set.(fold union (map constraint_vars cs) empty)

(** Collect the set of variables in a symbolic state *)
let state_vars st : var Set.t =
  constraints_vars st.filesystem.constraints

let rec fresh_var prefix n vars =
  let v =
    let name = Printf.sprintf "%s-%d" prefix n in
    V name
  in
  if Set.mem v vars
  then fresh_var prefix (succ n) vars
  else v

let fresh_var_prefix prefix vars =
  fresh_var prefix 0 vars

(** Create a fresh variable with given [prefix] that is not in [vars] **)
let fresh_var_feature (F prefix) vars =
  fresh_var prefix 0 vars

(** Create a fresh variant of a variable *)
let fresh_var_like (V var) vars =
  let prefix, n =
    try
      let prefix, n_str = String.split var ~by:"-" in
      prefix, int_of_string n_str
    with Not_found ->
      var, 0
  in
  fresh_var prefix n vars

(** Reverse a path.

    If [p] goes from the target to root, then the result goes from root
    to the traget, and vice verse.*)
let reverse_path p =
  List.map (fun (vf, fv) -> fv, vf) (List.rev p)

let print_down_path fmt p =
  let print_pair fmt (f, v) =
    fprintf fmt "[%a]%a" print_feature f print_var v
  in
  pp_print_list ~pp_sep:(pp_sep "") print_pair fmt p

(** [resolve st p] resolves path [p] (as string) in state [st], corresponding to
    {%resolve(\Sigma,x,np,p)%} in the document {em Specification of UNIX commands} **)
let resolve fs p : (constraints * path) * (path * constraints) list =
  (* [np] goes from current node to root *)
  let rec aux cs root np p =
    match p with
    | [] -> (* done *)
      (cs, np), []
    | "." :: p' (* ignore dots in path *)
    | "" :: p' -> (* ignore empty path components that result from double slashes *)
      aux cs root np p'
    | ".." :: p' -> (* up *)
      begin match np with
        | [] -> (* at root, don't move *)
          aux cs root np p'
        | _ :: np' ->
          aux cs root np' p'
      end
    | f :: p' -> (* down *)
      let f = F f in
      let x = path_target np root in
      let y = fresh_var_feature f (constraints_vars cs) in
      let np' = (y, f) :: np in
      let st_ok, st_fails' =
        let cs'' =
          (* [Dir x] implied by feature constraint  *)
          Feature (x, f, y) &
          cs
        in
        aux cs'' root np' p'
      in
      let st_fails = [
        np, Reg x & cs;
        np, Dir x & Absent (x, f) & cs;
      ] @ st_fails' in
      st_ok, st_fails
  in
  match String.split_on_char '/' p with
  | "" :: p' -> (* absolute path *)
    aux fs.constraints fs.root [] p'
  | p' -> (* relative path *)
    aux fs.constraints fs.root fs.cwd p'

(** Feature constraint lifted to normal paths.

    TODO Is this required or should we always use [resolve]? *)
let rec feature_path vars x p y =
  match p with
  | [] -> [Eq (x, y)]
  | f :: p' ->
    let z = fresh_var_feature f vars in
    Feature (x, f, z) :: feature_path (z & vars) z p' y

(** [present_path st p] implements the macro {% cwd[p]\downarrow %} from document
   `Specification of UNIX Commands`.*)
let rec present_path cs x p =
  match p with
  | [] -> cs
  | f :: p' ->
    let y = fresh_var_feature f (constraints_vars cs) in
    let cs' = Feature (x, f, y) & cs in
    present_path cs' y p'

(** [absent_path st p] implements the macro {% cwd[p]\uparrow %} from document
   `Specification of UNIX Commands`.

    Tentative implementation using [resolve] -- requires discussion first **)
let absent_path fs p =
  let _, cs_err = resolve fs p in
  cs_err

let print_err str sta =
  { sta with stdout = sta.stdout |> output ("[ERR] "^str) |> newline }

let print_dbg str sta =
  { sta with stdout = sta.stdout |> output ("[DBG] "^str) |> newline }

let interp_echo sta args =
  let sta' =
    match args with
    | "-n" :: args ->
      let str = String.concat " " args in
      { sta with stdout = sta.stdout |> output str }
    | args ->
      let str = String.concat " " args in
      { sta with stdout = sta.stdout |> output str }
  in
  [true, sta']

let interp_test_e sta args =
  match args with
  | [path] ->
    let (cs, _), cs_errs = resolve sta.filesystem path in
    let st_ok =
      let filesystem = {sta.filesystem with constraints=cs} in
      true, print_dbg "test-e ok" {sta with filesystem}
    in
    let sts_err =
      let aux (np, cs) =
        let filesystem = {sta.filesystem with constraints=cs} in
        let info = asprintf "test-e no resolve (child of %a)" print_path (np, sta.filesystem.root) in
        false, print_dbg info {sta with filesystem}
      in
      List.map aux cs_errs
    in
    st_ok :: sts_err
  | _ -> [false, print_err "test-e: Not exactly one argument" sta]

let interp_cd sta args =
  match args with
  | [path] ->
    let (constraints, np), cs_err = resolve sta.filesystem path in
    let x = path_target np sta.filesystem.root in
    let st_ok =
      let constraints = Dir x & constraints in
      let filesystem = {sta.filesystem with constraints; cwd=np} in
      true, {sta with filesystem}
    in
    let sts_err =
      let st_err_reg =
        let filesystem =
          let constraints = Reg x & constraints in
          {sta.filesystem with constraints}
        in
        false, print_err "cd: Not a directory" {sta with filesystem}
      in
      let sts_err =
        let aux (_, constraints) =
          let filesystem = {sta.filesystem with constraints} in
          false, print_err "No such file or directory" {sta with filesystem}
        in
        List.map aux cs_err
      in
      st_err_reg :: sts_err
    in
    st_ok :: sts_err
  | _ -> [false, print_err "cd: Not exactly one argument" sta]

(** When [np] is a path starting at [r], [similar cs r np r'] creates similarity constraints along from [r] along [np] starting at [r'] *)
let similar_path cs r np f r' : constraints * path =
  let rec aux cs x np y np' =
    match np with
    | [] ->
      let y' = fresh_var_feature f (constraints_vars cs) in
      let cs' =
        Similar (x, Set.singleton f, y) &
        Feature (y, f, y') &
        cs
      in
      cs', ((y', f) :: np')
    | (f, x') :: np'' ->
      let y' = fresh_var_like x' (constraints_vars cs) in
      let cs' =
        Similar (x, Set.singleton f, y) &
        (* Feature (x, f, x') is already implied by `np` as the result of `resolve` *)
        Feature (y, f, y') &
        cs
      in
      aux cs' x' np'' y' ((y', f) :: np')
  in
  aux cs r (reverse_path np) r' []

(** This corresponds to section {e Similarity constraint lifted to normal paths} in the
    {e Specification of UNIX commands}.

    Contrary to the previous implementations, it does not reuse the nodes from resolving the path [p], though.  *)
let rec similar_path' cs x p y =
  match p with
  | [] -> failwith "similar_path''"
  | [f] -> Similar (x, Set.singleton (F f), y) & cs
  | f :: p' ->
    let f = F f in
    let vars = constraints_vars cs in
    let x' = fresh_var_feature f vars in
    let y' = fresh_var_feature f (x' & vars) in
    let cs' =
      Similar (x, Set.singleton f, y) &
      Feature (x, f, x') &
      Feature (y, f, y') &
      cs
    in
    similar_path' cs' x' p' y'

(** Align the cwd path with a similar path *)
let path_align_similar root np root' np' =
  let rec aux x p x' p' =
    match p, p' with
    | (f, y) :: p_rest, (g, y') :: p'_rest ->
      if f = g
      then (g, y') :: aux y p_rest y' p'_rest
      else (* paths diverge, keep [p] *) p
    | [], _ -> []
    | _, [] -> p
  in
  reverse_path @@
  aux root (reverse_path np)
    root' (reverse_path np')

let last_path_component path =
  match List.rev (String.split_on_char '/' path) with
  | f :: p -> Some (String.join "/" (List.rev p), f)
  | _ -> None

let create_node fs np f =
  let root = fresh_var_like fs.root (constraints_vars fs.constraints) in
  let constraints, np' = similar_path fs.constraints fs.root np f root in
  let cwd = path_align_similar fs.root fs.cwd root np' in
  let y' = path_target np' root in
  y', {root; constraints; cwd}

(** Symbolic interpretation of command touch *)
let interp_touch sta args =
  match args with
  | [path] ->
    begin match last_path_component path with
      | Some (p, f) -> (* [path = p/f] *)
        let f = F f in
        let (cs, np), cs_err = resolve sta.filesystem p in
        (* [p] resolves to [x] in the original state *)
        let x = path_target np sta.filesystem.root in
        let st1_ok = (* p/f exists, nop *)
          let constraints =
            Present (x, f) &
            cs
          in
          let filesystem = {sta.filesystem with constraints} in
          true, print_dbg "touch: present" {sta with filesystem}
        in
        let st2_ok = (* p exists, p/f doesn't *)
          let y', fs = create_node {sta.filesystem with constraints=cs} np f in
          let constraints =
            Absent (x, f) &
            Reg y' &
            fs.constraints
          in
          let filesystem = {fs with constraints} in
          true, print_dbg "touch: absent" {sta with filesystem}
        in
        let sts_err =
          (* p does not exist *)
          let aux (_, constraints) =
            let filesystem = {sta.filesystem with constraints} in
            false, print_dbg "touch: no resolve" {sta with filesystem}
          in
          List.map aux cs_err
        in
        st1_ok :: st2_ok :: sts_err
      | None -> [false, print_err "touch: empty path" sta]
    end
  | _ -> [false, print_err "touch: not exactly one argument" sta]

(* Interprete mkdir *)
let interp_mkdir sta args =
  match args with
  | [path] ->
    begin match last_path_component path with
      | Some (p, f) -> (* [path = p/f] *)
        let f = F f in
        let (cs, np), errs = resolve sta.filesystem p in
        (* [p] targets [x] in [sta] *)
        let x = path_target np sta.filesystem.root in
        let sta_nor = (* p exists, p/f does not exist *)
          let y', fs = create_node {sta.filesystem with constraints=cs} np f in
          let filesystem =
            let constraints =
              Absent (x, f) &
              Dir y' & (* implied by Fence? *)
              Fence (y', Set.empty) & (* TODO That’s true for the moment but how to invalidate in subsequent commands that fill [y']? *)
              fs.constraints
            in
            {fs with constraints}
          in
          true, print_dbg "mkdir: created" {sta with filesystem}
        in
        let sta_err = (* p/f already exists *)
          let filesystem =
            let constraints =
              Present (x, f) &
              cs
            in
            {sta.filesystem with constraints}
          in
          false, print_err "mkdir: File exists" {sta with filesystem}
        in
        let sts_err =
          let aux (_, constraints) =
            let filesystem = {sta.filesystem with constraints} in
            false, print_err "mkdir: No such file or directory" {sta with filesystem}
          in
          List.map aux errs
        in
        sta_nor :: sta_err :: sts_err
      | None ->
        [false, print_err "mkdir: empty path" sta]
    end
  | _ -> [false, print_err "mkdir: not exactly one argument" sta]

let interp (sta:state) (name: string) (args:string list) : (bool * state) list =
  match name with
  | "true" -> [true, sta]
  | "false" -> [false, sta]
  | "echo" -> interp_echo sta args
  | "test-e" -> interp_test_e sta args
  | "cd" -> interp_cd sta args
  | "touch" -> interp_touch sta args
  | "mkdir" -> interp_mkdir sta args
  | _ -> [false, print_err ("Unknown builtin: "^name) sta]
