open Semantics__Buffers
open SymbolicInterpreter__Filesystem
open SymbolicInterpreter__State
open Constraints

let print_line str sta =
  let stdout = Stdout.(output str sta.stdout |> newline) in
  {sta with stdout}

let print_err str sta =
  print_line ("[ERR] "^str) sta

let print_dbg str sta =
  print_line ("[DBG] "^str) sta

let error str sta =
  print_err str sta, false

let interp_echo sta args =
  let str = String.concat " " args in
  [print_line str sta, true]

(* let with_clause sta clause =
 *   let filesystem = {sta.filesystem with clause} in
 *   [{sta with filesystem}] *)

module Specification =
  struct
    type case =
      { outcome : bool ;
        pre_state : Clause.t ;
        post_state : Clause.t }

    let apply_case_to_filesystem filesystem new_root case =
      (* Add pre_state and post_state to the current clause *)
      Clause.(add_to_conj (case.pre_state & case.post_state) filesystem.clause)
      (* For each clause in the received disjunction, create the
         corresponding filesystem. *)
      |> Clause.fold
           (fun filesystems clause ->
             { root = new_root ;
               clause ;
               cwd = filesystem.cwd (* FIXME *) }
             :: filesystems)
           []

      let apply_case_to_state state new_root case =
        apply_case_to_filesystem state.filesystem new_root case
        |> List.map
             (fun filesystem ->
              { state with filesystem }, case.outcome)

    type t = Var.t -> Var.t -> case list

    let apply_to_state state spec : (state * bool) list =
      let new_root = Var.fresh ~hintv:state.filesystem.root () in
      List.map
        (apply_case_to_state state new_root)
        (spec state.filesystem.root new_root)
      |> List.flatten
  end

(* Interpretation of "touch" *)

let interp_touch sta args =
  let open Clause in
  match args with
  | [full_path] ->
     let full_path = Path.from_string full_path in
     let path, _ = Path.split_last full_path in
     Specification.apply_to_state sta @@
       fun root root' ->
       [ (* The dir "path" exists but does not have "feat". *)
         { pre_state = dir (root, path) & nex (root, full_path) ;
           outcome = true ;
           post_state = sim1 root full_path root' & reg (root', full_path) } ;
         (* The dir "path" exists and has "feat". *)
         { pre_state = ex (root, full_path) ;
           outcome = true ;
           post_state = eq (root, Path.empty) (root', Path.empty) } ;
         (* The dir "path" does not exist. *)
         { pre_state = ndir (root, path) ; (*FIXME: see clause.mli*)
           outcome = false ;
           post_state = eq (root, Path.empty) (root', Path.empty) } ]
  | _ ->
     [error "touch: not exactly one argument" sta]

(* Interpretation of "mkdir" *)

let interp_mkdir sta args =
  let open Clause in
  match args with
  | [full_path] ->
     let full_path = Path.from_string full_path in
     let path, _ = Path.split_last full_path in
     Specification.apply_to_state sta @@
       fun root root' ->
       [ (* The dir "path" exists but does not have "feat". *)
         { pre_state = dir (root, path) & nex (root, full_path) ;
           outcome = true ;
           post_state = sim1 root full_path root'
                        & dir (root', full_path) & empty (root', full_path) } ;
         (* The dir "path" does not exist. *)
         { pre_state = nex_ndir (root, path) ;
           outcome = false ;
           post_state = eq (root, Path.empty) (root', Path.empty) } ;
         (* The dir "path" exists and has "feat". *)
         { pre_state = ex (root, full_path) ;
           outcome = false ;
           post_state = eq (root, Path.empty) (root', Path.empty) } ]
  | _ ->
    [error "mkdir: not exactly one argument" sta]

(* Interpretation of all commands *)

let interp (sta: state) (id: string) (args:string list) : (state * bool) list =
  match id with
  | "true" -> [sta, true]
  | "false" -> [sta, false]
  | "echo" -> interp_echo sta args
  (* | "test-e" -> interp_test_e sta args *)
  | "touch" -> interp_touch sta args
  | "mkdir" -> interp_mkdir sta args
  | _ ->
    [error ("Unknown builtin: "^id) sta]
