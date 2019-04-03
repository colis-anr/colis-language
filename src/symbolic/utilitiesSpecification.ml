open Constraints
open SymbolicInterpreter__Filesystem
open SymbolicInterpreter__State
open Semantics__Buffers

type utility = state -> (state * bool) list

let identity = fun s -> [(s, true)]

let seq join (u1 : utility) (u2 : utility) : utility = fun s0 -> List.(
     u1 s0
  |> map (fun (s1, b1) -> u2 s1 |> map (fun (s2, b2) -> (s2, join b1 b2)))
  |> flatten
)

let print_line msg state =
  let open Semantics__Buffers in
  let stdout = Stdout.(output msg state.stdout |> newline) in
  {state with stdout}

let print_utility_trace msg state =
  if String.equal msg "" then
    state
  else
    let msg = "[UTL] "^msg in
    print_line msg state

let print_error msg state =
  match msg with
  | Some msg ->
    let msg = "[ERR] "^msg in
    print_line msg state
  | None ->
    state

type case = {
  result : bool;
  spec : Clause.t;
  descr : string;
  stdout : Stdout.t ;
  error_message: string option;
}

let success_case ~descr ?(stdout=Stdout.empty) spec =
  { result = true ; error_message = None ; stdout ; descr; spec }

let error_case ~descr ?(stdout=Stdout.empty) ?error_message spec =
  { result = false ; error_message ; stdout ; descr ; spec }

let failure ?error_message () =
  [{ result = false ;
     descr = "" ;
     stdout = Stdout.empty ;
     error_message ;
     spec = Clause.true_ }]

let quantify_over_intermediate_root state conj =
  if BatOption.eq ~eq:Var.equal state.filesystem.root0 (Some state.filesystem.root) then
    [conj]
  else
    Clause.quantify_over state.filesystem.root conj

let apply_output_to_state (state : state) stdout =
  { state with stdout = Stdout.concat state.stdout stdout }

(* Create the corresponding filesystem, update the state and create corresponding
    result **)
let apply_clause_to_state state case root clause =
  let filesystem = {state.filesystem with clause; root} in
  let state' =
    { state with filesystem }
    |> print_error case.error_message
  in
  state', case.result

let apply_case_to_state state root case : (state * bool) list =
  let state = print_utility_trace case.descr state in
  let state = apply_output_to_state state case.stdout in
  (* Add the case specification to the current clause *)
  Clause.add_to_sat_conj case.spec state.filesystem.clause
  |> List.map (quantify_over_intermediate_root state)
  |> List.flatten
  |> List.map (apply_clause_to_state state case root)

type specifications = cwd:Path.t -> root:Var.t -> root':Var.t -> case list

let under_specifications : specifications -> state -> (state * bool) list =
  fun spec state ->
    let new_root = Var.fresh ~hint:(Var.hint state.filesystem.root) () in
    let cases = spec ~cwd:state.filesystem.cwd ~root:state.filesystem.root ~root':new_root in
    List.map (apply_case_to_state state new_root) cases |> List.flatten
