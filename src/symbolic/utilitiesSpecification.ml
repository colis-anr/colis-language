open Constraints
open SymbolicInterpreter__Filesystem
open SymbolicInterpreter__State
open Semantics__Buffers

type utility = state -> (state * bool) list

let return result : utility =
  function sta -> [sta, result]

let apply_to_list l u =
  (* apply utility [u] to a list [l] of states *)
  List.concat (List.map u l)

let separate_states l =
  (* split a list of pairs state*bool into the list of states with flag
     [true] and the list of pairs with flag [false]
   *)
  let rec separate_aux posacc negacc = function
    | [] -> (posacc,negacc)
    | (s,true)::l -> separate_aux (s::posacc) negacc l
    | (s,false)::l -> separate_aux posacc (s::negacc) l
  in separate_aux [] [] l

let choice u1 u2 =
  (* non-deterministic choice *)
  function state -> (u1 state) @ (u2 state)

let if_then_else (cond:utility) (posbranch:utility) (negbranch:utility) =
  function sta ->
            let (posstates,negstates) = separate_states (cond sta)
            in 
            (apply_to_list posstates posbranch)
            @ (apply_to_list negstates negbranch)

let compose_non_strict (u1:utility) (u2:utility) =
  function sta ->
    apply_to_list (List.map fst (u1 sta)) u2

let compose_strict (u1:utility) (u2:utility) =
  function sta ->
    let (success1,failure1) = separate_states (u1 sta)
    in (apply_to_list success1 u2) @
         (List.map (function sta -> (sta,false)) failure1)
         
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
