open Colis_internals
open Colis_constraints
open SymbolicInterpreter__Semantics
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

let if_then (cond:utility) (posbranch:utility) =
  function sta ->
    let (posstates,negstates) = separate_states (cond sta)
    in
    (apply_to_list posstates posbranch)
    @ (List.map (function sta -> (sta,true)) negstates)

let uneg (u:utility) : utility = fun st ->
  List.map (fun (s,b) -> (s, not b)) (u st)

let combine_results combinator u1 u2 : utility = fun st ->
  List.flatten
    (List.map
       (fun (s1,b1) ->
         List.map (fun (s2, b2) -> (s2, combinator b1 b2)) (u2 s1))
       (u1 st))

let uand =
  combine_results ( && )

let uor =
  combine_results ( || )

let multiple_times what args : utility =
  let rec aux = function
    | [] -> assert false (* By precondition. *)
    | [x] -> what x
    | x :: xs -> uand (what x) (aux xs)
  in
  aux args

let compose_non_strict (u1:utility) (u2:utility) =
  function sta ->
    apply_to_list (List.map fst (u1 sta)) u2

let compose_strict (u1:utility) (u2:utility) =
  function sta ->
    let (success1,failure1) = separate_states (u1 sta)
    in (apply_to_list success1 u2) @
         (List.map (function sta -> (sta,false)) failure1)

let print_output ~newline str output =
  let output = Stdout.output str output in
  if newline then Stdout.newline output else output

let print_stdout ~newline str sta =
  let stdout = print_output ~newline str sta.stdout in
  let log = print_output ~newline str sta.log in
  {sta with stdout; log}

let print_error opt sta =
  match opt with
  | None -> sta
  | Some str ->
    let log = print_output ~newline:true ("[ERR] "^str) sta.log in
    {sta with log}

let print_utility_trace str sta =
  if String.equal str "" then
    sta
  else
    let log = print_output ~newline:true ("[UTL] "^str) sta.log in
    {sta with log}

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

let failure ?error_message ~root ~root' =
  [{ result = false ;
     descr = "" ;
     stdout = Stdout.empty ;
     error_message ;
     spec = Clause.eq root root' }]

let quantify_over_intermediate_root fs conj =
  if BatOption.eq ~eq:Var.equal fs.SymbolicInterpreter__Filesystem.root0 (Some fs.root) then
    [conj]
  else
    Clause.quantify_over_and_simplify fs.root conj

(** Update a state with a specification case and a new root [root'].

    This may result in multiple states because the integration of the case clause in the
    filesystem may result in multiple clauses. *)
let apply_case_to_state sta root' case : (state * bool) list =
  (* First print the utility trace *)
  let sta = print_utility_trace case.descr sta in
  let sta = {
    (* output case stdout to stdout and log *)
    stdout = Stdout.concat sta.stdout case.stdout;
    log = Stdout.concat sta.log case.stdout;
    (* don't touch stdin *)
    stdin = sta.stdin;
    (* and keep the filesystem for the moment ... *)
    filesystem = sta.filesystem;
  } in
  (* (Optionally) print error message *)
  let sta = print_error case.error_message sta in
  (* Combine the case specification and the current filesystem clause *)
  Clause.add_to_sat_conj case.spec sta.filesystem.clause
  |> List.map (quantify_over_intermediate_root sta.filesystem)
  |> List.flatten
  (* ... now update the filesystem in the state with the new clauses and the new root *)
  |> List.map (fun clause ->
      let filesystem = {sta.filesystem with clause; root=root'} in
      {sta with filesystem})
  (* and combine each state with the case result *)
  |> List.map (fun sta -> sta, case.result)

type specifications = root:Var.t -> root':Var.t -> case list

let under_specifications : specifications -> state -> (state * bool) list =
  fun spec state ->
    let new_root = Var.fresh ~hint:(Var.hint state.filesystem.root) () in
    let cases = spec ~root:state.filesystem.root ~root':new_root in
    List.map (apply_case_to_state state new_root) cases |> List.flatten

(******************************************************************************)
(*                                  Auxiliaries                               *)
(******************************************************************************)

let last_comp_as_hint: root:Var.t -> Path.t -> string option =
  fun ~root path ->
    match Path.split_last path with
    | Some (_, Down f) ->
      Some (Feat.to_string f)
    | None -> (* Empty parent path => root *)
      Some (Var.hint root)
    | Some (_, (Here|Up)) ->
       (* We can’t know (if last component in parent path is a symbolic link) *)
      None

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

let unsupported ~utility msg =
  if !Options.fail_on_unknown_utilities then
    Errors.unsupported ~utility msg
  else
    error ~msg:(utility ^ ": " ^ msg) ()

let unknown_utility utility =
  if !Options.fail_on_unknown_utilities then
    Errors.unsupported ~utility "unknown utility"
  else
    error ~msg:(utility ^ ": command not found") ()

module IdMap = Env.IdMap

type context = {
  args: string list;
  cwd: Path.normal;
  env: string IdMap.t;
}

module type SYMBOLIC_UTILITY = sig
  val name : string
  val interprete : context -> utility
end

let table = Hashtbl.create 10

let register (module M:SYMBOLIC_UTILITY) =
  Hashtbl.replace table M.name M.interprete

let is_registered = Hashtbl.mem table

let dispatch ~name =
  try Hashtbl.find table name
  with Not_found -> fun _ -> unknown_utility name

let call name ctx args =
  dispatch ~name {ctx with args}

let dispatch' (cwd, env, args) name sta =
  let ctx = {cwd; args; env} in
  BatSet.of_list (dispatch ~name ctx sta)

(**/**)

let null_formatter = Format.make_formatter (fun _ _ _ -> ()) (fun () -> ())

let with_formatter_to_string f =
  let buf = Buffer.create 8 in
  let fmt = Format.formatter_of_buffer buf in
  let v =f fmt in
  Format.pp_print_flush fmt ();
  (Buffer.contents buf, v)

let cmdliner_eval_utility ~utility ?(empty_pos_args=false) fun_and_args ctx =
  let pos_args = Cmdliner.Arg.(
      (if empty_pos_args then value else non_empty)
      & pos_all string []
      & info [])
  in
  let argv = ctx.args |> List.cons utility |> Array.of_list in
  let (err, result) =
    with_formatter_to_string @@ fun err ->
    Cmdliner.Term.(
      eval
        (
          fun_and_args $ const ctx $ pos_args,
          info utility ~exits:default_exits
        )
        ~argv
        ~env:(fun var -> Env.IdMap.find_opt var ctx.env)
        ~help:null_formatter ~err ~catch:false
    )
  in
  match result with
  | `Ok a -> a
  | `Version -> Errors.unsupported ~utility "version"
  | `Help -> Errors.unsupported ~utility "help"
  | `Error (`Parse | `Term) -> Errors.unsupported ~utility ("parse error: " ^ err)
  | `Error `Exn -> assert false (* because ~catch:false *)
