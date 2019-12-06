open Colis_internals
open Colis_constraints
open Semantics__Result
open Semantics__Buffers
open SymbolicInterpreter__Semantics

type utility = state -> (state * bool result) list

let return result : utility =
  function sta -> [sta, Ok result]

let apply_to_list l u =
  (* apply utility [u] to a list [l] of states *)
  List.concat (List.map u l)

let separate_states l =
  (* split a list of pairs state*bool into the list of states with flag
     [true] and the list of pairs with flag [false]
   *)
  let rec separate_aux posacc negacc incacc = function
    | [] -> (posacc,negacc,incacc)
    | (s, Ok true)::l -> separate_aux (s::posacc) negacc incacc l
    | (s, Ok false)::l -> separate_aux posacc (s::negacc) incacc l
    | (_, Incomplete) as s::l -> separate_aux posacc negacc (s::incacc) l
  in separate_aux [] [] [] l

let choice u1 u2 =
  (* non-deterministic choice *)
  function state -> (u1 state) @ (u2 state)

let if_then_else (cond:utility) (posbranch:utility) (negbranch:utility) =
  function sta ->
    let (posstates,negstates,incstates) = separate_states (cond sta)
    in
    (apply_to_list posstates posbranch)
    @ (apply_to_list negstates negbranch)
    @ incstates

let if_then (cond:utility) (posbranch:utility) =
  function sta ->
    let (posstates,negstates,incstates) = separate_states (cond sta)
    in
    (apply_to_list posstates posbranch)
    @ (List.map (function sta -> (sta,Ok true)) negstates)
    @ incstates

let uneg (u:utility) : utility = fun st ->
  List.map (function (s,Ok b) -> (s, Ok (not b)) | x -> x) (u st)

let combine_results combinator u1 u2 : utility = fun st ->
  List.flatten
    (List.map
       (function (s1,Ok b1) ->
          List.map (function
              | (s2,Ok b2) -> (s2,Ok(combinator b1 b2))
              | x -> x) (u2 s1)
          | x -> [x])
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
    let (success1,failure1,incomplete1) = separate_states (u1 sta)
    in (apply_to_list success1 u2) @
       (List.map (function sta -> (sta,Ok false)) failure1) @
       incomplete1

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
    let log = print_output ~newline:true ("[ERROR] "^str) sta.log in
    {sta with log}

let print_incomplete_trace str sta =
  if String.equal str "" then
    sta
  else
    let log =
      print_output ~newline:true ("[INCOMPLETE] "^str) sta.log in
    {sta with log}

let print_utility_trace str sta =
  if String.equal str "" then
    sta
  else
    let log =
      print_output ~newline:true ("[TRACE] "^str) sta.log in
    {sta with log}

type case_spec = Var.t -> Var.t -> Clause.t

let noop : case_spec =
  Clause.eq

type case = {
  result : bool result;
  spec : case_spec;
  descr : string;
  stdout : Stdout.t ;
  error_message: string option;
}

let success_case ~descr ?(stdout=Stdout.empty) spec =
  { result = Ok true ; error_message = None ; stdout ; descr; spec }

let error_case ~descr ?(stdout=Stdout.empty) ?error_message spec =
  { result = Ok false ; error_message ; stdout ; descr ; spec }

let incomplete_case ~descr () =
  { result = Incomplete ;
    descr ;
    stdout = Stdout.empty ;
    error_message = None;
    spec = noop }

(** Apply the case specifications to a filesystem, resulting in a list of possible filesystems. *)
let apply_spec fs spec =
  let open SymbolicInterpreter__Filesystem in
  let root_is_root0 =
    (* fs.root0 = Some fs.root - garbare-collect fs.root only otherwise *)
    match fs.root0 with
    | Some root0 -> Var.equal root0 fs.root
    | None -> false in
  let root' = Var.fresh ~hint:(Var.hint fs.root) () in
  let clause = spec fs.root root' in
  let clauses = Clause.add_to_sat_conj clause fs.clause in
  let clauses =
    if root_is_root0 then
      clauses
    else
      List.flatten
        (List.map (Clause.quantify_over_and_simplify fs.root)
           clauses) in
  List.map (fun clause -> {fs with clause; root=root'}) clauses

(** Apply a case to a state.

    This may result in multiple states because the integration of the case clause in the
    filesystem may result in multiple clauses. *)
let apply_case sta case : (state * bool result) list =
  (* First print the utility trace *)
  let sta =
    let print = match case.result with Ok _ -> print_utility_trace | Incomplete -> print_incomplete_trace in
    print case.descr sta in
  let sta = {
    (* output case stdout to stdout and log *)
    stdout = Stdout.concat sta.stdout case.stdout;
    log = Stdout.concat sta.log case.stdout;
    (* don't touch stdin *)
    stdin = sta.stdin;
    (* and keep the filesystem (may be changed by apply_spec) *)
    filesystem = sta.filesystem;
  } in
  (* (Optionally) print error message *)
  if case.result = Incomplete then
    (* Make sure that the description is the last message on the log *)
    assert (case.error_message = None);
  let sta = print_error case.error_message sta in
  (* Apply the case specifications to the filesystem *)
  apply_spec sta.filesystem case.spec |>
  (* Inject the resulting filesystems into the state *)
  List.map (fun filesystem -> {sta with filesystem}) |>
  (* Add the result to each result state *)
  List.map (fun sta -> sta, case.result)

type specifications = case list

let under_specifications : specifications -> state -> (state * bool result) list =
  fun spec state ->
  List.flatten (List.map (apply_case state) spec)

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
    let sta' = print_error msg sta in
    [ sta', Ok false ]

let incomplete ~descr () : utility =
  fun sta ->
    let sta' = print_incomplete_trace descr sta in
    [sta', Incomplete]

let unsupported ~utility msg =
  if !Options.fail_on_unknown_utilities then
    Errors.unsupported ~utility msg
  else
    incomplete ~descr:(utility ^ ": " ^ msg) ()

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

let table =
  let table = Hashtbl.create 10 in
  (* TODO Register all POSIX utilities as: incomplete ~descr:(name^": not implemented") *)
  table

let register (module M:SYMBOLIC_UTILITY) =
  Hashtbl.replace table M.name M.interprete

let is_registered = Hashtbl.mem table

let dispatch ~name =
  try Hashtbl.find table name
  with Not_found -> fun _ ->
    error ~msg:(name^": command not found") ()

let call name ctx args =
  dispatch ~name {ctx with args}

let dispatch' (cwd, env, args) name sta =
  let cwd = List.map Colis_constraints.Feat.from_string cwd in
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
