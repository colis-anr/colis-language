open Colis_constraints
open Semantics__Result
open Semantics__Buffers
open Semantics__UtilityContext

module type FILESYSTEM = sig
  type filesystem
end

exception Incomplete_case_spec

module type CASESPEC = sig
  type filesystem
  type case_spec
  val noop : case_spec
  val apply_spec : filesystem -> case_spec -> filesystem list
end

module MakeInterpreter (Filesystem: FILESYSTEM) = struct

  module Semantics = SymbolicInterpreter__Interpreter.MakeSemantics (Filesystem)

  type state = Semantics.state = {
    filesystem: Filesystem.filesystem;
    stdin: string list;
    stdout: Stdout.t;
    log: Stdout.t;
  }

  open Semantics

  type utility = state -> (state * bool result) list

  let print_output ~newline str output =
    let output = Stdout.output str output in
    if newline then Stdout.newline output else output

  let print_stdout ~newline str sta =
    let stdout = print_output ~newline str sta.stdout in
    let log = print_output ~newline str sta.log in
    {sta with stdout; log}

  let print_error str sta =
    let log = print_output ~newline:true ("[ERROR] "^str) sta.log in
    {sta with log}

  let print_utility_trace str sta =
    if String.equal str "" then
      sta
    else
      let log = print_output ~newline:true ("[TRACE] "^str) sta.log in
      {sta with log}

  let print_incomplete_trace str sta =
    let log = print_output ~newline:true ("[INCOMPLETE] "^str) sta.log in
    {sta with log}

  let error ~utility msg : utility =
    fun sta ->
    let str = utility ^ ": " ^ msg in
    let sta = print_error str sta in
    [sta, Ok false]

  let incomplete ~utility msg : utility =
    fun sta ->
    let str = utility ^ ": " ^ msg in
    let sta = print_incomplete_trace str sta in
    [sta, Incomplete]

  let unknown ~utility msg : utility =
    let open Colis_internals in
    match !Options.unknown_behaviour with
    | Exception -> raise (Errors.Unknown_behaviour (utility, msg))
    | Incomplete -> incomplete ~utility msg
    | Error -> error ~utility "not found"

  let table : (string, utility_context -> utility) Hashtbl.t =
    let table = Hashtbl.create 10 in
    (* TODO Register all POSIX utilities as: incomplete ~descr:(name^": not implemented") *)
    table

  module type SYMBOLIC_UTILITY = sig
    val name : string
    val interprete : utility_context -> utility
  end

  let register (module M: SYMBOLIC_UTILITY) =
    Hashtbl.replace table M.name M.interprete

  let is_registered = Hashtbl.mem table

  let dispatch ~name =
    try Hashtbl.find table name
    with Not_found ->
    fun _ctx sta ->
      unknown ~utility:name "command not found" sta

  module Arg = struct
    let sym_interp_utility (ctx, name, sta) =
      dispatch ~name ctx sta
  end

  module Interpreter = MakeInterpreter (Arg)

  type sym_state = {
    context : Semantics__Context.context;
    state : Semantics.state;
  }

  let interp_program inp stas pro =
    let stas = List.map (fun {context; state} -> Interpreter.({context; state; data=()})) stas in
    Interpreter.interp_program inp stas pro

  let call name ctx args =
    dispatch ~name {ctx with args}

  let return result : utility =
    function sta ->
      [sta, Ok result]

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

  (******************************************************************************)
  (*                                  Auxiliaries                               *)
  (******************************************************************************)

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
          ~env:(fun var -> Env.SMap.find_opt var ctx.env)
          ~help:null_formatter ~err ~catch:false
      )
    in
    match result with
    | `Ok a -> a
    | `Version -> error ~utility "version"
    | `Help -> error ~utility "help"
    | `Error (`Parse | `Term) -> unknown ~utility ("parse error: " ^ err)
    | `Error `Exn -> assert false (* because ~catch:false *)

  (**/**)

  module MakeSpecifications (CaseSpec: CASESPEC with type filesystem = Filesystem.filesystem) = struct

    open CaseSpec

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

    let incomplete_case ~descr spec =
      { result = Incomplete ;
        descr ;
        stdout = Stdout.empty ;
        error_message = None;
        spec }

    (** Apply a case to a state.

        This may result in multiple states because the integration of the case clause in the
        filesystem may result in multiple clauses. *)
    let apply_case sta case : (state * bool result) list =
      (* First print the utility trace *)
      let sta =
        if case.result = Incomplete
        then sta (* Print incomplete trace last *)
        else print_utility_trace case.descr sta in
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
      let sta =
        match case.error_message with
        | Some msg -> print_error msg sta
        | None -> sta in
      let sta =
        if case.result = Incomplete
        then print_incomplete_trace case.descr sta
        else sta in
      (* Apply the specification and possibly induce incomplete behaviour *)
      let filesystems, result =
        try apply_spec sta.filesystem case.spec, case.result
        with Incomplete_case_spec -> [sta.filesystem], Incomplete in
      (* Apply the case specifications to the filesystem *)
      filesystems |>
      (* Inject the resulting filesystems into the state *)
      List.map (fun filesystem -> {sta with filesystem}) |>
      (* Add the result to each result state *)
      List.map (fun sta -> sta, result)

    let specification_cases cases state =
      List.flatten (List.map (apply_case state) cases)
  end
end

module UtilityContext = struct
  type context = utility_context = {
    cwd: Path.normal;
    env: string Env.SMap.t;
    args: string list;
  }
end

(* Constraints *)

module ConstraintsImplementation = struct

  type filesystem = {
    root: Var.t;
    clause: Clause.sat_conj;
    root0: Var.t option;
  }

  type case_spec = Var.t -> Var.t -> Clause.t

  let noop = Clause.eq

  (** Apply the case specifications to a filesystem, resulting in a list of possible filesystems. *)
  let apply_spec fs spec =
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
end

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

module Constraints = struct
  include ConstraintsImplementation
  include MakeInterpreter (ConstraintsImplementation)
  include MakeSpecifications (ConstraintsImplementation)
end

(* Transducers *)

module TransducersImplementation = struct
  type filesystem = unit
  type case_spec = unit
  let noop = ()
  let apply_spec fs spec =
    ignore spec; [fs]
end

module Transducers = struct
  include TransducersImplementation
  include MakeInterpreter (TransducersImplementation)
  include MakeSpecifications (TransducersImplementation)
end

(* Mixed *)

module MixedImplementation = struct

  type filesystem =
    | Constraints of ConstraintsImplementation.filesystem
    | Transducers of TransducersImplementation.filesystem

  (* The case_specs are wrapped in a closure because they may raise Incomplete_case_spec *)
  type case_spec = {
    constraints: unit -> ConstraintsImplementation.case_spec;
    transducers: unit -> TransducersImplementation.case_spec;
  }

  let case_spec ?transducers ?constraints () : case_spec =
    let case_spec_or_incomplete opt () =
      match opt with Some x -> x | None -> raise Incomplete_case_spec in
    {constraints = case_spec_or_incomplete constraints;
     transducers = case_spec_or_incomplete transducers}

  let noop : case_spec =
    case_spec
      ~transducers:TransducersImplementation.noop
      ~constraints:ConstraintsImplementation.noop
      ()

  let apply_spec fs spec =
    match fs with
    | Constraints fs ->
      ConstraintsImplementation.apply_spec fs (spec.constraints ()) |>
      List.map (fun fs -> Constraints fs)
    | Transducers fs ->
      TransducersImplementation.apply_spec fs (spec.transducers ()) |>
      List.map (fun fs -> Transducers fs)
end

module Mixed = struct
  include MixedImplementation
  include MakeInterpreter (MixedImplementation)
  include MakeSpecifications (MixedImplementation)

  let state_from_constraints (s : Constraints.state) : state = {
    filesystem=MixedImplementation.Constraints s.filesystem;
    stdin=s.stdin;
    stdout=s.stdout;
    log=s.log;
  }

  let sym_state_from_constraints (s: Constraints.sym_state) : sym_state = {
    state = state_from_constraints s.state;
    context = s.context;
  }

  let state_to_constraints (s: state) : Constraints.state =
    let filesystem =
      match s.filesystem with
      | Constraints fs -> fs
      | Transducers _ -> invalid_arg "state_to_constraints" in
    {filesystem; stdin=s.stdin; stdout=s.stdout; log=s.log}

  let interp_program_constraints inp stas pro =
    let stas' = List.map sym_state_from_constraints stas in
    let normals, errors, failures = interp_program inp stas' pro in
    List.map state_to_constraints normals, List.map state_to_constraints errors, List.map state_to_constraints failures

  let state_from_transducers (s : Transducers.state) : state = {
    filesystem=MixedImplementation.Transducers s.filesystem;
    stdin=s.stdin;
    stdout=s.stdout;
    log=s.log;
  }

  let sym_state_from_transducers (s: Transducers.sym_state) : sym_state = {
    state = state_from_transducers s.state;
    context = s.context;
  }

  let state_to_transducers (s: state) : Transducers.state =
    let filesystem =
      match s.filesystem with
      | Transducers fs -> fs
      | Constraints _ -> invalid_arg "state_to_transducers" in
    {filesystem; stdin=s.stdin; stdout=s.stdout; log=s.log}

  let interp_program_transducers inp stas pro =
    let stas' = List.map sym_state_from_transducers stas in
    let normals, errors, failures = interp_program inp stas' pro in
    List.map state_to_transducers normals, List.map state_to_transducers errors, List.map state_to_transducers failures

  let last_comp_as_hint = last_comp_as_hint
end

module ConstraintsCompatibility = struct

  include Mixed
  include UtilityContext

  let success_case ~descr ?stdout constraints =
    success_case ~descr ?stdout (case_spec ~constraints ())

  let error_case ~descr ?stdout ?error_message constraints =
    error_case ~descr ?stdout ?error_message (case_spec ~constraints ())

  let incomplete_case ~descr constraints =
    incomplete_case ~descr (case_spec ~constraints ())

  let noop = ConstraintsImplementation.noop

  let last_comp_as_hint = last_comp_as_hint
end
