open Format

module Errors = Errors
module Options = Options

module Language = struct
  module Nat = Syntax__Nat
  module Syntax = Syntax__Syntax
  module Parser = ColisParser
  module Lexer = ColisLexer
  module FromShell = FromShell
end

module Semantics = struct
  module Arguments = Semantics__Arguments
  module Behaviour = Semantics__Behaviour
  module Env = Env
  module Stdin = Semantics__Buffers.Stdin
  module Stdout = Semantics__Buffers.Stdout
  module Context = Semantics__Context
  module Input = Semantics__Input
end

module Concrete = struct
  module Filesystem = Interpreter__Filesystem
  module Interpreter = Interpreter__Interpreter
  module State = Interpreter__State
end

module Symbolic = struct
  module Filesystem = SymbolicInterpreter__Filesystem
  module FilesystemSpec = FilesystemSpec
  module State = SymbolicInterpreter__Semantics (* Semantics contais State *)
  module SymState = SymbolicInterpreter__SymState
  module Results = SymbolicInterpreter__Results
  module Interpreter = SymbolicInterpreter__Interpreter
  module Utilities = SymbolicUtilities
end

(* Parsers *)

type colis = Language.Syntax.program

let parse_colis_lexbuf ?(filename="-") lexbuf =
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p
      with Lexing.pos_fname = filename };
  try
    ColisParser.program ColisLexer.token lexbuf
  with
  | ColisLexer.LexerError s ->
    let pos = lexbuf.Lexing.lex_curr_p in
    raise (Errors.ParseError (s, pos))
  | ColisParser.Error ->
    let pos = lexbuf.Lexing.lex_curr_p in
    raise (Errors.ParseError ("", pos))

let parse_colis_channel ?(filename="-") channel =
  let lexbuf = Lexing.from_channel channel in
  parse_colis_lexbuf ~filename lexbuf

let parse_colis_file filename =
  let ic =
    try
      open_in filename
    with
      Sys_error msg -> raise (Errors.FileError msg)
  in
  try
    let colis = parse_colis_channel ~filename ic in
    close_in ic;
    colis
  with
    exn -> close_in ic; raise exn

let parse_colis_string string =
  let lexbuf = Lexing.from_string string in
  parse_colis_lexbuf lexbuf

let parse_shell_file = FromShell.parse_file

(* Printers *)

let pp_print_colis = ToColis.program

let print_colis =
  pp_print_colis Format.std_formatter

let colis_to_string colis =
  let buf = Buffer.create 8 in
  let fmt = Format.formatter_of_buffer buf in
  pp_print_colis fmt colis;
  Buffer.contents buf

let colis_to_file filename colis =
  let ochan = open_out filename in
  let fmt = Format.formatter_of_out_channel ochan in
  pp_print_colis fmt colis;
  close_out ochan

(* Interpret *)

let mk_var_env =
  let open Semantics in
  List.fold_left
    (* All variables given on the command line are exported! *)
    (fun env (id, value) -> Env.set env id {Context.value=Some value; exported=true})
    (Context.empty_var_env)

let run ~argument0 ?(arguments=[]) ?(vars=[]) colis =
  let open Semantics in
  let open Concrete in
  let input = { Input.empty with argument0 } in
  let state = State.empty_state () in
  state.arguments := arguments;
  state.var_env := mk_var_env vars;
  Interpreter.interp_program input state colis;
  print_string (Stdout.all_lines !(state.stdout) |> List.rev |> String.concat "\n");
  exit (if !(state.result) then 0 else 1)

let print_symbolic_filesystem fmt fs =
  let open Constraints in
  let open Symbolic.Filesystem in
  fprintf fmt "root: %a@\n" Var.pp fs.root;
  fprintf fmt "cwd: %a@\n" Path.pp fs.cwd;
  fprintf fmt "clause: %a@\n" Clause.pp_sat_conj fs.clause

let print_dot filename id clause =
  let ch = open_out filename in
  try
    let fmt = formatter_of_out_channel ch in
    Constraints.Clause.pp_sat_conj_as_dot ~name:id fmt clause;
    close_out ch
  with e ->
    close_out ch;
    raise e

let print_symbolic_state fmt ?id sta =
  let open Symbolic.State in
  begin match id with
    | Some id ->
      fprintf fmt "id: %s@\n" id;
      if !Options.print_states_dir <> "" then
        let filename = sprintf "%s/%s.dot" !Options.print_states_dir id in
        print_dot filename id sta.filesystem.clause;
    | None -> ()
  end;
  print_symbolic_filesystem fmt sta.filesystem;
  (* Print stdin *)
  if sta.stdin <> [] then begin
    fprintf fmt "stdin: |@\n";
    List.iter (fprintf fmt "  %s@\n")
      (List.rev sta.stdin)
  end;
  (* Print stdout *)
  if not (Semantics.Stdout.is_empty sta.stdout) then begin
    fprintf fmt "stdout: |@\n";
    List.iter (fprintf fmt "  %s@\n")
      (List.rev @@ sta.stdout.lines);
    fprintf fmt "  %s" sta.stdout.line
  end

type symbolic_config = {
  prune_init_state: bool;
  loop_limit: int;
  stack_size: int;
}

let run_symbolic config fs_spec ~argument0 ?(arguments=[]) ?(vars=[]) colis =
  let open Semantics in
  let open Symbolic in
  let open Constraints in
  let root = Constraints.Var.fresh ~hint:"r" () in
  (* Create disjunction representing the FS *)
  let disj =
    let clause = FilesystemSpec.compile root fs_spec in
    Clause.add_to_sat_conj clause Clause.true_sat_conj
  in
  (* Create corresponding states *)
  let stas =
    let root0 = if config.prune_init_state then None else Some root in
    let cwd = Constraints.Path.Abs [] in
    let aux conj =
      let filesystem = {Filesystem.clause=conj; cwd; root0; root} in
      {State.filesystem; stdin=Stdin.empty; stdout=Stdout.empty}
    in
    List.map aux disj
  in
  (* Create corresponding symbolic states by adding the context *)
  let stas' =
    let context =
      let var_env = mk_var_env vars in
      {Context.empty_context with arguments; var_env}
    in
    let aux state =
      {SymState.state; context; data=()}
    in
    List.map aux stas
  in
  let normals, errors, failures =
    let loop_limit = Z.of_int config.loop_limit in
    let stack_size = Z.of_int config.stack_size in
    let inp = { Input.empty with argument0 } in
    Interpreter.interp_program loop_limit stack_size inp (BatSet.of_list stas') colis
  in
  let print_symbolic_state label ctr fmt sta =
    let id = sprintf "%s-%d" label !ctr in
    incr ctr;
    fprintf fmt "- @[%a@]@\n" (print_symbolic_state ~id) sta
  in
  printf "* Initial states@\n";
  List.iter (print_symbolic_state "initial" (ref 1) Format.std_formatter) stas;
  if not (BatSet.is_empty normals) then begin
    printf "* Success states@\n";
    List.iter (print_symbolic_state "success" (ref 1) Format.std_formatter) (BatSet.to_list normals);
  end;
  if not (BatSet.is_empty errors) then begin
    printf "* Error states@\n";
    List.iter (print_symbolic_state "error" (ref 1) Format.std_formatter) (BatSet.to_list errors);
  end;
  if not (BatSet.is_empty failures) then begin
    printf "* Incomplete symbolic execution@\n";
    List.iter (print_symbolic_state "notcovered" (ref 1) Format.std_formatter) (BatSet.to_list failures);
  end;
  printf "* Summary@\n@\n";
  printf "- Success cases: %d@\n" (BatSet.cardinal normals);
  printf "- Error cases: %d@\n" (BatSet.cardinal errors);
  printf "- Incomplete symbolic execution: %d@\n" (BatSet.cardinal failures);
  (* Exit 1 if there is any error result *)
  if not (BatSet.is_empty errors) then
    exit 1;
  (* Exit 10 if there is any failure result *)
  if not (BatSet.is_empty failures) then
    exit 10;
  (* Exit 0, if there aren’t any errors or failures *)
  exit 0
