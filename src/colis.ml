open Format

module Internals = Colis_internals

module Language = struct
  module Nat = Syntax__Nat
  module Syntax = Syntax__Syntax
  module SyntaxHelpers = SyntaxHelpers
  module Parser = ColisParser
  module Lexer = ColisLexer
  module FromShell = FromShell
end

module Common = struct
  module Arguments = Semantics__Arguments
  module Behaviour = Semantics__Behaviour
  module Env = Env
  module Stdin = Semantics__Buffers.Stdin
  module Stdout = Semantics__Buffers.Stdout
  module Config = Semantics__Config
  module Input = Semantics__Input
end

module Concrete = struct
  module Filesystem = Interpreter__Filesystem
  module Interpreter = Interpreter__Interpreter
  module State = Interpreter__State
  module Semantics = Interpreter__Semantics
end

module Symbolic = struct
  module Filesystem = SymbolicInterpreter__Filesystem
  module FilesystemSpec = FilesystemSpec
  module Semantics = SymbolicInterpreter__Semantics
  module SymState = SymbolicInterpreter__SymState
  module Results = SymbolicInterpreter__Results
  module Interpreter = SymbolicInterpreter__Interpreter
  module Utility = SymbolicUtility

  let () =
    List.iter SymbolicUtility.register [
      (module Basics.True) ;
      (module Basics.Colon) ;
      (module Basics.False) ;
      (module Basics.Echo) ;
      (module Dpkg) ;
      (module DpkgMaintscriptHelper) ;
      (module Mv);
      (module Cp);
      (module Mkdir);
      (module Rm) ;
      (module Test) ;
      (module Test.Bracket) ;
      (module Touch) ;
      (module UpdateAlternatives) ;
      (module Which) ;
      (module Which.Silent) ;
      (* The Dark World *)
      (module ColisInternalUnsafeTouch)
    ]

  let add_fs_spec_to_clause root clause fs_spec =
    let fs_clause = FilesystemSpec.compile root fs_spec in
    Colis_constraints.Clause.add_to_sat_conj fs_clause clause

  let to_state ~prune_init_state ~root clause : Semantics.state =
    let root0 = if prune_init_state then None else Some root in
    let filesystem = {Filesystem.root; clause; root0} in
    {Semantics.filesystem; stdin=Common.Stdin.empty; stdout=Common.Stdout.empty; log=Common.Stdout.empty}

  let to_symbolic_state ~vars ~arguments state =
    let open Semantics in
    let context =
      let var_env = Semantics.add_var_bindings true vars Semantics.empty_var_env in
      {Semantics.empty_context with arguments; var_env; cwd=[]}
    in
    {SymState.state; context; data=()}

  let interp_program ~loop_limit ~stack_size ~argument0 stas' program =
    let open Common in
    let inp =
      let config =
        let open Config in
        let loop_limit = Finite (Z.of_int loop_limit) in
        let stack_size = Finite (Z.of_int stack_size) in
        { loop_limit; stack_size } in
      Input.({ argument0; config; under_condition=false }) in
    let normals, errors, failures = Interpreter.interp_program inp stas' program in
    normals, errors, failures
end

module Constraints = Colis_constraints

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
    raise (Internals.Errors.ParseError (s, pos))
  | ColisParser.Error ->
    let pos = lexbuf.Lexing.lex_curr_p in
    raise (Internals.Errors.ParseError ("", pos))

let parse_colis_channel ?(filename="-") channel =
  let lexbuf = Lexing.from_channel channel in
  parse_colis_lexbuf ~filename lexbuf

let parse_colis_file filename =
  let ic =
    try
      open_in filename
    with
      Sys_error msg -> raise (Internals.Errors.FileError msg)
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

let convert_shell_file = FromShell.program__to__program

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


let run ~argument0 ?(arguments=[]) ?(vars=[]) colis =
  let open Common in
  let open Concrete in
  let input =
    let config = Config.({loop_limit = Infinite; stack_size = Infinite }) in
    Input.({ argument0; config; under_condition=false }) in
  let state = Interpreter.empty_state () in
  state.arguments := arguments;
  state.var_env := Semantics.add_var_bindings true vars Semantics.empty_var_env;
  Interpreter.interp_program input state colis;
  print_string (Stdout.all_lines !(state.stdout) |> List.rev |> String.concat "\n");
  exit (if !(state.result) then 0 else 1)

let print_dot filename id clause =
  let ch = open_out filename in
  try
    let fmt = formatter_of_out_channel ch in
    Colis_constraints.Clause.pp_sat_conj_as_dot ~name:id fmt clause;
    close_out ch
  with e ->
    close_out ch;
    raise e

type symbolic_config = {
  prune_init_state: bool;
  loop_limit: int;
  stack_size: int;
}

let print_symbolic_filesystem fmt fs =
  let open Colis_constraints in
  let open Symbolic.Filesystem in
  fprintf fmt "root: %a@\n" Var.pp fs.root;
  fprintf fmt "clause: %a@\n" Clause.pp_sat_conj fs.clause

let print_symbolic_state fmt ?id sta =
  let open Symbolic.Semantics in
  begin match id with
    | Some id ->
      fprintf fmt "id: %s@\n" id;
      if !Colis_internals.Options.print_states_dir <> "" then
        let filename = sprintf "%s/%s.dot" !Colis_internals.Options.print_states_dir id in
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
  if not (Common.Stdout.is_empty sta.stdout) then begin
    fprintf fmt "stdout: |@\n";
    List.iter (fprintf fmt "  %s@\n")
      (List.rev @@ sta.stdout.lines);
    if sta.stdout.line <> "" then
      fprintf fmt "  %s@." sta.stdout.line
  end;
  (* Print log *)
  if not (Common.Stdout.is_empty sta.log) then begin
    fprintf fmt "log: |@\n";
    List.iter (fprintf fmt "  %s@\n")
      (List.rev @@ sta.log.lines);
    if sta.log.line <> "" then
      fprintf fmt "  %s" sta.log.line
  end

let print_symbolic_state_with_ctr label ctr fmt sta =
  let id = sprintf "%s-%d" label !ctr in
  incr ctr;
  fprintf fmt "- @[%a@]@\n" (print_symbolic_state ~id) sta

let print_symbolic_states ~initials (normals, errors, failures) =
    printf "* Initial states@\n";
  List.iter (print_symbolic_state_with_ctr "initial" (ref 1) Format.std_formatter) initials;
  if normals <> [] then begin
    printf "* Success states@\n";
    List.iter (print_symbolic_state_with_ctr "success" (ref 1) Format.std_formatter) normals;
  end;
  if errors <> [] then begin
    printf "* Error states@\n";
    List.iter (print_symbolic_state_with_ctr "error" (ref 1) Format.std_formatter) errors;
  end;
  if failures <> [] then begin
    printf "* Incomplete symbolic execution@\n";
    List.iter (print_symbolic_state_with_ctr "notcovered" (ref 1) Format.std_formatter) failures;
  end;
  printf "* Summary@\n@\n";
  printf "- Success cases: %d@\n" (List.length normals);
  printf "- Error cases: %d@\n" (List.length errors);
  printf "- Incomplete symbolic execution: %d@\n" (List.length failures)

let exit_code (_, errors, failures) =
  (* Exit 1 if there is any error result *)
  if errors <> [] then
    1
    (* Exit 10 if there is any failure result *)
  else if failures <> [] then
    10
  else (* Exit 0, if there aren’t any errors or failures *)
    exit 0

let run_symbolic config fs_spec ~argument0 ?(arguments=[]) ?(vars=[]) colis =
  let open Symbolic in
  let root = Colis_constraints.Var.fresh ~hint:"r" () in
  let disj = add_fs_spec_to_clause root Colis_constraints.Clause.true_sat_conj fs_spec in
  let stas = List.map (to_state ~prune_init_state:config.prune_init_state ~root) disj in
  let stas' = List.map (to_symbolic_state ~vars ~arguments) stas in
  let results =
    interp_program ~loop_limit:config.loop_limit ~stack_size:config.stack_size ~argument0
      stas' colis
  in
  print_symbolic_states ~initials:stas results;
  exit (exit_code results)
