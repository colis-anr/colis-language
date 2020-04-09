open Format

module Internals = Colis_internals

type colis = Syntax__Syntax.program

module Language = struct
  module Nat = Syntax__Nat
  module Syntax = Syntax__Syntax
  module SyntaxHelpers = SyntaxHelpers
  module Parser = ColisParser
  module Lexer = ColisLexer
  module FromShell = FromShell
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

  let embellish_colis = Embellisher.embellish
end

module Common = struct
  module Arguments = Semantics__Arguments
  module Behaviour = Semantics__Behaviour
  module Config = Semantics__Config
  module Context = Semantics__Context
  module Env = Semantics__Env
  module Input = Semantics__Input
  module InterpUtilitySpec = Semantics__InterpUtilitySpec
  module Path = Semantics__Path
  module Result = Semantics__Result
  module Stdin = Semantics__Buffers.Stdin
  module Stdout = Semantics__Buffers.Stdout
end

module Concrete = struct
  module Filesystem = Interpreter__Filesystem
  module Interpreter = Interpreter__Interpreter
  module State = Interpreter__State
  module Semantics = Interpreter__Semantics

  (* Interpret *)

  let run ~argument0 ?(arguments=[]) ?(vars=[]) colis =
    let open Common in
    let open Context in
    let input =
      let config = Config.({loop_limit = Infinite; stack_size = Infinite }) in
      Input.({ argument0; config; under_condition=false }) in
    let state = Interpreter.empty_state () in
    state.arguments := arguments;
    state.var_env := add_var_bindings true vars empty_var_env;
    try
      Interpreter.interp_program input state colis;
      print_string (Stdout.all_lines !(state.stdout) |> List.rev |> String.concat "\n");
      exit (if !(state.result) then 0 else 1)
    with Interpreter.EIncomplete ->
      print_string (Stdout.all_lines !(state.stdout) |> List.rev |> String.concat "\n");
      exit 7
end

module FilesystemSpec = FilesystemSpec

module Constraints = Colis_constraints

let () =
  (* Register the utilities to the mixed backend, which is used in interpreting the
     interfaces for constraints and transducers equally *)
  List.iter SymbolicUtility.Mixed.register [
    (module Basics.True) ;
    (module Basics.Colon) ;
    (module Basics.False) ;
    (module Basics.Echo) ;
    (module Cp);
    (module Dpkg) ;
    (module DpkgMaintscriptHelper) ;
    (module EmacsPackage.Install) ;
    (module EmacsPackage.Remove) ;
    (module Mkdir);
    (module Mv);
    (module Rm) ;
    (module Test) ;
    (module Test.Bracket) ;
    (module Touch) ;
    (module UpdateAlternatives) ;
    (module UpdateMenus) ;
    (module Which) ;
    (module Which.Silent) ;
    (* The Dark World *)
    (module ColisInternalUnsafeTouch) ;
  ]

let mk_config ~loop_limit ~stack_size =
  let open Common.Config in
  let loop_limit = Finite (Z.of_int loop_limit) in
  let stack_size = Finite (Z.of_int stack_size) in
  { loop_limit; stack_size }

let mk_context ~arguments ~vars =
  let open Common.Context in
  let var_env = add_var_bindings true vars empty_var_env in
  {empty_context with arguments; var_env}

let exit_code (_, errors, failures) =
  (* Exit 1 if there is any error result *)
  if errors <> [] then
    1
    (* Exit 10 if there is any failure result *)
  else if failures <> [] then
    10
  else (* Exit 0, if there aren’t any errors or failures *)
    exit 0

type sym_config = {
  loop_limit: int;
  stack_size: int;
  filesystem_spec : FilesystemSpec.t;
}

module SymbolicConstraints = struct

  type state = SymbolicUtility.Constraints.state
  type sym_state = SymbolicUtility.Constraints.sym_state
  type config = SymbolicUtility.Constraints.config = {
    prune_init_state: bool;
  }

  let is_registered = SymbolicUtility.Mixed.is_registered

  let interp_program ~loop_limit ~stack_size ~argument0 stas' program =
    let open Common in
    let config = mk_config ~loop_limit ~stack_size in
    let inp = Input.{ config; argument0; under_condition=false } in
    SymbolicUtility.Mixed.interp_program_constraints inp stas' program

  let print_dot filename id clause =
    let ch = open_out filename in
    try
      let fmt = formatter_of_out_channel ch in
      Constraints.Clause.pp_sat_conj_as_dot ~name:id fmt clause;
      close_out ch
    with e ->
      close_out ch;
      raise e

  let print_filesystem fmt fs =
    let open Constraints in
    let open SymbolicUtility.Constraints in
    fprintf fmt "root: %a@\n" Var.pp fs.root;
    fprintf fmt "clause: %a@\n" Clause.pp_sat_conj fs.clause

  let print_state fmt ?id sta =
    let open SymbolicUtility in
    begin match id with
      | Some id ->
        fprintf fmt "id: %s@\n" id;
        if !Colis_internals.Options.print_states_dir <> "" then
          let filename = sprintf "%s/%s.dot" !Colis_internals.Options.print_states_dir id in
          print_dot filename id sta.Constraints.filesystem.clause;
      | None -> ()
    end;
    print_filesystem fmt sta.Constraints.filesystem;
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

  let print_state_with_ctr label ctr fmt sta =
    let id = sprintf "%s-%d" label !ctr in
    incr ctr;
    fprintf fmt "- @[%a@]@\n" (print_state ~id) sta

  let print_states ~initials (normals, errors, failures) =
    printf "* Initial states@\n";
    List.iter (print_state_with_ctr "initial" (ref 1) Format.std_formatter) initials;
    if normals <> [] then begin
      printf "* Normal states@\n";
      List.iter (print_state_with_ctr "normal" (ref 1) Format.std_formatter) normals;
    end;
    if errors <> [] then begin
      printf "* Error states@\n";
      List.iter (print_state_with_ctr "error" (ref 1) Format.std_formatter) errors;
    end;
    if failures <> [] then begin
      printf "* Incomplete symbolic execution@\n";
      List.iter (print_state_with_ctr "incomplete" (ref 1) Format.std_formatter) failures;
    end;
    printf "* Summary@\n@\n";
    printf "- Success cases: %d@\n" (List.length normals);
    printf "- Error cases: %d@\n" (List.length errors);
    printf "- Incomplete symbolic execution: %d@\n" (List.length failures)

 let run sym_config config ~argument0 ?(arguments=[]) ?(vars=[]) colis =
    let open SymbolicUtility in
    let context = mk_context ~arguments ~vars in
    let filesystems = Constraints.filesystems config sym_config.filesystem_spec in
    let stas = List.map Constraints.mk_state filesystems in
    let sym_stas = List.map (fun state -> Constraints.{ state; context }) stas in
    let results =
      interp_program ~loop_limit:sym_config.loop_limit ~stack_size:sym_config.stack_size ~argument0
        sym_stas colis
    in
    print_states ~initials:stas results;
    exit_code results

  (** {3 For colis-batch} *)

  let add_fs_spec_to_clause root clause fs_spec =
    let fs_clause = FilesystemSpec.compile_constraints root fs_spec in
    Constraints.Clause.add_to_sat_conj fs_clause clause

  let to_state ~prune_init_state ~root clause : state =
    let open SymbolicUtility.Constraints in
    let root0 = if prune_init_state then None else Some root in
    let filesystem =
      {root; clause; root0} in
    mk_state filesystem

  let to_symbolic_state ~vars ~arguments state =
    SymbolicUtility.Constraints.{state; context = mk_context ~arguments ~vars}
end

module SymbolicTransducers = struct
  type config = SymbolicUtility.Transducers.config

  let run sym_config config ~argument0 ?(arguments=[]) ?(vars=[]) colis =
      let open SymbolicUtility in
      let open Common in
      let inp =
        let config = mk_config ~loop_limit:sym_config.loop_limit ~stack_size:sym_config.stack_size in
        Input.{ config; argument0; under_condition=false } in
      let sym_stas =
        let context = mk_context ~arguments ~vars in
        let stas =
          let fs = Transducers.filesystems config sym_config.filesystem_spec in
          List.map Transducers.mk_state fs in
        List.map (fun state -> Transducers.{ state; context }) stas in
      let results = SymbolicUtility.Mixed.interp_program_transducers inp sym_stas colis in
      exit_code results
end
