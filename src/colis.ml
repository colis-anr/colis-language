open Format

module Language = struct
  module Nat = Syntax__Nat
  module Syntax = Syntax__Syntax
  module Parser = ColisParser
  module Lexer = ColisLexer
  module FromShell = FromShell
end

module Concrete = struct
  module Arguments = Semantics__Arguments
  module Behaviour = Semantics__Behaviour
  module Stdin = Semantics__Buffers.Stdin
  module Stdout = Semantics__Buffers.Stdout
  module Context = Semantics__Context
  module Env = Semantics__Env
  module Input = Semantics__Input
  module Semantics = Semantics__Semantics
  module Filesystem = Interpreter__Filesystem
  module Interpreter = Interpreter__Interpreter
end

module Symbolic = struct
  module Context = SymbolicInterpreter__Context
  module Filesystem = SymbolicInterpreter__Filesystem
  module State = SymbolicInterpreter__State
  module SymState = SymbolicInterpreter__SymState
  module Results = SymbolicInterpreter__Results
  module Interpreter = SymbolicInterpreter__Interpreter
end

(* CoLiS *)

exception ParseError of string * Lexing.position
exception ConversionError of string

type colis = Language.Syntax.program

let colis_from_lexbuf ?(filename="-") lexbuf =
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p
      with Lexing.pos_fname = filename };
  try
    ColisParser.program ColisLexer.token lexbuf
  with
  | ColisLexer.LexerError s ->
    let pos = lexbuf.Lexing.lex_curr_p in
    raise (ParseError (s, pos))
  | ColisParser.Error ->
    let pos = lexbuf.Lexing.lex_curr_p in
    raise (ParseError ("", pos))

let colis_from_channel ?(filename="-") channel =
  let lexbuf = Lexing.from_channel channel in
  colis_from_lexbuf ~filename lexbuf

let colis_from_file filename =
  let ic = open_in filename in
  try
    let colis = colis_from_channel ~filename ic in
    close_in ic;
    colis
  with
    exn -> close_in ic; raise exn

let colis_from_string string =
  let lexbuf = Lexing.from_string string in
  colis_from_lexbuf lexbuf

let pp_print_colis = ToColis.program

let print_colis =
  pp_print_colis (Format.formatter_of_out_channel stdout)

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

(* Shell *)

type shell = Morsmall.AST.program

let shell_from_file file =
  try
    Morsmall.parse_file file
  with
    Morsmall.SyntaxError pos ->
    raise (ParseError ("", pos))

let shell_to_colis shell =
  try
    FromShell.program__to__program shell
  with
    FromShell.Unsupported feat ->
    raise (ConversionError ("unsupported feature: " ^ feat))

(* Interpret *)

let run ~argument0 ?(arguments=[]) colis =
  let open Concrete in
  let input = { Input.empty with argument0 } in
  let state = Interpreter.empty_state () in
  state.arguments := arguments;
  Interpreter.interp_program input state colis;
  print_string (Stdout.all_lines !(state.stdout) |> List.rev |> String.concat "\n");
  exit (if !(state.result) then 0 else 1)

let print_symbolic_filesystem fmt fs =
  let open Constraints in
  let open Symbolic.Filesystem in
  fprintf fmt "root: %a@\n" Var.pp fs.root;
  fprintf fmt "cwd: %a@\n" Path.pp fs.cwd;
  fprintf fmt "clause: %a@\n" Clause.pp_sat_conj fs.clause

let print_symbolic_state fmt sta =
  let open Symbolic.State in
  print_symbolic_filesystem fmt sta.filesystem;
  (* Print stdin *)
  if sta.stdin <> [] then begin
    fprintf fmt "stdin: |@\n";
    List.iter (fprintf fmt "  %s@\n")
      (List.rev sta.stdin)
  end;
  (* Print stdout *)
  if not (Concrete.Stdout.is_empty sta.stdout) then begin
    fprintf fmt "stdout: |@\n";
    List.iter (fprintf fmt "  %s@\n")
      (List.rev @@ sta.stdout.lines);
    fprintf fmt "  %s" sta.stdout.line
  end

let run_symbolic ~argument0 ?(arguments=[]) colis =
  let stdin = Concrete.Stdin.empty in
  let stdout = Concrete.Stdout.empty in
  let input = { Concrete.Input.empty with argument0 } in
  let open Symbolic in
  let context = { Context.empty_context with arguments } in
  let states : State.state list =
    let open Constraints in
    let root = Var.fresh ~hint:"root" () in
    let cwd = Path.Abs [] in
    let open Clause in
    let spec = dir root in
    add_to_sat_conj spec true_ |>
    List.map (fun clause -> { Filesystem.root; clause; cwd }) |>
    List.map (fun filesystem -> { State.filesystem; stdin; stdout })
  in
  List.iter
    (fun state ->
       let normals, failures = Interpreter.interp_program input context state colis in
       printf "* Initial state@\n";
       printf "@\n- @[%a@]@\n" print_symbolic_state state;
       printf "* Success states@\n";
       List.iter (printf "@\n- @[%a@]" print_symbolic_state) (BatSet.to_list normals);
       printf "@\n";
       printf "* Failure states@\n";
       List.iter (printf "@\n- @[%a@]" print_symbolic_state) (BatSet.to_list failures);
       printf "@\n";
       printf "* Summary@\n@\n";
       printf "- Success cases: %d@\n" (BatSet.cardinal normals);
       printf "- Error cases: %d@\n" (BatSet.cardinal failures))
    states;
