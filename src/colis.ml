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
  module FilesystemSpec = FilesystemSpec
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

let _print_dot filename id clause =
  let ch = open_out filename in
  try
    let fmt = formatter_of_out_channel ch in
    Constraints.Conj.pp_as_dot ~name:id fmt clause;
    close_out ch
  with e ->
    close_out ch;
    raise e

let print_symbolic_state _id fmt sta =
  let open Symbolic.State in
  print_symbolic_filesystem fmt sta.filesystem;
  (* print_dot (sprintf "/tmp/%s.dot" id) id sta.filesystem.clause; *)
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

let run_symbolic ~argument0 ?(arguments=[]) fs_spec colis =
  let open Symbolic in
  let states : State.state list =
    let open Constraints in
    let root = Var.fresh ~hint:"r" () in
    let clause = FilesystemSpec.compile ~root fs_spec in
    let cwd = Path.Abs [] in
    let root0 = Some root in (* [None] for pruning [root] while symbolic execution, [Some root] to prevent pruning the initial state but slower execution *)
    Clause.(add_to_sat_conj clause true_) |>
    List.map (fun c -> {Filesystem.clause=c; root; root0; cwd}) |>
    List.map
      (fun filesystem -> {
           State.filesystem;
           stdin = Concrete.Stdin.empty;
           stdout = Concrete.Stdout.empty;
         })
  in
  let print_state_record ctr label fmt =
    let id : string = sprintf "%s-%d" label !ctr in
    incr ctr; fprintf fmt "@\n- @[id: %s@\n%a@]" id (print_symbolic_state id)
  in
  List.iter
    (fun state ->
       let input = { Concrete.Input.empty with argument0 } in
       let context = { Context.empty_context with arguments } in
       let normals, failures = Interpreter.interp_program input context state colis in
       printf "* Initial state@\n";
       printf "%a@\n" (print_state_record (ref 0) "init") state;
       printf "* Success states@\n";
       List.iter (print_state_record (ref 1) "success" Format.std_formatter) (BatSet.to_list normals);
       printf "@\n";
       printf "* Failure states@\n";
       List.iter (print_state_record (ref 1) "failure" Format.std_formatter) (BatSet.to_list failures);
       printf "@\n";
       printf "* Summary@\n@\n";
       printf "- Success cases: %d@\n" (BatSet.cardinal normals);
       printf "- Error cases: %d@\n" (BatSet.cardinal failures))
    states;
