(** {2 CoLiS-Language} *)

module Errors = Errors
module Options = Options

module Language: sig
  module Nat = Syntax__Nat
  module Syntax = Syntax__Syntax
  module Parser = ColisParser
  module Lexer = ColisLexer
  module FromShell = FromShell
end

module Concrete: sig
  module Arguments = Semantics__Arguments
  module Behaviour = Semantics__Behaviour
  module Context = Semantics__Context
  module Stdin = Semantics__Buffers.Stdin
  module Stdout = Semantics__Buffers.Stdout
  module Env = Semantics__Env
  module Input = Semantics__Input
  module Semantics = Semantics__Semantics
  module Filesystem = Interpreter__Filesystem
  module Interpreter = Interpreter__Interpreter
end

module Symbolic: sig
  module Context = SymbolicInterpreter__Context
  module Filesystem = SymbolicInterpreter__Filesystem
  module FilesystemSpec = FilesystemSpec
  module State = SymbolicInterpreter__State
  module SymState = SymbolicInterpreter__SymState
  module Results = SymbolicInterpreter__Results
  module Interpreter = SymbolicInterpreter__Interpreter
  module Utilities = SymbolicUtilities
end

type colis = Language.Syntax.program
(** The abstract syntax of Colis programs. *)

(** {2 Parsing} *)

val parse_colis_channel : ?filename:string -> in_channel -> colis
(** Reads Colis syntax from a channel and returns the corresponding AST.

    @raise {!Errors.ParseError} *)

val parse_colis_file : string -> colis
(** Reads Colis syntax from a file and returns the corresponding AST.

    @raise {!Errors.ParseError} *)

val parse_colis_string : string -> colis
(** Reads Colis syntax from a string and returns the corresponding AST.

    @raise {!Errors.ParseError} *)

val parse_shell_file : string -> colis
(** Reads Shell from a file, converts it to Colis and returns the
   corresponding AST.

    @raise {!Errors.ParseError}
    @raise {!Errors.ConversionError} *)

(** {2 Printing} *)

val print_colis : colis -> unit
(** Prints a Colis program to stdout. *)

val colis_to_string : colis -> string
(** Prints a Colis program to a string. *)

val colis_to_file : string -> colis -> unit
(** Prints a Colis program to a file. *)

val pp_print_colis : Format.formatter -> colis -> unit
(** Generic pretty-printing function for Colis. *)

(** {2 Interpreting} *)

val run : argument0:string -> ?arguments:(string list) -> colis -> unit
(** Runs a Colis program.

    @param argument0 Value for argument zero (the interpreter or filename)
    @param arguments Other arguments
  *)

val run_symbolic : prune_init_state:bool -> loop_limit:int -> stack_size:int -> fs_spec:Symbolic.FilesystemSpec.t -> argument0:string -> ?arguments:(string list) -> colis -> unit
(** Symbolically executes a Colis program. *)
