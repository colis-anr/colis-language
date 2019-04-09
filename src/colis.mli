(** {2 CoLiS-Language} *)

module Errors = Errors
module Options = Options

module Language : sig
  module Nat = Syntax__Nat
  module Syntax = Syntax__Syntax
  module Parser = ColisParser
  module Lexer = ColisLexer
  module FromShell = FromShell
end

module Semantics : sig
  module Arguments = Semantics__Arguments
  module Behaviour = Semantics__Behaviour
  module Env = Env
  module Stdin = Semantics__Buffers.Stdin
  module Stdout = Semantics__Buffers.Stdout
  module Context = Semantics__Context
  module Input = Semantics__Input
end

module Concrete : sig
  module Filesystem = Interpreter__Filesystem
  module Interpreter = Interpreter__Interpreter
  module State = Interpreter__State
end

module Symbolic : sig
  module Filesystem = SymbolicInterpreter__Filesystem
  module FilesystemSpec = FilesystemSpec
  module State = SymbolicInterpreter__Semantics (* Semantics contais State *)
  module SymState = SymbolicInterpreter__SymState
  module Results = SymbolicInterpreter__Results
  module Interpreter = SymbolicInterpreter__Interpreter
  module Utilities = SymbolicUtilities

  open Constraints

  (** Create a disjunction representing the FS *)
  val compile_fs_spec : root:Var.t -> FilesystemSpec.t -> NaiveClause.sat_conj list

  (* Create a state corresponding to a conjunction *)
  val to_state : prune_init_state:bool -> root:Var.t -> NaiveClause.sat_conj -> State.state

  (* Create a symbolic states by adding context to a stringe *)
  val to_symbolic_state : vars:(string * string) list -> arguments:string list -> State.state -> unit SymState.sym_state

  (* Wrapper around [Symbolic.Interpreter.interp_program] *)
  val interp_program : loop_limit:int -> stack_size:int -> argument0:string -> unit SymState.sym_state list -> Language.Syntax.program -> (State.state list * State.state list * State.state list)
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

val run : argument0:string -> ?arguments:(string list) -> ?vars:((string * string) list) -> colis -> unit
(** Runs a Colis program.

    @param argument0 Value for argument zero (the interpreter or filename)
    @param arguments Other arguments
  *)

type symbolic_config = {
  prune_init_state: bool;
  (** Prune the initial symbolic state during symbolic execution for a faster execution *)
  loop_limit: int;
  (** Maximum number of iterations of while loops in symbolic execution *)
  stack_size: int;
  (** Maximum height of the call stack in symbolic execution *)
}

open Symbolic

val print_symbolic_states : initials:State.state list -> (State.state list * State.state list * State.state list) -> unit

val exit_code : (State.state list * State.state list * State.state list) -> int

val run_symbolic : symbolic_config -> Symbolic.FilesystemSpec.t -> argument0:string -> ?arguments:(string list) -> ?vars:((string * string) list) -> colis -> unit
(** Symbolically executes a Colis program. *)
