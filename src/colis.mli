(** {2 CoLiS-Language} *)

module Internals = Colis_internals
module SymbolicUtility = SymbolicUtility

(** The abstract syntax of CoLiS programs. *)
type colis = Syntax__Syntax.program

(** The CoLiS language *)
module Language : sig
  module Nat = Syntax__Nat
  module Syntax = Syntax__Syntax
  module SyntaxHelpers = SyntaxHelpers
  module Parser = ColisParser
  module Lexer = ColisLexer
  module FromShell = FromShell

  (** {2 Printing} *)

  val print_colis : colis -> unit
  (** Prints a Colis program to stdout. *)

  val colis_to_string : colis -> string
  (** Prints a Colis program to a string. *)

  val colis_to_file : string -> colis -> unit
  (** Prints a Colis program to a file. *)

  val pp_print_colis : Format.formatter -> colis -> unit
  (** Generic pretty-printing function for Colis. *)

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

  val convert_shell_file : cmd_line_arguments:string list -> Morsmall.AST.program -> colis
  (** Converts the given Shell script to Colis and return the corresponding AST.

      @raise {!Errors.ConversionError} *)

  val parse_shell_file : cmd_line_arguments:string list -> string -> colis
  (** Reads Shell from a file, converts it to Colis and returns the
      corresponding AST.

      @raise {!Errors.ParseError}
      @raise {!Errors.ConversionError} *)

  val embellish_colis : colis -> colis

end

(** Modules shared between the different interpreters *)
module Common : sig
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

(** The concrete interpreter *)
module Concrete : sig
  module Filesystem = Interpreter__Filesystem
  module Interpreter = Interpreter__Interpreter
  module State = Interpreter__State
  module Semantics = Interpreter__Semantics

  (** {2 Interpreting} *)

  val run : argument0:string -> ?arguments:(string list) -> ?vars:((string * string) list) -> colis -> unit
  (** Runs a Colis program.

      @param argument0 Value for argument zero (the interpreter or filename)
      @param arguments Other arguments
  *)
end

(** Specification of the initial filesystem *)
module FilesystemSpec = FilesystemSpec

module Constraints = Colis_constraints

(** {1 The interpreters} *)

type sym_config = {
  loop_limit: int; (** Maximum number of iterations of while loops in symbolic execution *)
  stack_size: int; (** Maximum height of the call stack in symbolic execution *)
  filesystem_spec : FilesystemSpec.t; (** Specification of the initial filesystem *)
}

(** The symbolic interpreter using constraints on the mixed backend of SymbolicUtility *)
module SymbolicConstraints : sig
  open Constraints

  type state = SymbolicUtility.Constraints.state
  type sym_state = SymbolicUtility.Constraints.sym_state
  type config = SymbolicUtility.Constraints.config = {
    prune_init_state: bool; (** Prune the initial symbolic state during symbolic execution for a faster execution *)
  }

  (** Test if an utility is registerered in the mixed backend (the actual backend for this
      module) *)
  val is_registered : name:string -> bool

  (* Wrapper around [SymbolicUtility.Mixed.interp_program] (sic!) *)
  val interp_program : loop_limit:int -> stack_size:int -> argument0:string -> sym_state list -> Language.Syntax.program -> (state list * state list * state list)

  val run : config -> sym_config -> argument0:string -> ?arguments:(string list) -> ?vars:((string * string) list) -> colis -> unit

  val print_state : Format.formatter -> ?id:string -> state -> unit

  val print_states : initials:state list -> (state list * state list * state list) -> unit

  (** {3 For colis-batch} *)

  (** [compile_fs_spec root conj fs_spec] creates a disjunction that represents the conjunction [conj] with constraints representing the filesystem specified by [fs_spec] *)
  val add_fs_spec_to_clause : Var.t -> Clause.sat_conj -> FilesystemSpec.t -> Clause.sat_conj list

  (* Create a state corresponding to a conjunction *)
  val to_state : prune_init_state:bool -> root:Var.t -> Clause.sat_conj -> state

  (* Create a symbolic states by adding context to a stringe *)
  val to_symbolic_state : vars:(string * string) list -> arguments:string list -> state -> sym_state
end

(** The symbolic interpreter using transducers *)
module SymbolicTransducers : sig
  type config = SymbolicUtility.Transducers.config
  val run : config -> sym_config -> argument0:string -> ?arguments:(string list) -> ?vars:((string * string) list) -> colis -> unit
end
