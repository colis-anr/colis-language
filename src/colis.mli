(** {1 CoLiS-Language} *)

module AST = Syntax__Syntax

type colis = AST.program
(** The abstract syntax of Colis programs. *)

type shell = Morsmall.AST.program
(** The abstract syntax of Shell programs, taken from Morsmall. *)

(** {2 Parsing} *)

val parse_colis : string -> colis
(** Reads a Colis file and returns the corresponding AST. *)

val parse_shell : string -> shell
(** Reads a Shell file and returns the corresponding AST. *)

(** {2 Printing} *)

val print_colis : colis -> unit
(** Prints a Colis program to stdout. *)

val colis_to_string : colis -> string
(** Prints a Colis program to a string. *)

val colis_to_file : string -> colis -> unit
(** Prints a Colis program to a file. *)

val pp_print_colis : Format.formatter -> colis -> unit
(** Generic pretty-printing function for Colis. *)

(** {2 Converting} *)

val shell_to_colis : shell -> colis
(** Converts a Shell program to a Colis program. *)

(** {2 Interpreting} *)

val run : ?arguments:(string list) -> colis -> unit
(** Runs a Colis program. *)
