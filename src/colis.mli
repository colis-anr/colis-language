(** {1 CoLiS-Language} *)

module AST = Syntax__Syntax

type colis = AST.program
type shell = Morsmall.AST.program

(** {2 Parsing} *)

val parse_colis : string -> colis
val parse_shell : string -> shell

(** {2 Printing} *)

val pp_print_colis : Format.formatter -> colis -> unit
val print_colis : colis -> unit
val colis_to_string : colis -> string
val colis_to_file : string -> colis -> unit

(** {2 Converting} *)

val shell_to_colis : shell -> colis

(** {2 Interpreting} *)

val run : ?arguments:(string list) -> colis -> unit
