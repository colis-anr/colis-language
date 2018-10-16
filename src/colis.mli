(** {1 CoLiS-Language} *)

module AST = Syntax__Syntax
module ColisParser = ColisParser
module ColisLexer = ColisLexer
module FromShell = FromShell

type colis = AST.program
(** The abstract syntax of Colis programs. *)

type shell = Morsmall.AST.program
(** The abstract syntax of Shell programs, taken from Morsmall. *)

(** {2 Parsing} *)

val colis_from_channel : ?filename:string -> in_channel -> colis
(** Reads Colis syntax from a channel and returns the corresponding AST.

    @raises {!ColisLexer.LexerError}
    @raises {!ColisParser.Error} *)

val colis_from_file : string -> colis
(** Reads Colis syntax from a file and returns the corresponding AST.

    @raises {!ColisLexer.LexerError}
    @raises {!ColisParser.Error} *)

val colis_from_string : string -> colis
(** Reads Colis syntax from a string and returns the corresponding AST.

    @raises {!ColisLexer.LexerError}
    @raises {!ColisParser.Error} *)

val shell_from_file : string -> shell
(** Reads a Shell file and returns the corresponding AST. This is a
    wrapper around Morsmall.parse_file.

    @raises Morsmall.SyntaxError *)

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
(** Converts a Shell program to a Colis program.

    @raises {!FromShell.Unsupported} *)

(** {2 Interpreting} *)

val run : ?arguments:(string list) -> colis -> unit
(** Runs a Colis program. *)
