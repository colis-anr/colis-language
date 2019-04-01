(* module for handling Contents files *)

type t
(* type representing a table associating to a package name a list of
   file names. *)

val newtable: unit -> t

val scan: string -> t -> unit
(* [scanfile filename table] scans the Contents file with name [filename],
   and adds its contents to [table].
 *)

val print: t -> unit
 (* print [t] dumps [t] to [stdout] *)

val save: string -> t -> unit
  (* save [t] to the file [filename] *)

val load: string -> t
  (* load a contentstable from file [filename] *)
