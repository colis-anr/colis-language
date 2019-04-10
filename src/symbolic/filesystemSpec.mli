open Constraints

(** Simple specification of filessytems (with a translation to feature constraints) *)
type t

(** Empty filesystem (i.e., only the root directory) *)
val empty : t

(** Add a directory to a FS spec at the given relative path to root, defined by a list of
   feature names.

    @raise Invalid_argument if any super-path is already specified as a file. *)
val add_dir : string list -> t -> t

(** Add a file to a FS spec at the given relative path to root.

    @raise Invalid_argument if any super-path of the parent is specified as a file or the
    path is already specified as a directory. *)
val add_file : string list -> t -> t

(** Adds files and directories from a channel. One line per entry, all lines start with
    '/' and directories end with '/'

    @raise Invalid_argument like functions [add_dir] and [add_file ]*)
val add_channel : in_channel -> t -> t

(** Compile the specification of a filesystem into a feature constraint at the given root
    variable.*)
val compile : Var.t -> t -> Clause.t

(** Print the FS spec as a tree *)
val pp : Format.formatter -> t -> unit
