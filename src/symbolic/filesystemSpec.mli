(** Simple specification of filessytems (with a translation to feature constraints) *)

type spec = node list
and node

val node : string -> spec -> node

(** Compile the specification of a filesystem into a feature constraint. Returns the
    root variable and a constraint that encodes the filesystem *)
val compile : root:Constraints.Var.t -> spec -> Constraints.Clause.t

(** Empty filesystem (root directory only) *)
val empty : spec

(** A simple filesystem (including some but not all directories from the FHS) *)
val simple : spec

(** Filesystem with directory structor following the Filesystem Hierarchy Standard *)
val fsh : spec
