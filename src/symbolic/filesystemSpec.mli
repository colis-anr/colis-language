(** Simple specification of filessytems (with a translation to feature constraints) *)

type t = node list
and node

val node : string -> t -> node

(** Compile the specification of a filesystem into a feature constraint. Returns the
    root variable and a constraint that encodes the filesystem *)
val compile : root:Constraints.Var.t -> t -> Constraints.Clause.t

(** Empty filesystem (only root directory) *)
val empty : t

(** A simple filesystem (including some but not all directories from the FHS)

    Specified directories:

    {v
    /etc
    /var/lib
    /usr/lib
    /usr/local/lib
    v} *)
val simple : t

(** Filesystem with directory structor following the
   {{:https://refspecs.linuxfoundation.org/FHS_3.0/fhs-3.0.html:}Filesystem Hierarchy
   Standard}.

    Incomplete. *)
val fsh : t
