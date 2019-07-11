(** {1 Core} *)

open Constraints_common

(** {2 Information About a Variable} *)

type info
(** Type of the information we have on a variable. *)

(** {2 Main Structure} *)

type var

type t
(** Type of an irreducible existential clause. *)

val get_info : var -> t -> info
