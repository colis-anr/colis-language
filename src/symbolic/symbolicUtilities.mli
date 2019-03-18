open Env

(** [interp name var_env args] *)
val interp : string -> string env -> string list -> UtilitiesSpecification.utility (*TODO name arguments *)
