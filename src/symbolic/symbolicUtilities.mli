type env = string Env.IdMap.t

(** [interp name var_env args] *)
val interp : string -> env -> string list -> UtilitiesSpecification.utility (*TODO name arguments *)
