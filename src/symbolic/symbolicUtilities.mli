(** The concrete evaluation context. It contains the fields from
   Colis.Semantics.Context.context that are relevant to the utilities **)
type context = {
  args: string list; (** Command arguments *)
  cwd: Constraints.Path.t; (** Current working directory *)
  env: string Env.IdMap.t; (** Variable environment *)
}

(** Entry-point for the interpretation of symbolic utilties *)
val interp : name:string -> context -> UtilitiesSpecification.utility
