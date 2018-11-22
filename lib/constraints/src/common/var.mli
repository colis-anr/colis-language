type t

val fresh :
  ?hint:string -> ?hintf:Feat.t -> ?hintv:t -> ?hintp:Path.t ->
  unit -> t

val eq : t -> t -> bool

module Set : (Set.S with type elt = t)
module Map : (Map.S with type key = t)
