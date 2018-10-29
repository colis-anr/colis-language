type t

val fresh :
  ?hint:string -> ?hintf:Feat.t -> ?hintv:t -> ?hintp:Path.t ->
  unit -> t
