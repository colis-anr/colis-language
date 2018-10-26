type t

val fresh : ?hint:string -> ?hintf:Feat.t -> ?hintv:t -> unit -> t
