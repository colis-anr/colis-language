
type env = (string * string) list

type args = string list

val interp : string -> env -> args -> UtilitiesSpecification.utility
