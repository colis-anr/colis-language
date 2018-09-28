open Semantics__Buffers
open Semantics__Context

let interp_builtin : state -> string -> string list -> (state * stdout * bool) =
  fun state name args ->
    failwith "interp_builtin"
