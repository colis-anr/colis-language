open Semantics__Buffers
open Semantics__Context

let interp_builtin : state -> string -> string list -> (state * stdout * bool) =
  fun sta name args ->
    match name with
    | "echo" ->
      let str = String.concat " " args in
      let out = empty_stdout |> output str |> newline in
      sta, out, true
    | "true" ->
      sta, empty_stdout, true
    | "false" ->
      sta, empty_stdout, false
    | _ ->
      let str = name^": command not found" in
      let out = empty_stdout |> output str |> newline in
      sta, out, false
