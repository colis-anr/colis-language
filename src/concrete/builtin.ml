open Semantics__Buffers
open Semantics__Context

let interp_builtin : state -> string -> string list -> (state * stdout * bool) =
  fun sta name args ->
    match name with
    | "echo" ->
      let str = String.concat " " args in
      let out = empty_stdout |> output str |> newline in
      sta, out, true
    | _ -> failwith ("interp_builtin: " ^ name)
