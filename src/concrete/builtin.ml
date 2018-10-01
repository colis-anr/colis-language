open Semantics__Buffers
open Semantics__Context
open Mystring__String

let interp_builtin : state -> string -> string list -> (state * stdout * bool) =
  fun sta name args ->
    match name with
    | "echo-n" ->
      let str = concat args in
      let out = output stdout empty_stdout |> newline in
      sta, out, true
    | _ -> failwith ("interp_builtin: " ^ name)
