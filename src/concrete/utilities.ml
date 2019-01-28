(** Concrete interpretation of selected shell builtins.

    See subsections of http://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap04.html#tag_20
  *)

open Semantics__State
open Semantics__Buffers

let unknown_utility ?(msg="command not found") ~name sta =
  if !Options.fail_on_unknown_utilities then
    raise (Errors.UnsupportedUtility name)
  else
    let str = name ^ ": " ^ msg in
    let stdout = Stdout.(sta.stdout |> output str |> newline) in
    {sta with stdout}, false

let unknown_argument ?(msg="Unknown argument") ~name ~arg sta =
  if !Options.fail_on_unknown_utilities then
    raise (Errors.UnsupportedArgument (name, arg))
  else
    let str = name ^ ": " ^ msg ^ ": " ^ arg in
    let stdout = Stdout.(sta.stdout |> output str |> newline) in
    {sta with stdout}, false

let test (sta : state) : string list -> (state * bool) = function
  | [sa; "="; sb] ->
     (sta, sa = sb)
  | _ ->
     unknown_argument ~name:"test" ~arg:"" sta

let interp_utility : state -> string -> string list -> (state * bool) =
  fun sta name args ->
  match name with
  | "echo" ->
     let stdout =
       match args with
       | "-n" :: args ->
          let str = String.concat " " args in
          Stdout.(sta.stdout |> output str)
       | _ ->
          let str = String.concat " " args in
          Stdout.(sta.stdout |> output str |> newline)
     in
     {sta with stdout}, true
  | "true" ->
     sta, true
  | "false" ->
     sta, false
  | "test" -> test sta args
  | "grep" -> (* Just for testing stdin/stdout handling *)
     begin match args with
     | [word] ->
        let stdout, result =
          let re = Str.regexp_string word in
          let f (stdout, res) line =
            try
              ignore (Str.search_forward re line 0);
              Stdout.(stdout |> output line |> newline), true
            with Not_found ->
              stdout, res
          in
          List.fold_left f (sta.stdout, false) sta.stdin
        in
        let sta' = {sta with stdout; stdin=Stdin.empty} in
        sta', result
     | _ ->
        let str = "grep: not exactly one argument" in
        let stdout = Stdout.(sta.stdout |> output str |> newline) in
        {sta with stdout}, false
     end
  | _ -> unknown_utility ~name sta
