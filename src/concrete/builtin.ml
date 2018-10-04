open Semantics__Buffers
open Semantics__Context

let interp_builtin : state -> string -> string list -> (state * stdout * bool) =
  fun sta name args ->
    match name with
    | "echo" ->
      let out =
        match args with
        | "-n" :: args ->
          let str = String.concat " " args in
          empty_stdout |> output str
        | _ ->
          let str = String.concat " " args in
          empty_stdout |> output str |> newline
      in
      sta, out, true
    | "true" ->
      sta, empty_stdout, true
    | "false" ->
      sta, empty_stdout, false
    | "grep" -> (* Just for testing stdin/stdout handling *)
      begin match args with
        | [word] ->
          let stdout =
            let re = Str.regexp_string word in
            let f stdout line =
              try
                ignore (Str.search_forward re line 0);
                stdout |> output line |> newline
              with Not_found ->
                stdout
            in
            List.fold_left f empty_stdout sta.stdin
          in
          let sta' = {sta with stdin=empty_stdin} in
          sta', stdout, stdout <> empty_stdout
        | _ ->
          let str = "grep: not exactly one argument" in
          let out = empty_stdout |> output str |> newline in
          sta, out, false
      end
    | _ ->
      let str = name^": command not found" in
      let out = empty_stdout |> output str |> newline in
      sta, out, false
