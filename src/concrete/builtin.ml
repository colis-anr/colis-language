open Semantics__Buffers
open Semantics__Context

let interp_builtin : state -> string -> string list -> (state * bool) =
  fun sta name args ->
    match name with
    | "echo" ->
      let stdout =
        match args with
        | "-n" :: args ->
          let str = String.concat " " args in
          sta.stdout |> output str
        | _ ->
          let str = String.concat " " args in
          sta.stdout |> output str |> newline
      in
      {sta with stdout}, true
    | "true" ->
      sta, true
    | "false" ->
      sta, false
    | "grep" -> (* Just for testing stdin/stdout handling *)
      begin match args with
        | [word] ->
          let stdout, result =
            let re = Str.regexp_string word in
            let f (stdout, res) line =
              try
                ignore (Str.search_forward re line 0);
                stdout |> output line |> newline, true
              with Not_found ->
                stdout, res
            in
            List.fold_left f (sta.stdout, false) sta.stdin
          in
          let sta' = {sta with stdout; stdin=empty_stdin} in
          sta', result
        | _ ->
          let str = "grep: not exactly one argument" in
          let stdout = sta.stdout |> output str |> newline in
          {sta with stdout}, false
      end
    | _ ->
      let str = name ^ ": command not found" in
      let stdout = sta.stdout |> output str |> newline in
      {sta with stdout}, false
